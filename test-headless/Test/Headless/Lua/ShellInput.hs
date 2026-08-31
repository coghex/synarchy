-- | #1187: the debug console's input line (@scripts/shell.lua@) treats its
-- cursor and scroll offsets as Unicode CODE-POINT offsets into the input
-- buffer, the same contract the editable widgets got in #746. Before this,
-- every editing and navigation path stepped one BYTE, so a single accented
-- letter typed and then backspaced left half a character behind.
--
-- These drive the REAL shell module through its production event entry points
-- (@onCharInput@, @onTextBackspace@, @onTextDelete@, @onTabPressed@, the
-- cursor handlers, @onInterrupt@) on a headless backend, with the shell
-- actually SHOWN — otherwise @updateDisplay@ returns immediately and the
-- measuring paths (@updateInputScroll@, @updateCursorPos@, @getVisibleInput@,
-- @updateGhostText@) never run.
--
-- The stubs are the GPU asset loads, the window size, and the on-disk
-- arrow-key history file (user-owned, gitignored — a test must never read or
-- write it). @engine.getTextWidth@ is the one measuring stub: it charges a
-- fixed width per code point so scrolling is deterministic, and REFUSES any
-- argument that isn't valid UTF-8, which is what turns "never measures a
-- partial sequence" into a checked fact rather than a source grep.
module Test.Headless.Lua.ShellInput (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Namespaces
    (engineApiNamespaces, consoleExposedNamespaces, consoleWithheldNamespaces)
import Engine.Scripting.Lua.API.Shell (setupShellSandbox)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.ShellFocus (createFocusManager)
import UI.Types (emptyUIPageManager)

spec ∷ Spec
spec = do
    around withHeadlessEngine $ do
        describe "editing" $ do
            it "inserts and removes exactly one code point per keystroke" $ \env → do
                ls ← shellBackend env
                -- One-, two-, three-, and four-byte characters in one line.
                -- Pre-#1187 the cursor sat 4 BYTES into a 10-byte buffer and
                -- this backspace cut '界' in half.
                eval ls
                    "__boot(); local f=__fid; \
                    \for _,c in ipairs({'a','é','界','🙂'}) do __shell.onCharInput(f,c) end; \
                    \local t1,c1=__shell.getInputState(); \
                    \__shell.onTextBackspace(f); \
                    \local t2,c2=__shell.getInputState(); \
                    \return t1..'|'..c1..'|'..#t1..'|'..t2..'|'..c2 \
                    \..'|'..__shell.getVisibleInput()..'|'..tostring(utf8.len(t2)~=nil)"
                    `shouldReturn` "aé界🙂|4|10|aé界|3|aé界|true"

            it "routes the dispatched Delete callback and honours focus" $ \env → do
                ls ← shellBackend env
                -- Engine.Scripting.Lua.Thread.Dispatch broadcasts LuaTextDelete
                -- as "onTextDelete"; the shell only exported "onDelete", so the
                -- real Delete key never reached it at all.
                eval ls
                    "__boot(); local f=__fid; \
                    \for _,c in ipairs({'é','界','🙂'}) do __shell.onCharInput(f,c) end; \
                    \__shell.onCursorHome(f); __shell.onTextDelete(f); \
                    \local t1,c1=__shell.getInputState(); \
                    \__shell.onTextDelete(f+7); \
                    \local t2,c2=__shell.getInputState(); \
                    \__shell.onCursorEnd(f); __shell.onTextDelete(f); \
                    \local t3,c3=__shell.getInputState(); \
                    \return t1..'|'..c1..'|'..t2..'|'..c2..'|'..t3..'|'..c3"
                    `shouldReturn` "界🙂|0|界🙂|0|界🙂|2"

            it "moves the cursor between whole characters" $ \env → do
                ls ← shellBackend env
                eval ls
                    "__boot(); local f=__fid; \
                    \for _,c in ipairs({'é','界','🙂','a'}) do __shell.onCharInput(f,c) end; \
                    \__shell.onCursorHome(f); local _,c0=__shell.getInputState(); \
                    \__shell.onCursorRight(f); __shell.onCursorRight(f); \
                    \local _,c1=__shell.getInputState(); \
                    \__shell.onCharInput(f,'X'); \
                    \local t2,c2=__shell.getInputState(); \
                    \__shell.onCursorEnd(f); local _,c3=__shell.getInputState(); \
                    \__shell.onCursorRight(f); local _,c4=__shell.getInputState(); \
                    \__shell.onCursorLeft(f); __shell.onCursorLeft(f); \
                    \local _,c5=__shell.getInputState(); \
                    \return c0..'|'..c1..'|'..t2..'|'..c2..'|'..c3..'|'..c4..'|'..c5 \
                    \..'|'..tostring(utf8.len(t2)~=nil)"
                    `shouldReturn` "0|2|é界X🙂a|3|5|5|3|true"

        describe "completion" $ do
            it "inserts a multibyte completion by code points" $ \env → do
                ls ← shellBackend env
                eval ls
                    "__boot(); local f=__fid; shellSandbox['ww🙂ab']=1; shellSandbox['ww🙂cd']=1; \
                    \__shell.onCharInput(f,'w'); __shell.onCharInput(f,'w'); \
                    \__shell.onTabPressed(f); \
                    \local t,c=__shell.getInputState(); \
                    \return t..'|'..c..'|'..#t..'|'..tostring(utf8.len(t)~=nil)"
                    `shouldReturn` "ww🙂|3|6|true"

            it "never derives a partial code point as the common prefix" $ \env → do
                ls ← shellBackend env
                -- 🙂 (F0 9F 99 82) and 🙃 (F0 9F 99 83) agree on three of four
                -- bytes, so the byte-wise common prefix of these two names is
                -- invalid UTF-8 — and it used to become ghost text and a Tab
                -- insertion.
                eval ls
                    "__boot(); local f=__fid; shellSandbox['zz🙂x']=1; shellSandbox['zz🙃y']=1; \
                    \local lcp=__shell.longestCommonPrefix(__shell.getCompletions('zz')); \
                    \__shell.onCharInput(f,'z'); __shell.onCharInput(f,'z'); \
                    \__shell.onTabPressed(f); \
                    \local t,c=__shell.getInputState(); \
                    \return lcp..'|'..#lcp..'|'..tostring(utf8.len(lcp)~=nil)..'|'..t..'|'..c"
                    `shouldReturn` "zz|2|true|zz|2"

            it "measures whole characters for the ghost hint" $ \env → do
                ls ← shellBackend env
                -- The ghost path only runs with the cursor at the true END of
                -- the buffer, which is itself a code-point comparison: with a
                -- leading 'é' the byte-counting cursor sat one short of
                -- #inputBuffer and the hint silently never appeared.
                eval ls
                    "__boot(); local f=__fid; shellSandbox['gg界xy']=1; shellSandbox['gg界zw']=1; \
                    \__shell.onCharInput(f,'é'); __shell.onCharInput(f,'g'); \
                    \_G.__measured={}; __shell.onCharInput(f,'g'); \
                    \local ghost,buffer=false,false; \
                    \for _,s in ipairs(__measured) do \
                    \if s=='界' then ghost=true end; if s=='égg' then buffer=true end end; \
                    \return tostring(ghost)..'|'..tostring(buffer)"
                    `shouldReturn` "true|true"

            it "offers only names the console's own environment can resolve" $ \env → do
                ls ← shellBackend env
                -- _G and shellSandbox are DISTINCT objects at both levels:
                -- a top-level name, and a member of an engine API table the
                -- sandbox shallow-copied before scripts loaded. Only the
                -- sandbox side may be suggested, and what is suggested must
                -- resolve where engine.shellExecute actually runs it.
                eval ls
                    "__boot(); \
                    \_G['zqGlobalOnly']=1; shellSandbox['zqSandboxOnly']=1; \
                    \engine.zqGlobalMember=1; shellSandbox.engine.zqSandboxMember=1; \
                    \local top=table.concat(__shell.getCompletions('zq'),','); \
                    \local mem=table.concat(__shell.getCompletions('engine.zq'),','); \
                    \local r1=engine.shellExecute('return zqSandboxOnly ~= nil'); \
                    \local r2=engine.shellExecute('return engine.zqSandboxMember ~= nil'); \
                    \return top..'|'..mem..'|'..r1..'|'..r2"
                    `shouldReturn`
                        "zqSandboxOnly|engine.zqSandboxMember|true|true"

            it "resolves every candidate it offers in the execution environment" $ \env → do
                ls ← shellBackend env
                -- 'c' matches no Lua keyword and no copied stdlib name, and
                -- the visible history is empty, so every candidate for it
                -- comes from the environment source requirement 2 governs --
                -- and all five are engine API tables the sandbox used to
                -- omit. 'world.get' is the member half of the same check.
                eval ls
                    "__boot(); local bad,n={},0; \
                    \local function check(p) \
                    \  for _,c in ipairs(__shell.getCompletions(p)) do n=n+1; \
                    \    local r,e=engine.shellExecute('return ('..c..') ~= nil'); \
                    \    if e or r~='true' then bad[#bad+1]=c end end end; \
                    \check('c'); check('world.get'); \
                    \return table.concat(__shell.getCompletions('c'),',') \
                    \..'|'..table.concat(bad,',')..'|'..tostring(n>=10)"
                    `shouldReturn`
                        "camera,chop,combat,construction,craft||true"

            it "keeps keyword and visible-history candidates beside the \
               \environment source" $ \env → do
                ls ← shellBackend env
                -- Requirement 6: keywords and past commands are separate
                -- sources and are NOT required to resolve as sandbox keys.
                -- 'wh' reaches only the keyword; 'zzc' only the history.
                eval ls
                    "__boot(); \
                    \__shell.addHistory('zzcOne','OK',false); \
                    \__shell.addHistory('zzcTwo','OK',false); \
                    \return table.concat(__shell.getCompletions('wh'),',') \
                    \..'|'..table.concat(__shell.getCompletions('zzc'),',') \
                    \..'|'..table.concat(__shell.getCompletions('pr'),',')"
                    `shouldReturn` "while|zzcOne,zzcTwo|print"

        -- #1958: the console offered completions it could not execute. The
        -- sandbox's engine API list had drifted sixteen namespaces behind
        -- the setglobal calls it was asked to mirror by comment, and
        -- getCompletions enumerated _G on top of that. Both sides here are
        -- derived live -- the registered set by diffing _G across a real
        -- registerLuaAPI, the reachable set from the real shellSandbox --
        -- so neither this group nor production restates the other's list.
        describe "engine API namespace synchronisation" $ do
            it "exposes every registered engine API namespace to the console \
               \except the declared withheld ones" $ \env → do
                (ls, registered) ← apiRegistrationBackend env
                sandboxTables ← splitNames ⊚ eval ls
                    "local n={}; for k,v in pairs(shellSandbox) do \
                    \if type(k)=='string' and type(v)=='table' then n[#n+1]=k end \
                    \end; table.sort(n); return table.concat(n,',')"
                let declared  = sort (map TE.decodeUtf8 engineApiNamespaces)
                    exposed   = sort (map TE.decodeUtf8 consoleExposedNamespaces)
                    withheld  = sort (map (TE.decodeUtf8 . fst)
                                          consoleWithheldNamespaces)
                    -- Not every sandbox table is an API namespace: math,
                    -- string, table and os are deliberately there too.
                    projection = filter (`elem` registered) sandboxTables
                -- Both directions: a namespace registered but undeclared,
                -- and a declared name no longer registered, each fail here.
                registered `shouldBe` declared
                filter (`notElem` registered) withheld `shouldBe` []
                -- The contract itself: registered minus withheld, reachable.
                projection `shouldBe` filter (`notElem` withheld) registered
                projection `shouldBe` exposed

            it "withholds debug and keeps its escape primitives out of the \
               \console" $ \env → do
                (ls, registered) ← apiRegistrationBackend env
                -- debug is the augmented case: openlibs created the stdlib
                -- table and registerDebugAPI added engine verbs to it, so
                -- it never appears as a NEW global. The live derivation has
                -- to recognise it anyway, or the one withheld namespace
                -- would silently drop out of the comparison above.
                ("debug" `elem` registered) `shouldBe` True
                eval ls "return tostring(shellSandbox.debug)"
                    `shouldReturn` "nil"
                -- The reason it is withheld, checked rather than asserted in
                -- prose: the console cannot reach the upvalue primitive that
                -- would rewrite its own _ENV.
                eval ls "local r=engine.shellExecute('return debug'); return r"
                    `shouldReturn` "nil"
                eval ls "local _,e=engine.shellExecute('return debug.setupvalue'); \
                        \return tostring(e)"
                    `shouldReturn` "true"
                -- Withholding is a deliberate decision, so widening the set
                -- is meant to cost an edit here as well as in production.
                map (TE.decodeUtf8 . fst) consoleWithheldNamespaces
                    `shouldBe` ["debug"]

        describe "scrolling" $ do
            it "scrolls by code points and shows whole characters" $ \env → do
                ls ← shellBackend env
                -- 100px per code point against the shell's 892px input
                -- field: exactly eight characters fit, so a fifteen-character
                -- line must scroll seven of them away.
                --
                -- #1959 is why that field is 892px and not the 1100px this
                -- case measured against before, and the stub framebuffer
                -- below is unchanged — the width it implies is what moved.
                -- The console now fits its box to the framebuffer, and
                -- 1280px cannot hold the preferred 1368px-wide box, so the
                -- center narrows to 1112 (interior 104..1216); and the input
                -- budget is now the room actually left after the prompt
                -- rather than a flat 100px inset, so the 200px this stub
                -- charges for "$>" is what the field gives up.
                eval ls
                    "__boot(); local f=__fid; _G.__px=100; \
                    \for i=1,5 do for _,c in ipairs({'é','界','🙂'}) do \
                    \__shell.onCharInput(f,c) end end; \
                    \local t,c,s=__shell.getInputState(); \
                    \local vis=__shell.getVisibleInput(); \
                    \for i=1,12 do __shell.onCursorLeft(f) end; \
                    \local _,c2,s2=__shell.getInputState(); \
                    \local vis2=__shell.getVisibleInput(); \
                    \return utf8.len(t)..'|'..c..'|'..s..'|'..utf8.len(vis)..'|'..vis \
                    \..'|'..c2..'|'..s2..'|'..utf8.len(vis2)..'|'..vis2"
                    `shouldReturn`
                        "15|15|7|8|界🙂é界🙂é界🙂|3|3|8|é界🙂é界🙂é界"

        describe "history and interrupt" $ do
            it "restores stored Unicode commands with a code-point cursor" $ \env → do
                ls ← shellBackend env
                -- 'café🙂' is 5 code points in 8 bytes, '東京' is 2 in 6.
                eval ls
                    "__boot({'café🙂','東京'}); local f=__fid; \
                    \__shell.onCharInput(f,'x'); __shell.onCharInput(f,'🙂'); \
                    \__shell.onCursorUp(f); local t1,c1=__shell.getInputState(); \
                    \__shell.onCursorUp(f); local t2,c2=__shell.getInputState(); \
                    \__shell.onCursorDown(f); local t3,c3=__shell.getInputState(); \
                    \__shell.onCursorDown(f); local t4,c4=__shell.getInputState(); \
                    \__shell.onInterrupt(f); local t5,c5,s5=__shell.getInputState(); \
                    \return t1..'|'..c1..'|'..t2..'|'..c2..'|'..t3..'|'..c3 \
                    \..'|'..t4..'|'..c4..'|['..t5..']'..c5..'|'..s5"
                    `shouldReturn` "東京|2|café🙂|5|東京|2|x🙂|2|[]0|0"

            it "refuses a stored history line that is not valid UTF-8" $ \env → do
                ls ← shellBackend env
                -- config/shell_history.txt is plain text a user can hand-edit
                -- or truncate, and it is the one input-buffer ingress that
                -- isn't engine-delivered text. A broken line must not become an
                -- unslicable buffer.
                eval ls
                    "__boot({'ok','br\\xE9ken','東京'}); local f=__fid; \
                    \__shell.onCursorUp(f); local t1,c1=__shell.getInputState(); \
                    \__shell.onCursorUp(f); local t2,c2=__shell.getInputState(); \
                    \__shell.onCursorUp(f); local t3,c3=__shell.getInputState(); \
                    \return t1..'|'..c1..'|'..t2..'|'..c2..'|'..t3..'|'..c3"
                    `shouldReturn` "東京|2|ok|2|ok|2"

        -- "Unchanged" is #1187's contract: ASCII must behave exactly as it
        -- did before the byte -> code-point rewrite. The trailing scroll
        -- offset and visible slice moved with #1959's fitted input width
        -- (see the scrolling case above); every editing, navigation and
        -- completion expectation in the line is untouched.
        describe "ASCII behaviour is unchanged" $ do
            it "edits, navigates, completes, and scrolls exactly as before" $ \env → do
                ls ← shellBackend env
                eval ls
                    "__boot(); local f=__fid; shellSandbox['qqrs']=1; shellSandbox['qqrt']=1; \
                    \for _,c in ipairs({'a','b','c'}) do __shell.onCharInput(f,c) end; \
                    \__shell.onCursorLeft(f); __shell.onCharInput(f,'X'); \
                    \local t1,c1=__shell.getInputState(); \
                    \__shell.onTextBackspace(f); __shell.onTextDelete(f); \
                    \local t2,c2=__shell.getInputState(); \
                    \__shell.onCursorHome(f); local _,c3=__shell.getInputState(); \
                    \__shell.onCursorEnd(f); local _,c4=__shell.getInputState(); \
                    \__shell.onInterrupt(f); \
                    \__shell.onCharInput(f,'q'); __shell.onCharInput(f,'q'); \
                    \__shell.onTabPressed(f); \
                    \local t5,c5=__shell.getInputState(); \
                    \__shell.onInterrupt(f); _G.__px=100; \
                    \for i=1,15 do __shell.onCharInput(f,'a') end; \
                    \local _,c6,s6=__shell.getInputState(); \
                    \return t1..'|'..c1..'|'..t2..'|'..c2..'|'..c3..'|'..c4 \
                    \..'|'..t5..'|'..c5..'|'..c6..'|'..s6..'|'..__shell.getVisibleInput()"
                    `shouldReturn` "abXc|3|ab|2|0|2|qqr|3|15|7|aaaaaaaa"

    -- The byte-offset assumption was spread across every editing, navigation
    -- and measuring path in the file, so pin its absence rather than only its
    -- replacements.
    describe "input-editing call sites (#1187 audit)" $ do
        it "the debug console edits its buffer through the shared UTF-8 helper" $ do
            src ← TIO.readFile "scripts/shell.lua"
            T.isInfixOf "require(\"scripts.ui.utf8_safe\")" src `shouldBe` True
            -- No byte slice of, and no byte length of, the input buffer.
            T.isInfixOf "inputBuffer:sub" src `shouldBe` False
            T.isInfixOf "#inputBuffer" src `shouldBe` False
            -- The Delete key's dispatched name is wired up.
            T.isInfixOf "function shell.onTextDelete" src `shouldBe` True

-- | A headless Lua backend with the real shell module loaded and the stubs it
-- needs to run without a GPU, a window, or the user's history file. Booting is
-- left to @__boot@ so each case can seed its own arrow-key history first.
--
-- The @engine.getTextWidth@ stub lands on @_G.engine@, which is a different
-- table from the @shellSandbox.engine@ the sandbox copied — that is exactly
-- the divergence #1958 is about. It is the right side to stub: @shell.lua@
-- itself runs unsandboxed and calls the @_G@ copy.
shellBackend ∷ EngineEnv → IO LuaBackendState
shellBackend env = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (focusManagerRef env) createFocusManager
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    -- Production order (Engine.Scripting.Lua.Thread.luaStartup): register
    -- the API, build the sandbox, and only then load scripts. Since #1958
    -- the sandbox is also the console's completion source, so a fixture
    -- that skipped this step would test a console with no names at all.
    setupShellSandbox (lbsLuaState ls)
    setup ← eval ls $ T.unwords
        [ "_G.__measured={}; _G.__px=10;"
        , "engine.getTextWidth=function(_,t,_) local n,b=utf8.len(t);"
        , "assert(n,'invalid UTF-8 measured at byte '..tostring(b));"
        , "__measured[#__measured+1]=t; return n*__px end;"
        , "engine.loadTexture=function() return 1 end;"
        , "engine.loadFont=function() return 1 end;"
        , "engine.getUIScale=function() return 1 end;"
        , "engine.getFramebufferSize=function() return 1280,720 end;"
        -- config/shell_history.txt is the user's own gitignored file: stub the
        -- io boundary so the arrow-key history is a fixture and nothing is ever
        -- written, while the real load/dedup/navigation code still runs.
        , "_G.__historySeed={}; _G.__historyWrites={};"
        , "local realOpen=io.open;"
        , "io.open=function(path,mode)"
        , "  if path=='config/shell_history.txt' then"
        , "    if mode=='w' then return {write=function(_,s)"
        , "      __historyWrites[#__historyWrites+1]=s end, close=function() end} end;"
        , "    local i=0;"
        , "    return {lines=function() return function() i=i+1;"
        , "      return __historySeed[i] end end, close=function() end}"
        , "  end;"
        , "  return realOpen(path,mode) end;"
        , "_G.__shell=require('scripts.shell');"
        , "_G.__boot=function(seed) _G.__historySeed=seed or {};"
        , "  __shell.init(1); __shell.show(); _G.__fid=__shell.getFocusId() end;"
        , "return 'ready'"
        ]
    setup `shouldBe` "ready"
    pure ls

-- | A backend that answers "which globals did 'registerLuaAPI' actually
-- install?" from the live Lua state rather than from a list, then builds the
-- real console sandbox on top. The registered set is the difference @_G@ took
-- across the call: a name that appeared, a name whose value was replaced, or
-- — the @debug@ case — a table @Lua.openlibs@ had already created that gained
-- members. @__apiSnap@ is this fixture's only addition to @_G@ and both
-- passes skip it by name; it is scaffolding, not a namespace.
--
-- No shell module here: this pair of cases is about registration and sandbox
-- construction, which happen before any script loads.
apiRegistrationBackend ∷ EngineEnv → IO (LuaBackendState, [T.Text])
apiRegistrationBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    snapped ← eval ls $ T.unwords
        [ "_G.__apiSnap={values={},keys={}};"
        , "for k,v in pairs(_G) do"
        , "  if type(k)=='string' and k~='__apiSnap' then"
        , "    __apiSnap.values[k]=v;"
        , "    if type(v)=='table' then local ks={};"
        , "      for mk in pairs(v) do ks[mk]=true end;"
        , "      __apiSnap.keys[k]=ks end"
        , "  end end;"
        , "return 'snapped'"
        ]
    snapped `shouldBe` "snapped"
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    registered ← splitNames ⊚ eval ls (T.unwords
        [ "local n={};"
        , "for k,v in pairs(_G) do"
        , "  if type(k)=='string' and k~='__apiSnap' then"
        , "    local pv=__apiSnap.values[k];"
        , "    local isNew=(pv==nil) or (pv~=v);"
        -- v~=_G skips the _G._G self-alias: the table being diffed
        -- gained all 26 new globals as members, so it would otherwise
        -- report itself as an augmented namespace.
        , "    if not isNew and type(v)=='table' and v~=_G then"
        , "      local pk=__apiSnap.keys[k] or {};"
        , "      for mk in pairs(v) do"
        , "        if pk[mk]==nil then isNew=true; break end end"
        , "    end;"
        , "    if isNew then n[#n+1]=k end"
        , "  end end;"
        , "table.sort(n); return table.concat(n,',')"
        ])
    setupShellSandbox (lbsLuaState ls)
    pure (ls, registered)

-- | The comma-joined name lists the Lua fixtures above return. An empty
-- answer is an empty list, not a one-element list holding "".
splitNames ∷ T.Text → [T.Text]
splitNames = filter (not ∘ T.null) ∘ T.splitOn ","

eval ∷ LuaBackendState → T.Text → IO T.Text
eval ls code = do
    out ← executeDebugLua (lbsLuaState ls) code
    -- executeDebugLua JSON-serializes string return values; these fixtures
    -- avoid quotes and backslashes, so stripping the outer pair is enough.
    pure $ if T.length out ≥ 2 ∧ T.head out ≡ '"' ∧ T.last out ≡ '"'
        then T.dropEnd 1 (T.drop 1 out)
        else out
