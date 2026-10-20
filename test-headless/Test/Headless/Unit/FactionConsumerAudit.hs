{-# LANGUAGE Strict #-}
-- | Who may still read faction through the LEGACY surfaces (#2518,
--   FTS-4 requirement 5), as a ratchet over the source tree rather than
--   a grep someone has to remember to run.
--
--   After FTS-4 no non-relation consumer reads 'Unit.Faction.Membership.
--   legacyFactionOf', and after FTS-5 (#2521) no relation consumer
--   either; nothing reads the scalar 'Unit.Faction.isPlayerOwned' /
--   'Unit.Faction.isPlayerCommandable', @unit.getFaction@, or the
--   string-taking @faction.*@ property verbs for ownership or
--   commandability. Each scan below pins the EXACT set of files allowed
--   to name its surface, so a reintroduced consumer fails here, and so
--   does an allowlisted file that stops needing the surface (shrink the
--   list when FTS-5/FTS-7 retire one).
--
--   Comments and string literals are stripped first: prose may name a
--   legacy function, and the registrar's verb-name strings are not
--   reads.
module Test.Headless.Unit.FactionConsumerAudit
    ( consumerAuditSpec
    , haskellCode
    , luaCode
    ) where

import UPrelude
import Test.Hspec
import Control.Monad (filterM)
import Data.Char (isAlphaNum)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

consumerAuditSpec ∷ Spec
consumerAuditSpec = describe "legacy faction consumers (FTS-4)" $ do
    it "only unit.getFaction, the save codec and the adapter itself read \
       \legacyFactionOf" $ do
        files ← haskellFilesNaming "legacyFactionOf"
        files `shouldBe`
            [ "src/Engine/Scripting/Lua/API/Units/Spawn.hs"  -- unit.getFaction
            , "src/Unit/Faction/Membership.hs"               -- its definition
            , "src/World/Save/UnitFaction.hs" ]              -- the save codec

    it "the scalar ownership and commandability predicates are read only \
       \by their definition and the string-taking faction.* verbs" $ do
        owned ← haskellFilesNaming "isPlayerOwned"
        commandable ← haskellFilesNaming "isPlayerCommandable"
        let allowed = [ "src/Engine/Scripting/Lua/API/Faction.hs"
                      , "src/Unit/Faction.hs" ]
        owned `shouldBe` allowed
        commandable `shouldBe` allowed

    it "no script calls a string-taking faction verb" $
        forM_ [ "faction.isPlayerOwned", "faction.isPlayerCommandable"
              , "faction.hasUnrestrictedCombat"
              -- #2521 (FTS-5): the relation verbs joined them.
              , "faction.relation", "faction.areAllies", "faction.canAttack" ]
          $ \verb → luaFilesNaming verb ⌦ (`shouldBe` [])

    -- #2521 (FTS-5) ported the four relation consumers this allowlisted,
    -- so it is EMPTY: any future script deciding policy from the legacy
    -- tag fails here.
    it "no script reads unit.getFaction" $
        luaFilesNaming "unit.getFaction" ⌦ (`shouldBe` [])

    describe "the scanner itself" $ do
        it "ignores comments and string literals, keeps code" $ do
            haskellCode "f = legacyFactionOf p -- legacyFactionOf\n"
                `shouldBe` "f = legacyFactionOf p \n"
            haskellCode "{- legacyFactionOf {- nested -} -} g \"isPlayerOwned\" x"
                `shouldBe` " g  x"
            luaCode "x = unit.getFaction(u) -- unit.getFaction\n"
                `shouldBe` "x = unit.getFaction(u) \n"
            luaCode "--[[ faction.isPlayerOwned ]] y = 'faction.isPlayerOwned'"
                `shouldBe` " y = "

        it "matches whole identifiers only" $ do
            names "isPlayerOwned" "isProfilePlayerOwned p" `shouldBe` False
            names "isPlayerOwned" "unit.isPlayerOwned(uid)" `shouldBe` True
            names "unit.getFaction" "unit.getFactionTags(u)" `shouldBe` False
            names "legacyFactionOf" "M.legacyFactionOf p" `shouldBe` True

-- * Scans

haskellFilesNaming ∷ Text → IO [FilePath]
haskellFilesNaming ident = do
    files ← (⧺) ⊚ sourcesUnder ".hs" "src" ⊛ sourcesUnder ".hs" "app"
    files `shouldSatisfy` ((> 100) ∘ length)
    filterM (\f → names ident ∘ haskellCode ⊚ TIO.readFile f) files

luaFilesNaming ∷ Text → IO [FilePath]
luaFilesNaming ident = do
    files ← sourcesUnder ".lua" "scripts"
    files `shouldSatisfy` ((> 50) ∘ length)
    filterM (\f → names ident ∘ luaCode ⊚ TIO.readFile f) files

-- | Does @ident@ occur in @code@ as a whole token? Dots inside @ident@
--   are literal; the characters on either side must not continue an
--   identifier (a leading qualifier dot is allowed, so @M.f@ names @f@).
names ∷ Text → Text → Bool
names ident code = any bounded (T.breakOnAll ident code)
  where
    bounded (before, rest) =
        let after = T.drop (T.length ident) rest
        in not (endsIdent before) ∧ not (startsIdent after)
    endsIdent t = maybe False (identChar ∘ snd) (T.unsnoc t)
    startsIdent t = maybe False (identChar ∘ fst) (T.uncons t)
    identChar c = isAlphaNum c ∨ c ≡ '_' ∨ c ≡ '\''

sourcesUnder ∷ String → FilePath → IO [FilePath]
sourcesUnder ext root = do
    present ← doesDirectoryExist root
    if not present then pure [] else go root
  where
    go dir = do
        entries ← L.sort ⊚ listDirectory dir
        fmap concat $ forM entries $ \e → do
            let path = dir </> e
            isDir ← doesDirectoryExist path
            if isDir then go path
                     else pure [ path | takeExtension path ≡ ext ]

-- * Comment and string stripping

-- | Haskell source with line comments, nested block comments and string
--   literals removed. Character literals are left alone except @'"'@,
--   the one a string scan would misread.
haskellCode ∷ Text → Text
haskellCode = T.pack ∘ go (0 ∷ Int) ∘ T.unpack
  where
    -- The one character literal a string scan would misread.
    go 0 ('\'' : '"' : '\'' : cs) = go 0 cs
    go 0 ('\'' : '\\' : '"' : '\'' : cs) = go 0 cs
    go 0 ('"' : cs) = go 0 (skipString cs)
    go depth ('{' : '-' : cs) = go (depth + 1) cs
    go depth ('-' : '}' : cs) | depth > 0 = go (depth - 1) cs
    go depth (_ : cs) | depth > 0 = go depth cs
    go _ ('-' : '-' : cs)
        | not (startsOperator cs) = go 0 (dropWhile (≢ '\n') cs)
    go _ (c : cs) = c : go 0 cs
    go _ [] = []
    -- @-->@ and friends are operators, not comments.
    startsOperator (c : _) = c `elem` ("!#$%&*+./<=>?@\\^|~:" ∷ String)
    startsOperator [] = False

-- | Lua source with @--@ and @--[[ ]]@ comments and quoted strings
--   removed.
luaCode ∷ Text → Text
luaCode = T.pack ∘ go ∘ T.unpack
  where
    go ('-' : '-' : '[' : '[' : cs) = go (dropBlock cs)
    go ('-' : '-' : cs) = go (dropWhile (≢ '\n') cs)
    go ('"' : cs) = go (skipQuoted '"' cs)
    go ('\'' : cs) = go (skipQuoted '\'' cs)
    go (c : cs) = c : go cs
    go [] = []
    dropBlock (']' : ']' : cs) = cs
    dropBlock (_ : cs) = dropBlock cs
    dropBlock [] = []

skipString ∷ String → String
skipString = skipQuoted '"'

-- | Drop through the closing @q@, honouring backslash escapes.
skipQuoted ∷ Char → String → String
skipQuoted q = go
  where
    go ('\\' : _ : cs) = go cs
    go (c : cs) | c ≡ q = cs
                | c ≡ '\n' = c : cs  -- unterminated: stop at the line
                | otherwise = go cs
    go [] = []
