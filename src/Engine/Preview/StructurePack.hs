-- | The @--preview structures/\<name\>@ PACK viewer's pre-boot half
--   (#2495, BDA-17): decode @data\/structure_packs\/\<name\>.yaml@, enumerate
--   every appearance it declares, and judge every path it names — all
--   BEFORE @App.Preview.runPreview@ creates a window.
--
--   Authority. The pack YAML is the appearance authority, not the asset
--   tree: @dungeon_1@'s art lives under @assets\/textures\/buildings\/@
--   and its palette paths are persisted ('Structure.Palette',
--   @sdTexPalette@), so the viewer follows the YAML wherever it points
--   rather than asking the art to move. The game reads the same file
--   through @scripts\/structures.lua@ and @scripts\/wire.lua@; this module
--   restates only their RESOLUTION rules (variant inheritance of texture
--   and facemaps, never-inherited lifecycle sequences, the scalar-or-table
--   connection form), each noted where it is applied.
--
--   Resolution order (#2495 requirement 1 and its review amendment):
--
--   1. The item name gets the SAME single-component validation every
--      grouped category applies ('Engine.Preview.Discovery.resolveItemDir')
--      before any path is built from it, so a traversal shape is rejected
--      with the identical message whether or not a pack could exist.
--   2. Only an ABSENT manifest falls back to the folder browser. A
--      symlink (dangling or not), a directory, any other non-regular file,
--      an unreadable file and a malformed one are all pre-boot
--      diagnostics naming the manifest.
--
--   Declaration order. YAML mappings decode to key-ordered maps, which
--   would silently re-sort a pack's pieces, variants and connections; the
--   order is instead read from the libyaml event stream of the SAME bytes
--   ('keyOrders') and applied explicitly.
module Engine.Preview.StructurePack
  ( -- * Errors
    StructurePackError(..)
  , structurePackErrorMessage
    -- * Decoding
  , DeclLifecycle(..)
  , DeclFacemap(..)
  , DeclAppearance(..)
  , decodeStructurePack
    -- * Resolution
  , structurePacksRoot
  , structureTextureRoot
  , structurePreviewDefaultFps
  , wallCaps
  , resolveStructurePack
  , loadStructurePack
  , loadStructurePackFrom
    -- * Playback
  , lifecycleFrameIndexAt
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Text.Libyaml as LibYaml
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception (IOException, SomeException, try)
import System.Posix.Files
    ( FileStatus, getSymbolicLinkStatus, isDirectory, isRegularFile
    , isSymbolicLink )
import System.FilePath ((</>), (<.>), isAbsolute, splitDirectories
                       , pathSeparator)
import Engine.Core.Types
    ( PreviewStructPath(..), PreviewStructFacemap(..)
    , PreviewStructLifecycle(..), PreviewStructAppearance(..)
    , PreviewStructurePack(..) )
import Engine.Preview.BuildingMatrix
    ( CellStatus(..), classifyDeclaredPath, missingReasonKey
    , previewFrameIndexAt )
import Engine.Preview.Discovery (ItemDirError(..), itemDirErrorMessage)

-- * Errors

-- | Every pre-boot refusal of a structures PACK target. An absent
--   manifest is not one of them: it is the folder browser's case.
data StructurePackError
  = PackNameInvalid
    -- ^ The item is not a single path component — rejected with the
    --   folder browser's own 'ItemDirEscapesRoot' wording, so a pack
    --   viewer never changes what an unsafe name reports.
  | PackManifestSymlink !FilePath
  | PackManifestNotAFile !FilePath
    -- ^ A directory, FIFO, socket or device where the manifest belongs.
  | PackManifestUnreadable !FilePath !Text
  | PackManifestMalformed !FilePath !Text
  deriving (Eq, Show)

structurePackErrorMessage ∷ StructurePackError → Text
structurePackErrorMessage PackNameInvalid =
    itemDirErrorMessage ItemDirEscapesRoot
structurePackErrorMessage (PackManifestSymlink p) =
    T.pack p <> ": structure pack manifest must not be a symlink"
structurePackErrorMessage (PackManifestNotAFile p) =
    T.pack p <> ": structure pack manifest is not a regular file"
structurePackErrorMessage (PackManifestUnreadable p why) =
    T.pack p <> ": structure pack manifest is unreadable: " <> why
structurePackErrorMessage (PackManifestMalformed p why) =
    T.pack p <> ": malformed structure pack: " <> why

-- * Decoded declarations

-- | One lifecycle sequence exactly as the pack declares it, before any
--   path is judged. 'DeclSequence' carries the authored fps for a
--   destruction clip (#2491) and 'Nothing' for a construction sequence,
--   which gameplay indexes by build progress rather than a clock (#2488).
data DeclLifecycle
  = DeclUndeclared
  | DeclSequence ![Text] !(Maybe Double)
  deriving (Eq, Show)

-- | One facemap an appearance lights with, after inheritance: the cap
--   (walls only), the declared path ('Nothing' when no path is declared
--   for that cap), and whether a variant inherited it.
data DeclFacemap = DeclFacemap
  { dfCap       ∷ !(Maybe Text)
  , dfPath      ∷ !(Maybe Text)
  , dfInherited ∷ !Bool
  } deriving (Eq, Show)

data DeclAppearance = DeclAppearance
  { daKind         ∷ !Text
  , daEdge         ∷ !(Maybe Text)
  , daConnection   ∷ !(Maybe Text)
  , daVariant      ∷ !Text
  , daTexture      ∷ !Text
  , daTextureInherited ∷ !Bool
  , daFacemaps     ∷ ![DeclFacemap]
  , daConstruction ∷ !DeclLifecycle
  , daDestruction  ∷ !DeclLifecycle
  } deriving (Eq, Show)

-- | The four wall cap facemap variants, in the order the shipped pack
--   declares them and the viewer offers them (#1712).
wallCaps ∷ [Text]
wallCaps = ["00", "10", "01", "11"]

wallEdges ∷ [Text]
wallEdges = ["ne", "nw", "se", "sw"]

defaultVariant ∷ Text
defaultVariant = "default"

-- * Declaration order

-- | Mapping-path → keys in the order the document declares them. A path
--   is the list of mapping keys from the document root (a sequence
--   element contributes @#\<index\>@, which no mapping key here uses).
type Orders = Map.Map [Text] [Text]

-- | Walk a libyaml event stream and record every mapping's key order.
--   Total: an event shape this walk does not model (a complex key) ends
--   the walk, and 'orderedPairs' then falls back to key order for
--   whatever it did not record — never a failure, since 'Yaml' already
--   decided whether the document is well formed.
keyOrders ∷ [LibYaml.Event] → Orders
keyOrders = fst ∘ node [] ∘ dropWhile preamble
  where
    preamble LibYaml.EventStreamStart   = True
    preamble LibYaml.EventDocumentStart = True
    preamble _                          = False

    node _ [] = (Map.empty, [])
    node path (e : es) = case e of
        LibYaml.EventMappingStart {}  → mapping path [] Map.empty es
        LibYaml.EventSequenceStart {} → sequence path (0 ∷ Int) Map.empty es
        _                             → (Map.empty, es)

    mapping path keys acc = \case
        LibYaml.EventMappingEnd : es →
            (Map.insert path (reverse keys) acc, es)
        LibYaml.EventScalar bytes _ _ _ : es →
            let k = TE.decodeUtf8Lenient bytes
                (inner, rest) = node (path ⧺ [k]) es
            in mapping path (k : keys) (Map.union acc inner) rest
        _ → (Map.insert path (reverse keys) acc, [])

    sequence path i acc = \case
        LibYaml.EventSequenceEnd : es → (acc, es)
        [] → (acc, [])
        es → let (inner, rest) = node (path ⧺ ["#" <> tshow i]) es
             in sequence path (i + 1) (Map.union acc inner) rest

-- | An object's pairs in DECLARED order: every key the document order
--   recorded, then any it did not (a merge key's contributions, say) in
--   ascending key order.
orderedPairs ∷ Orders → [Text] → Aeson.Object → [(Text, Aeson.Value)]
orderedPairs orders path o = declared ⧺ rest
  where
    pairs    = [ (Key.toText k, v) | (k, v) ← KM.toAscList o ]
    order    = Map.findWithDefault [] path orders
    declared = [ (k, v) | k ← dedup order, Just v ← [lookup k pairs] ]
    rest     = [ p | p@(k, _) ← pairs, k `notElem` order ]
    dedup    = foldr (\k acc → k : filter (≢ k) acc) []

-- * Decoding

type P = Either Text

-- | Decode a pack manifest's bytes into its appearances, in the grouped
--   order requirement 4 lists them: every piece kind (its default, then
--   each variant overriding it, in variant declaration order), then
--   every wall edge likewise, then Wire's connections.
--
--   The error names the offending location; 'loadStructurePackFrom'
--   prefixes the manifest path.
decodeStructurePack ∷ BS.ByteString → IO (Either Text [DeclAppearance])
decodeStructurePack bytes = case Yaml.decodeEither' bytes of
    Left err → pure (Left (T.pack (Yaml.prettyPrintParseException err)))
    Right value → do
        events ← try (runResourceT (runConduit
                         (LibYaml.decode bytes .| CL.consume)))
        let orders = either (\(_ ∷ SomeException) → Map.empty) keyOrders events
        pure (decodeValue orders value)

decodeValue ∷ Orders → Aeson.Value → P [DeclAppearance]
decodeValue orders = \case
    Aeson.Object top → do
        pieces   ← optObject top "pieces"
        walls    ← optObject top "walls"
        variants ← optObject top "variants"
        conns    ← optObject top "connections"
        let pieceList = maybe [] (orderedPairs orders ["pieces"]) pieces
            wallList  = maybe [] (orderedPairs orders ["walls"]) walls
            varList   = maybe [] (orderedPairs orders ["variants"]) variants
        forM_ wallList $ \(e, _) →
            unless (e `elem` wallEdges) $
                Left ("walls: unknown wall edge `" <> e
                      <> "` (expected one of " <> T.intercalate ", " wallEdges
                      <> ")")
        basePieces ← forM pieceList $ \(k, v) → (k,) ⊚ basePiece k v
        baseWalls  ← forM wallList $ \(e, v) → (e,) ⊚ baseWall e v
        overrides  ← forM varList $ \(name, v) →
            (name,) ⊚ variantDecl orders basePieces baseWalls name v
        wire ← case conns of
            Nothing → pure []
            Just o  → wireAppearances orders top o
        let pieceApps =
                concat [ base : [ a | (_, (ps, _)) ← overrides
                                    , Just a ← [lookup k ps] ]
                       | (k, base) ← basePieces ]
            wallApps =
                concat [ base : [ a | (_, (_, ws)) ← overrides
                                    , Just a ← [lookup e ws] ]
                       | (e, base) ← baseWalls ]
            apps = pieceApps ⧺ wallApps ⧺ wire
        when (null apps) $
            Left "the pack declares no appearances (no pieces, walls or \
                 \connections)"
        pure apps
    _ → Left "the document is not a mapping"

optObject ∷ Aeson.Object → Text → P (Maybe Aeson.Object)
optObject o k = case KM.lookup (Key.fromText k) o of
    Nothing          → pure Nothing
    Just Aeson.Null  → pure Nothing
    Just (Aeson.Object x) → pure (Just x)
    Just _ → Left (k <> ": expected a mapping")

field ∷ Aeson.Object → Text → Maybe Aeson.Value
field o k = case KM.lookup (Key.fromText k) o of
    Just Aeson.Null → Nothing
    other           → other

asObject ∷ Text → Aeson.Value → P Aeson.Object
asObject _ (Aeson.Object o) = pure o
asObject ctx _ = Left (ctx <> ": expected a mapping")

reqPath ∷ Text → Aeson.Object → Text → P Text
reqPath ctx o k = case field o k of
    Just v  → pathValue (ctx <> "." <> k) v
    Nothing → Left (ctx <> ": missing required `" <> k <> "` path")

optPath ∷ Text → Aeson.Object → Text → P (Maybe Text)
optPath ctx o k = traverse (pathValue (ctx <> "." <> k)) (field o k)

pathValue ∷ Text → Aeson.Value → P Text
pathValue _ (Aeson.String t) | not (T.null t) = pure t
pathValue ctx _ = Left (ctx <> ": expected a texture path")

-- | #2488's @construction:@ and #2491's @destruction:@ declarations, read
--   with the engine's own refusals: an empty or sparse list, a non-path
--   entry, and a destruction clip whose @fps@ is absent, non-numeric,
--   non-finite or not positive are all decode faults, because the game
--   refuses exactly those packs (@Structure.ArtCatalog.registerPackArt@).
--   An absent or null key is UNDECLARED, the state every shipped
--   appearance is in.
lifecycles ∷ Text → Aeson.Object → P (DeclLifecycle, DeclLifecycle)
lifecycles ctx o = (,) ⊚ construction ⊛ destruction
  where
    construction = case field o "construction" of
        Nothing → pure DeclUndeclared
        Just v  → (`DeclSequence` Nothing) ⊚ frameList (ctx <> ".construction") v
    destruction = case field o "destruction" of
        Nothing → pure DeclUndeclared
        Just (Aeson.Object d) → do
            let dctx = ctx <> ".destruction"
            fps ← case field d "fps" of
                Just n@(Aeson.Number _)
                  | Aeson.Success (f ∷ Double) ← Aeson.fromJSON n →
                    if isNaN f ∨ isInfinite f ∨ f ≤ 0
                        then Left (dctx <> ".fps: must be a finite positive \
                                          \number, got " <> tshow f)
                        else pure f
                Just _  → Left (dctx <> ".fps: must be a number")
                Nothing → Left (dctx <> ": missing required `fps`")
            frames ← case field d "frames" of
                Just v  → frameList (dctx <> ".frames") v
                Nothing → Left (dctx <> ": missing required `frames` list")
            pure (DeclSequence frames (Just fps))
        Just _ → Left (ctx <> ".destruction: expected a mapping with `fps` \
                              \and `frames`")

frameList ∷ Text → Aeson.Value → P [Text]
frameList ctx (Aeson.Array xs)
    | V.null xs = Left (ctx <> ": the frame list is empty")
    | otherwise = forM (zip [0 ∷ Int ..] (V.toList xs)) $ \(i, v) →
        pathValue (ctx <> "[" <> tshow i <> "]") v
frameList ctx _ = Left (ctx <> ": expected a list of texture paths")

basePiece ∷ Text → Aeson.Value → P DeclAppearance
basePiece kind v = do
    let ctx = "pieces." <> kind
    o ← asObject ctx v
    tex ← reqPath ctx o "texture"
    face ← reqPath ctx o "facemap"
    (c, d) ← lifecycles ctx o
    pure DeclAppearance
        { daKind = kind, daEdge = Nothing, daConnection = Nothing
        , daVariant = defaultVariant, daTexture = tex
        , daTextureInherited = False
        , daFacemaps = [DeclFacemap Nothing (Just face) False]
        , daConstruction = c, daDestruction = d }

-- | A wall edge: one texture and its cap facemaps. A cap the edge does
--   not declare is reported as such rather than rejected — the game only
--   warns about a short wall family (#1712) — while a cap outside the
--   four known ones is a decode fault.
baseWall ∷ Text → Aeson.Value → P DeclAppearance
baseWall edge v = do
    let ctx = "walls." <> edge
    o ← asObject ctx v
    tex ← reqPath ctx o "texture"
    caps ← capMap ctx o ⌦ maybe
        (Left (ctx <> ": missing required `facemaps` mapping")) pure
    (c, d) ← lifecycles ctx o
    pure DeclAppearance
        { daKind = "wall", daEdge = Just edge, daConnection = Nothing
        , daVariant = defaultVariant, daTexture = tex
        , daTextureInherited = False
        , daFacemaps = [ DeclFacemap (Just cap) (lookup cap caps) False
                       | cap ← wallCaps ]
        , daConstruction = c, daDestruction = d }

capMap ∷ Text → Aeson.Object → P (Maybe [(Text, Text)])
capMap ctx o = case field o "facemaps" of
    Nothing → pure Nothing
    Just (Aeson.Object m) → fmap Just $
        forM (KM.toAscList m) $ \(k, v) → do
            let cap = Key.toText k
            unless (cap `elem` wallCaps) $
                Left (ctx <> ".facemaps: unknown cap `" <> cap
                      <> "` (expected one of "
                      <> T.intercalate ", " wallCaps <> ")")
            (cap,) ⊚ pathValue (ctx <> ".facemaps." <> cap) v
    Just _ → Left (ctx <> ".facemaps: expected a mapping of cap to path")

-- | One @variants.\<name\>@ block: the pieces and walls it overrides,
--   resolved against the default exactly as @scripts\/structures.lua@
--   does — texture and facemaps fall back to the default's, while a
--   construction or destruction sequence is the variant's OWN or none
--   ('scripts\/structure_frames.lua' @declaredBy@: a damaged wall must
--   never be built out of the intact wall's frames). An override naming
--   a piece or edge the default never declares has nothing to inherit
--   from and is a decode fault.
variantDecl
    ∷ Orders → [(Text, DeclAppearance)] → [(Text, DeclAppearance)]
    → Text → Aeson.Value
    → P ([(Text, DeclAppearance)], [(Text, DeclAppearance)])
variantDecl orders basePieces baseWalls name v = do
    let ctx = "variants." <> name
    o ← asObject ctx v
    ps ← optObject o "pieces"
    ws ← optObject o "walls"
    pieces ← forM (maybe [] (orderedPairs orders ["variants", name, "pieces"]) ps) $
        \(k, pv) → do
            base ← maybe (Left (ctx <> ".pieces." <> k <> ": overrides a piece \
                                \the pack's `pieces` does not declare"))
                         pure (lookup k basePieces)
            let pctx = ctx <> ".pieces." <> k
            po ← asObject pctx pv
            tex ← optPath pctx po "texture"
            face ← optPath pctx po "facemap"
            (c, d) ← lifecycles pctx po
            let inherited = listToMaybe (daFacemaps base)
            pure (k, base
                { daVariant = name
                , daTexture = fromMaybe (daTexture base) tex
                , daTextureInherited = isNothing tex
                , daFacemaps = case face of
                    Just f  → [DeclFacemap Nothing (Just f) False]
                    Nothing → [ fm { dfInherited = True }
                              | fm ← maybeToList inherited ]
                , daConstruction = c, daDestruction = d })
    walls ← forM (maybe [] (orderedPairs orders ["variants", name, "walls"]) ws) $
        \(e, wv) → do
            base ← maybe (Left (ctx <> ".walls." <> e <> ": overrides a wall \
                                \edge the pack's `walls` does not declare"))
                         pure (lookup e baseWalls)
            let wctx = ctx <> ".walls." <> e
            wo ← asObject wctx wv
            tex ← optPath wctx wo "texture"
            caps ← fromMaybe [] ⊚ capMap wctx wo
            (c, d) ← lifecycles wctx wo
            pure (e, base
                { daVariant = name
                , daTexture = fromMaybe (daTexture base) tex
                , daTextureInherited = isNothing tex
                , daFacemaps =
                    [ case lookup cap caps of
                        Just p  → DeclFacemap (Just cap) (Just p) False
                        Nothing → fm { dfInherited = isJust (dfPath fm) }
                    | fm ← daFacemaps base
                    , let cap = fromMaybe "" (dfCap fm) ]
                , daConstruction = c, daDestruction = d })
    pure (pieces, walls)

-- | Wire's 16 autotile connections (@scripts\/wire.lua@
--   @connectionEntry@): each is a bare texture path or a table with a
--   @texture@ and optional lifecycle sequences, all sharing the pack's
--   one top-level @facemap@.
wireAppearances ∷ Orders → Aeson.Object → Aeson.Object → P [DeclAppearance]
wireAppearances orders top conns = do
    let pairs = orderedPairs orders ["connections"] conns
    if null pairs then pure [] else do
        face ← reqPath "pack" top "facemap"
        forM pairs $ \(name, v) → do
            let ctx = "connections." <> name
            (tex, (c, d)) ← case v of
                Aeson.String _ → (, (DeclUndeclared, DeclUndeclared))
                                   ⊚ pathValue ctx v
                Aeson.Object o → (,) ⊚ reqPath ctx o "texture" ⊛ lifecycles ctx o
                _ → Left (ctx <> ": expected a texture path or a mapping \
                                 \with a `texture` path")
            pure DeclAppearance
                { daKind = "wire", daEdge = Nothing, daConnection = Just name
                , daVariant = defaultVariant, daTexture = tex
                , daTextureInherited = False
                , daFacemaps = [DeclFacemap Nothing (Just face) False]
                , daConstruction = c, daDestruction = d }

-- * Resolution

-- | @data\/structure_packs@ — where a pack manifest lives.
structurePacksRoot ∷ FilePath
structurePacksRoot = "data" </> "structure_packs"

-- | @assets\/textures@ — the containment root every declared pack path
--   is judged against. A pack legitimately points at more than one
--   category (@dungeon_1@ uses @buildings\/@ and @facemap\/@), so the
--   boundary is the texture tree itself rather than one category.
structureTextureRoot ∷ FilePath
structureTextureRoot = "assets" </> "textures"

-- | The preview's inspection rate for a sequence that authors none — a
--   construction sequence, which gameplay indexes by build PROGRESS
--   rather than a clock. The same value the buildings viewer defaults
--   to ('Engine.Preview.Building.buildingDefaultFps'); it changes no
--   gameplay timing.
structurePreviewDefaultFps ∷ Float
structurePreviewDefaultFps = 8.0

-- | Judge every declared path under @texRoot@ and build the viewer's
--   payload.
resolveStructurePack
    ∷ FilePath → Text → Text → [DeclAppearance] → IO PreviewStructurePack
resolveStructurePack texRoot name manifest decls = do
    apps ← forM decls resolveOne
    pure PreviewStructurePack
        { pspkName        = name
        , pspkManifest    = manifest
        , pspkAppearances = apps
        , pspkDefault     = maybe "" psaIdentity (listToMaybe apps)
        }
  where
    judge path = do
        st ← classifyDeclaredPath texRoot path
        pure PreviewStructPath
            { pstPath = path
            , pstMissing = st ≢ CellLoadable
            , pstReason = missingReasonKey st }

    resolveOne da = do
        static ← judge (daTexture da)
        faces ← forM (daFacemaps da) $ \fm → do
            file ← traverse judge (dfPath fm)
            pure PreviewStructFacemap
                { psfCap = dfCap fm, psfFile = file
                , psfInherited = dfInherited fm }
        cons ← sequenceOf "construction" (daConstruction da)
        dest ← sequenceOf "destruction" (daDestruction da)
        let staticL = PreviewStructLifecycle
                { pslName = "static", pslDeclared = True, pslFrames = [static]
                , pslFps = 0, pslFpsSource = "static"
                , pslAlphaPolicy = "facemap-alpha" }
        pure PreviewStructAppearance
            { psaIdentity   = identityOf da
            , psaLabel      = labelOf da
            , psaGroup      = groupOf da
            , psaKind       = daKind da
            , psaEdge       = daEdge da
            , psaConnection = daConnection da
            , psaVariant    = daVariant da
            , psaTextureInherited = daTextureInherited da
            , psaFacemaps   = faces
            , psaLifecycles = [staticL, cons, dest]
            }

    sequenceOf lname = \case
        DeclUndeclared → pure PreviewStructLifecycle
            { pslName = lname, pslDeclared = False, pslFrames = []
            , pslFps = 0, pslFpsSource = "undeclared"
            , pslAlphaPolicy = "frame-alpha" }
        DeclSequence paths mfps → do
            frames ← forM paths judge
            pure PreviewStructLifecycle
                { pslName = lname, pslDeclared = True, pslFrames = frames
                , pslFps = maybe structurePreviewDefaultFps realToFrac mfps
                , pslFpsSource = maybe "preview-default" (const "authored") mfps
                , pslAlphaPolicy = "frame-alpha" }

    groupOf da = case (daEdge da, daConnection da) of
        (Just e, _) → "wall " <> e
        (_, Just _) → "wire"
        _           → daKind da
    identityOf da =
        daKind da
          <> maybe "" (":" <>) (daEdge da)
          <> maybe "" (":" <>) (daConnection da)
          <> "@" <> daVariant da
    labelOf da = case daConnection da of
        Just c  → "wire / " <> c
        Nothing → groupOf da <> " / " <> daVariant da

-- | The whole pre-boot pipeline for @--preview structures/\<name\>@,
--   against the shipped roots.
loadStructurePack
    ∷ String → IO (Either StructurePackError (Maybe PreviewStructurePack))
loadStructurePack = loadStructurePackFrom structurePacksRoot structureTextureRoot

-- | 'loadStructurePack' with explicit roots, for fixtures.
--
--   'Right' 'Nothing' means the manifest is ABSENT and the target falls
--   back to the folder browser unchanged; every other outcome is either
--   a pack or a pre-boot diagnostic.
loadStructurePackFrom
    ∷ FilePath → FilePath → String
    → IO (Either StructurePackError (Maybe PreviewStructurePack))
loadStructurePackFrom packsRoot texRoot item
    | null item ∨ isAbsolute item ∨ length (splitDirectories item) ≢ 1
        ∨ item ≡ "." ∨ item ≡ ".." ∨ pathSeparator `elem` item ∨ '/' `elem` item =
        pure (Left PackNameInvalid)
    | otherwise = do
        let manifest = packsRoot </> item <.> "yaml"
        mst ← (try (getSymbolicLinkStatus manifest)
                  ∷ IO (Either IOException FileStatus))
        case mst of
            Left _ → pure (Right Nothing)
            Right st
                | isSymbolicLink st → pure (Left (PackManifestSymlink manifest))
                | isDirectory st ∨ not (isRegularFile st) →
                    pure (Left (PackManifestNotAFile manifest))
                | otherwise → do
                    rb ← try (BS.readFile manifest)
                    case rb of
                        Left (e ∷ IOException) →
                            pure (Left (PackManifestUnreadable manifest
                                            (tshow e)))
                        Right bytes → decodeStructurePack bytes ⌦ \case
                            Left why →
                                pure (Left (PackManifestMalformed manifest why))
                            Right decls → Right ∘ Just ⊚
                                resolveStructurePack texRoot (T.pack item)
                                    (T.pack manifest) decls

-- * Playback

-- | The frame a lifecycle shows at @elapsed@ seconds after it was
--   selected: 'previewFrameIndexAt''s forced replay (#1833) at the
--   lifecycle's effective rate. A missing frame keeps its position, so
--   the count is always the DECLARED one. Static and undeclared
--   lifecycles never advance.
lifecycleFrameIndexAt ∷ PreviewStructLifecycle → Double → Int
lifecycleFrameIndexAt l elapsed
    | not (pslDeclared l) ∨ pslFps l ≤ 0 = 0
    | otherwise = previewFrameIndexAt (pslFps l) (length (pslFrames l)) elapsed
