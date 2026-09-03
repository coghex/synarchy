-- | A YAML number that remembers how the document spelled it (#2288).
--
--   Two config loaders need the same thing from a numeric leaf: the
--   value, so a domain check can judge it, and the SOURCE TEXT, so the
--   warning about a rejected leaf quotes what the file actually said
--   rather than the infinity the number narrowed to.
--
--   They also need the same tolerance. A YAML scalar spelling a
--   non-finite number (@.inf@, @.nan@) decodes as a STRING, not a
--   number, so a plain @Float@ leaf fails the whole document's parse on
--   one such leaf — turning a field-local domain rejection into a
--   structural failure that discards every other setting in the file.
--   Recognising those spellings here keeps the document structurally
--   valid, so the domain boundary can reject exactly the one leaf.
--
--   This module depends on nothing local but 'UPrelude', so both the
--   video config ('Engine.Graphics.Config') and the world-generation
--   config ('World.Generate.Config.Types') import it without a cycle.
module Engine.Core.Yaml.Scalar
  ( NumberSource(..)
  , parseNumberSource
  , nonFiniteSpelling
  ) where

import UPrelude
import qualified Data.Text as T
import Data.Aeson (Value(..), FromJSON(..))
import Data.Aeson.Types (Parser, typeMismatch)

-- | One decoded numeric leaf: its value, and the text the document
--   spelled it with. 'nsText' is what a rejection quotes.
data NumberSource = NumberSource
    { nsValue ∷ !Double
    , nsText  ∷ !Text
    } deriving (Show, Eq)

-- | The instance every numeric leaf decoded through @.:?@ uses. Aeson
--   prefixes a failure with the document path, so the offending key is
--   named without this parser knowing it.
instance FromJSON NumberSource where
    parseJSON = parseNumberSource "a number"

-- | Decode one numeric leaf, preserving its spelling. The first
--   argument describes what was expected, for the messages a genuinely
--   non-numeric scalar produces (e.g. @"a number"@, @"a ui_scale
--   number"@).
--
--   A number carries its own rendering; a string is accepted ONLY when
--   it spells a non-finite number, and any other string is the
--   structural error it always was.
parseNumberSource ∷ Text → Value → Parser NumberSource
parseNumberSource _expected v@(Number _) = do
    d ← parseJSON v
    pure (NumberSource d (tshow d))
parseNumberSource expected (String t) = case nonFiniteSpelling t of
    Just d  → pure (NumberSource d t)
    Nothing → fail ("expected " <> T.unpack expected <> ", got the string "
                      <> show t)
parseNumberSource expected v = typeMismatch (T.unpack expected) v

-- | The non-finite number a scalar spells, if it spells one: YAML
--   1.1\/1.2 core-schema (@.inf@, @+.inf@, @-.inf@, @.nan@), aeson's
--   (@+inf@, @-inf@) and Haskell's own 'show' forms, case-insensitively.
nonFiniteSpelling ∷ Text → Maybe Double
nonFiniteSpelling t
    | s `elem` [".inf", "+.inf", "inf", "+inf", "infinity", "+infinity"] = Just (1 / 0)
    | s `elem` ["-.inf", "-inf", "-infinity"]                            = Just (-1 / 0)
    | s `elem` [".nan", "nan"]                                           = Just (0 / 0)
    | otherwise                                                          = Nothing
  where
    s = T.toLower (T.strip t)
