module Text.JSON.ToJson (ToJson(..)) where

import qualified Data.JSON.Types as J
import Data.Int (Int32)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text.Lazy (pack)
import Data.Int (Int64)

class ToJson a where
  toJson :: a -> J.Value

  toJsons :: [a] -> J.Value
  toJsons = J.ValueArray . map toJson

instance ToJson a => ToJson [a] where
  toJson = toJsons

instance ToJson () where
  toJson _ = J.ValueArray []

instance (ToJson a, ToJson b) => ToJson (a,b) where
  toJson (a,b) = J.ValueArray [ toJson a, toJson b ]

instance (ToJson a, ToJson b, ToJson c) => ToJson (a,b,c) where
  toJson (a,b,c) = J.ValueArray [ toJson a, toJson b, toJson c ]

instance (ToJson a, ToJson b, ToJson c, ToJson d) => ToJson (a,b,c,d) where
  toJson (a,b,c,d) = J.ValueArray [toJson a, toJson b, toJson c, toJson d]

instance ToJson J.Value where
    toJson = id

instance ToJson Bool where
  toJson = J.ValueAtom . J.AtomBoolean

instance ToJson Char where
  toJson c = J.ValueAtom $ J.AtomText $ pack (c:[])
  toJsons  = J.ValueAtom . J.AtomText . pack

instance (ToJson LBS.ByteString) where
  toJson = toJson . LC.unpack
instance (ToJson Int32) where
  toJson = J.ValueAtom . J.AtomNumber . toRational
instance (ToJson Int64) where
  toJson = J.ValueAtom . J.AtomNumber . toRational
instance (ToJson Int) where
  toJson = J.ValueAtom . J.AtomNumber . toRational
instance (ToJson Float) where
  toJson = J.ValueAtom . J.AtomNumber . toRational
instance (ToJson Double) where
  toJson = J.ValueAtom . J.AtomNumber . toRational
