{-# LANGUAGE OverloadedStrings #-}
import Text.JSON.Enumerator
import Data.JSON.Types
import Data.Enumerator ((>>==), ($$))
import qualified Data.Enumerator as E
import qualified Data.ByteString.Lazy as L
import Data.Ratio ((%))
import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as Map

main = do
    x <- E.run_ $ E.enumList 1 [EventBeginObject, EventAttributeName "foo", EventBeginArray, EventAtom $ AtomText "bar", EventEndArray, EventEndObject] $$ E.joinI $ renderEvents $$ E.consume
    L.putStrLn $ toLazyByteString $ mconcat x
    y <- E.run_ $ E.enumList 1 [EventBeginObject, EventAttributeName "foo", EventBeginArray, EventAtom $ AtomText "bar", EventAtom $ AtomNumber (5 % 4), EventAtom $ AtomNumber (1 % 3), EventEndArray, EventEndObject] $$ E.joinI $ renderEventsToBytes $$ E.consume
    L.putStrLn $ L.fromChunks y
    L.putStrLn $ toLazyByteString $ renderValue $ ValueObject $ Map.fromList
        [ ("foo", ValueArray
            [ ValueAtom $ AtomBoolean True
            ])
        ]
