{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.JSON.Enumerator
    ( renderEvents
    , renderEventsToBytes
    , renderValue
    , renderAtom
    , JsonException (..)
    ) where

import qualified Data.Enumerator as E
import Data.Enumerator ((>>==), ($$))
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.Monoid (mappend, mconcat)
import Data.Text.Lazy (Text, unpack)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Blaze.ByteString.Builder.Internal (fromWriteList)
import Blaze.ByteString.Builder.Char.Utf8 (writeChar)
import Blaze.ByteString.Builder.ByteString (writeByteString)
import Data.Bits (shiftR, (.&.))
import Data.JSON.Types
import Control.Monad.IO.Class (MonadIO)
import Data.List (foldl')
import qualified Data.Map as Map

data GState = NoState
            | InArray GState
            | InArray1 GState
            | InObject GState
            | InObjectValue GState
            | InObject1 GState
    deriving Show

renderEvents :: Monad m => E.Enumeratee Event Builder m b
renderEvents =
    loop NoState
  where
    loop state = E.checkDone $ \k -> do
        me <- E.head
        case me of
            Nothing -> k E.EOF >>== return
            Just e -> do
                (bs, state') <- go e state
                k (E.Chunks [bs]) >>== loop state'

    go EventEndArray (InArray s) = return (fromChar ']', s)
    go EventEndArray (InArray1 s) = return (fromChar ']', s)
    go EventEndArray s = E.throwError $ UnexpectedEndArray s
    go EventEndObject (InObject s) = return (fromChar '}', s)
    go EventEndObject (InObject1 s) = return (fromChar '}', s)
    go EventEndObject s = E.throwError $ UnexpectedEndObject s
    go EventBeginArray s =
        let (b, s') = incrState s
         in return (b $ fromChar '[', InArray s')
    go EventBeginObject s =
        let (b, s') = incrState s
         in return (b $ fromChar '{', InObject s')
    go (EventAttributeName t) (InObject s) = return (escape t, InObjectValue s)
    go (EventAttributeName t) (InObject1 s) = return (fromChar ',' `mappend` escape t, InObjectValue s)
    go (EventAttributeName n) s = E.throwError $ UnexpectedAttributeName n s
    go (EventAtom a) s = scalar (renderAtom a) s

    incrState (InArray s) = (id, InArray1 s)
    incrState (InArray1 s) = (mappend $ fromChar ',', InArray1 s)
    incrState (InObject s) = (id, InObjectValue s)
    incrState (InObject1 s) = (mappend $ fromChar ',', InObjectValue s)
    incrState (InObjectValue s) = (mappend $ fromChar ':', InObject1 s)
    incrState NoState = (id, NoState)

    scalar _ (InObject s) = E.throwError $ ExpectedAttributeName s
    scalar _ (InObject1 s) = E.throwError $ ExpectedAttributeName s
    scalar b s =
        let (b', s') = incrState s
         in return (b' b, s')

data JsonException = UnexpectedEndArray GState
                   | UnexpectedEndObject GState
                   | UnexpectedAttributeName Text GState
                   | ExpectedAttributeName GState
    deriving (Show, Typeable)
instance Exception JsonException

renderEventsToBytes :: MonadIO m => E.Enumeratee Event ByteString m b
renderEventsToBytes s = E.joinI $ renderEvents $$ builderToByteString s

renderValue :: Value -> Builder
renderValue (ValueAtom a) = renderAtom a
renderValue (ValueArray []) = fromByteString "[]"
renderValue (ValueArray (x:xs)) =
    foldl' go (fromChar '[' `mappend` renderValue x) xs
                            `mappend` fromChar ']'
  where
    go y a = y `mappend` fromChar ',' `mappend` renderValue a
renderValue (ValueObject o) =
    case Map.toList o of
        [] -> fromByteString "{}"
        (x:xs) -> foldl' go (fromChar '{' `mappend` renderPair x) xs
                                          `mappend` fromChar '}'
  where
    renderPair (k, v) = escape k `mappend` fromChar ':' `mappend` renderValue v
    go y p = y `mappend` fromChar ',' `mappend` renderPair p

renderAtom :: Atom -> Builder
renderAtom AtomNull = fromByteString "null"
renderAtom (AtomBoolean True) = fromByteString "true"
renderAtom (AtomBoolean False) = fromByteString "false"
renderAtom (AtomNumber r) = fromShow (fromRational r :: Double)
renderAtom (AtomText t) = escape t

escape :: Text -> Builder
escape t = mconcat
    [ fromChar '"'
    , fromWriteList writeJChar $ unpack t
    , fromChar '"'
    ]
  where
    writeJChar '\"' = writeByteString "\\\""
    writeJChar '\\' = writeByteString "\\\\"
    writeJChar '/' = writeByteString "\\/"
    writeJChar '\b' = writeByteString "\\b"
    writeJChar '\f' = writeByteString "\\f"
    writeJChar '\n' = writeByteString "\\n"
    writeJChar '\r' = writeByteString "\\r"
    writeJChar '\t' = writeByteString "\\t"
    writeJChar c
        | c < '\x10' = writeByteString "\\u000"
                        `mappend` writeChar (hex $ fromEnum c)
        | c < '\x20' = writeByteString "\\u00"
                        `mappend` writeChar (hex i1)
                        `mappend` writeChar (hex i2)
          where
            i = fromEnum c
            i1 = i `shiftR` 4
            i2 = i .&. 15
            hex 0 = '0'
            hex 1 = '1'
            hex 2 = '2'
            hex 3 = '3'
            hex 4 = '4'
            hex 5 = '5'
            hex 6 = '6'
            hex 7 = '7'
            hex 8 = '8'
            hex 9 = '9'
            hex 10 = 'A'
            hex 11 = 'B'
            hex 12 = 'C'
            hex 13 = 'D'
            hex 14 = 'E'
            hex 15 = 'F'
    writeJChar c = writeChar c
