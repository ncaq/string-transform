{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}
module Data.String.Transform where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Short     as BS
import qualified Data.ByteString.UTF8      as BU
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL

class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString B.ByteString where
    toString = BU.toString

instance ToString BL.ByteString where
    toString = BLU.toString

instance ToString BS.ShortByteString where
    toString = toString . BS.fromShort

instance ToString T.Text where
    toString = T.unpack

instance ToString TL.Text where
    toString = TL.unpack

class ToByteStringStrict a where
    toByteStringStrict :: a -> B.ByteString

instance ToByteStringStrict String where
    toByteStringStrict = BU.fromString

instance ToByteStringStrict B.ByteString where
    toByteStringStrict = id

instance ToByteStringStrict BL.ByteString where
    toByteStringStrict = BL.toStrict

instance ToByteStringStrict BS.ShortByteString where
    toByteStringStrict = BS.fromShort

instance ToByteStringStrict T.Text where
    toByteStringStrict = T.encodeUtf8

instance ToByteStringStrict TL.Text where
    toByteStringStrict = toByteStringStrict . TL.toStrict

class ToByteStringLazy a where
    toByteStringLazy :: a -> BL.ByteString

instance ToByteStringLazy String where
    toByteStringLazy = BLU.fromString

instance ToByteStringLazy B.ByteString where
    toByteStringLazy = BL.fromStrict

instance ToByteStringLazy BL.ByteString where
    toByteStringLazy = id

instance ToByteStringLazy BS.ShortByteString where
    toByteStringLazy = toByteStringLazy . BS.fromShort

instance ToByteStringLazy T.Text where
    toByteStringLazy = toByteStringLazy . T.encodeUtf8

instance ToByteStringLazy TL.Text where
    toByteStringLazy = TL.encodeUtf8

class ToShortByteString a where
    toShortByteString :: a -> BS.ShortByteString

instance ToShortByteString String where
    toShortByteString = toShortByteString . BU.fromString

instance ToShortByteString B.ByteString where
    toShortByteString = BS.toShort

instance ToShortByteString BL.ByteString where
    toShortByteString = toShortByteString . toByteStringStrict

instance ToShortByteString BS.ShortByteString where
    toShortByteString = id

instance ToShortByteString T.Text where
    toShortByteString = toShortByteString . toByteStringStrict

instance ToShortByteString TL.Text where
    toShortByteString = toShortByteString . toByteStringStrict

class ToTextStrict a where
    toTextStrict :: a -> T.Text

instance ToTextStrict String where
    toTextStrict = T.pack

instance ToTextStrict B.ByteString where
    toTextStrict = T.decodeUtf8

instance ToTextStrict BL.ByteString where
    toTextStrict = toTextStrict . BL.toStrict

instance ToTextStrict BS.ShortByteString where
    toTextStrict = toTextStrict . BS.fromShort

instance ToTextStrict T.Text where
    toTextStrict = id

instance ToTextStrict TL.Text where
    toTextStrict = TL.toStrict

class ToTextLazy a where
    toTextLazy :: a -> TL.Text

instance ToTextLazy String where
    toTextLazy = TL.pack

instance ToTextLazy B.ByteString where
    toTextLazy = toTextLazy . BL.fromStrict

instance ToTextLazy BL.ByteString where
    toTextLazy = TL.decodeUtf8

instance ToTextLazy BS.ShortByteString where
    toTextLazy = toTextLazy . toByteStringLazy

instance ToTextLazy T.Text where
    toTextLazy = TL.fromStrict

instance ToTextLazy TL.Text where
    toTextLazy = id
