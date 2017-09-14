{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.String.Transform where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
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

instance ToString T.Text where
    toString = T.unpack

instance ToString TL.Text where
    toString = TL.unpack

instance Show a => ToString a where
    toString = show

class ToByteStringStrict a where
    toByteStringStrict :: a -> B.ByteString

instance ToByteStringStrict String where
    toByteStringStrict = BU.fromString

instance ToByteStringStrict B.ByteString where
    toByteStringStrict = id

instance ToByteStringStrict BL.ByteString where
    toByteStringStrict = BL.toStrict

instance ToByteStringStrict T.Text where
    toByteStringStrict = T.encodeUtf8

instance ToByteStringStrict TL.Text where
    toByteStringStrict = T.encodeUtf8 . TL.toStrict

instance Show a => ToByteStringStrict a where
    toByteStringStrict = toByteStringStrict . show

class ToByteStringLazy a where
    toByteStringLazy :: a -> BL.ByteString

instance ToByteStringLazy String where
    toByteStringLazy = BLU.fromString

instance ToByteStringLazy B.ByteString where
    toByteStringLazy = BL.fromStrict

instance ToByteStringLazy BL.ByteString where
    toByteStringLazy = id

instance ToByteStringLazy T.Text where
    toByteStringLazy = BL.fromStrict . T.encodeUtf8

instance ToByteStringLazy TL.Text where
    toByteStringLazy = TL.encodeUtf8

instance Show a => ToByteStringLazy a where
    toByteStringLazy = toByteStringLazy . show

class ToTextStrict a where
    toTextStrict :: a -> T.Text

instance ToTextStrict String where
    toTextStrict = T.pack

instance ToTextStrict B.ByteString where
    toTextStrict = T.decodeUtf8

instance ToTextStrict BL.ByteString where
    toTextStrict = T.decodeUtf8 . BL.toStrict

instance ToTextStrict T.Text where
    toTextStrict = id

instance ToTextStrict TL.Text where
    toTextStrict = TL.toStrict

instance Show a => ToTextStrict a where
    toTextStrict = toTextStrict . show

class ToTextLazy a where
    toTextLazy :: a -> TL.Text

instance ToTextLazy String where
    toTextLazy = TL.pack

instance ToTextLazy B.ByteString where
    toTextLazy = TL.decodeUtf8 . BL.fromStrict

instance ToTextLazy BL.ByteString where
    toTextLazy = TL.decodeUtf8

instance ToTextLazy T.Text where
    toTextLazy = TL.fromStrict

instance ToTextLazy TL.Text where
    toTextLazy = id

instance Show a => ToTextLazy a where
    toTextLazy = toTextLazy . show
