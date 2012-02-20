{-# LANGUAGE MultiParamTypeClasses #-}

module Sound.PortAudio.Buffer.StorableVector (
      Buffer
    , toBuffer
    , fromBuffer
    , withBufer
) where

import qualified Data.StorableVector.Base as SV
import qualified Sound.PortAudio as PA
import Foreign.Storable (Storable)


newtype Buffer a = Buffer { fromBuffer :: SV.Vector a }

toBuffer :: SV.Vector a -> Buffer a
toBuffer = Buffer

withBuffer :: (SV.Vector a -> SV.Vector b) -> Buffer a -> Buffer b
withBuffer f = toBuffer . f . fromBuffer

instance Storable a => PA.Buffer Buffer a where
    fromForeignPtr p i n = return $ toBuffer $ SV.SV p i n
    toForeignPtr         = return . SV.toForeignPtr . fromBuffer