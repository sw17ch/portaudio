{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- This is the Buffer Typeclass instance for Storable Vectors
-- that are provided in the Vector Package: http://hackage.haskell.org/package/vector

-- When using, it is critical that you do not Freeze and rethaw ever!
-- We want modifications to be seen by the underlying Pointer Directly

module Sound.PortAudio.Buffer.VectorPackage (
      Buffer
    , toBuffer
    , fromBuffer
    , withBuffer
) where

import qualified Data.Vector.Storable as SV
import qualified Sound.PortAudio.Buffer as PA
import Foreign.Storable (Storable)


newtype Buffer a = Buffer { fromBuffer :: SV.Vector a }

toBuffer :: SV.Vector a -> Buffer a
toBuffer = Buffer

withBuffer :: (SV.Vector a -> SV.Vector b) -> Buffer a -> Buffer b
withBuffer f = toBuffer . f . fromBuffer

instance Storable a => PA.Buffer Buffer a where
    fromForeignPtr p n = do
        let buff = SV.unsafeFromForeignPtr0 p n
        return $ toBuffer buff
        
    toForeignPtr       = return . SV.unsafeToForeignPtr0 . fromBuffer