{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- This is the Buffer Typeclass instance for Storable Vectors
-- that are provided in the StorableVector Package: http://hackage.haskell.org/package/storablevector

-- When using, it is critical that you do not Freeze and rethaw ever!
-- We want modifications to be seen by the underlying Pointer Directly

module Sound.PortAudio.Buffer.StorableVectorPackage (
      Buffer
    , toBuffer
    , fromBuffer
    , withBuffer
) where

import qualified Data.StorableVector.Base as SV
import qualified Sound.PortAudio.Buffer as PA
import Foreign.Storable (Storable)


newtype Buffer a = Buffer { fromBuffer :: SV.Vector a }

toBuffer :: SV.Vector a -> Buffer a
toBuffer = Buffer

withBuffer :: (SV.Vector a -> SV.Vector b) -> Buffer a -> Buffer b
withBuffer f = toBuffer . f . fromBuffer

instance Storable a => PA.Buffer Buffer a where
    fromForeignPtr p n   = return $ toBuffer $ SV.SV p 0 n
    toForeignPtr x       = do
        let (a, _, b) = SV.toForeignPtr $ fromBuffer x
        return $ (a, b)