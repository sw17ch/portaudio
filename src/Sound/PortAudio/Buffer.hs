module Sound.PortAudio.Buffer where

import qualified Data.Vector as V

-- We use these to convert from a User -> PortAudio type and Vice Versa
class (Storable a, StreamFormat e) => Encodeable a e | a -> e where
    toStream   :: a -> e
    fromStream :: e -> a

instance Encodeable Int8   Int8   where
    toStream   = id
    fromStream = id
    
instance Encodeable Int16  Int16  where
    toStream   = id
    fromStream = id
    
instance Encodeable Int32  Int32  where
    toStream   = id
    fromStream = id
    
instance Encodeable CFloat CFloat where
    toStream   = id
    fromStream = id
    
instance Encodeable Int    Int32  where
    toStream   = fromIntegral
    fromStream = fromIntegral
    
instance Encodeable Float  CFloat where
    toStream   = realToFrac
    fromStream = realToFrac
    
instance Encodeable Double CFloat where
    toStream   = realToFrac
    fromStream = realToFrac




-- Code modified from hsndfile implementation
-- | Buffer class for I\/O on soundfile handles.
class Encodeable e s => Buffer a e s | e -> s where
    copyFromBuffer :: Ptr s -> Int -> IO (a e)
    writeToBuffer  :: a e -> IO (ForeignPtr s, Int)
    

instance Buffer ([] e s) where
    copyFromBuffer ptr len = go [] len where
        go !lst 0  = return $ lst 
        go !lst !t = do
            x <- fromStream <$> peekElemOff ptr t
            go (x:lst) (t-1)
        
    writeToBuffer store = do
        let len = length store
        array <- mallocArray len
        foldM_ (\i el -> pokeElemOff array i (toStream el) >> return (i + 1)) 0 len
        return $ (newForeignPtr array, len)
    
    
instance Buffer (Vector e s) where
    copyFromBuffer ptr len =
    writeToBuffer store ptr maxSize = 
        
instance Buffer ((Array Int) e s) where
    copyFromBuffer ptr len = 
    writeToBuffer store ptr maxSize = 