module Main where

import Sound.PortAudio.Base
import Sound.PortAudio

import qualified Data.Vector as V
import Control.Monad (foldM, forM_)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Printf

import Foreign.C.Types
import Foreign.Storable

import System.IO.Unsafe
import Data.IORef

numSeconds :: Int
numSeconds = 5

sampRate :: Int
sampRate = 44100

framesPerBuffer :: Int
framesPerBuffer = 100

tableSize :: Int
tableSize = 200

data SineTable = SineTable {
    sine            :: V.Vector Float,
    left_phase      :: Int,
    right_phase     :: Int
}

newTable :: Int -> SineTable
newTable sze = SineTable vec 0 0 where
    intSze = fromInteger $ toInteger sze
    vec = V.fromList $ map (\i -> sin $ (i / intSze) * pi * 2) [0..(intSze - 1)]


-- Recall:
-- type StreamCallback input output =
--      Base.PaStreamCallbackTimeInfo -- ^ Timing information for the input and output
--   -> [StreamCallbackFlag]          -- ^ Status flags
--   -> CULong                        -- ^ # of input samples
--   -> Ptr input                     -- ^ input samples
--   -> Ptr output                    -- ^ where to write output samples
--   -> IO StreamResult               -- ^ What to do with the stream, plus the output to stream
--

paTestCallback :: MVar SineTable -> StreamCallback CFloat CFloat
paTestCallback mvar _ _ frames _ out = do
    tbl <- readMVar mvar

    let func (l, r) i = do
        pokeElemOff out (2 * i)      (realToFrac $ (V.!) (sine tbl) l)
        pokeElemOff out (2 * i + 1)  (realToFrac $ (V.!) (sine tbl) r)
        let newL = let x = l + 1 in (if x >= tableSize then (x - tableSize) else x)
        let newR = let x = r + 1 in (if x >= tableSize then (x - tableSize) else x)
        return (newL, newR)

    (newL', newR') <- foldM func (left_phase tbl, right_phase tbl) [0..(fromIntegral $ frames - 1)]
        
    swapMVar mvar (tbl { left_phase = newL', right_phase = newR' })
    return Continue

streamFinished :: String -> IO ()
streamFinished msg = putStrLn ("Stream Completed: " ++ msg)

-- Recall:
-- withStream
--    :: (StreamFormat input, StreamFormat output) =>
--       Maybe (StreamParameters input)      -- ^ Input parameters
--    -> Maybe (StreamParameters output)     -- ^ Output parameters
--    -> CDouble                             -- ^ Sample rate
--    -> Maybe CULong                        -- ^ Frames per buffer
--    -> [StreamOpenFlag]                    -- ^ Stream flags
--    -> Maybe (StreamCallback input output) -- ^ Callback, or @Nothing@ for a blocking read/write stream
--    -> (Stream -> IO (Either Error a))     -- ^ Computation to apply
--    -> IO (Either Error a)

main :: IO ()
main = do
    putStrLn $ printf "PortAudio Test: output sine wave. SR = %d, BufSize = %d" sampRate (fromIntegral framesPerBuffer :: Int)
    result <- withPortAudio $ do
        withDefaultOutputInfo $ \(out, outInfo) -> do
            baseTbl <- newMVar (newTable tableSize)
            
            let strmParams = (Just $ StreamParameters out 2 (defaultLowOutputLatency outInfo))
                smpRate = (realToFrac sampRate)
                frmPerBuf = (Just $ fromIntegral framesPerBuffer)
                callb = (Just $ paTestCallback baseTbl)
            
            withStream Nothing strmParams smpRate frmPerBuf [ClipOff] callb $ \strm -> do
                s1 <- addStreamFin (makeFinishedCallback $ streamFinished "Sine Wave") strm
                s2 <- startStream strm
                threadDelay $ numSeconds * 1000 * 1000
                s3 <- stopStream strm
                return $ Right ()

    case result of
        Left err -> print err
        Right _ -> return ()
