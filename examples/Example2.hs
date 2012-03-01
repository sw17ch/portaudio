{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sound.PortAudio.Base
import Sound.PortAudio

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF
import qualified Data.Vector.Storable as V

import Control.Monad (foldM, foldM_, forM_)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Printf

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import System.Environment (getArgs)



framesPerBuffer :: Int
framesPerBuffer = 1024


simpleCallback :: MVar (V.Vector Float) -> MVar () -> StreamCallback CFloat CFloat
simpleCallback dataMVar doneMVar _ _ frames _ out = do
    pnts <- modifyMVar dataMVar (\vec -> let (x, y) = V.splitAt (fromIntegral frames) vec in return (y, x))
    if (V.null pnts)
        then (putMVar doneMVar () >> return Complete)
        else do
            V.foldM'_ (\i e -> pokeElemOff out i (realToFrac e) >> return (i + 1)) 0 pnts
            return Continue


withCallbackIO :: SF.Info -> MVar (V.Vector Float) -> IO (Either Error ())
withCallbackIO info fileData = do
    songDone <- newEmptyMVar
    
    let channels = SF.channels info
        smpl = (realToFrac $ SF.samplerate info)
        callbackFunc = Just $ simpleCallback fileData songDone 
    
    withDefaultStream 0 channels smpl (Just framesPerBuffer) callbackFunc Nothing $ \strm -> do
        allocaBytes (framesPerBuffer * channels) $ \out -> do
            out' <- newForeignPtr_ out
            startStream strm
            takeMVar songDone
            stopStream strm
            return $ Right ()  



runFunc :: MVar (V.Vector Float) -> Int -> Int -> ForeignPtr CFloat -> Stream CFloat CFloat -> IO ()
runFunc fileData frames channels out strm = do
    pnts <- modifyMVar fileData (\vec -> let (x, y) = V.splitAt (fromIntegral frames * channels) vec in return (y, x))
    if (V.null pnts) then (return ()) else do
        let totalLen = V.length pnts
                    
        withForeignPtr out $ \p -> V.foldM_ (\i val -> pokeElemOff p i (realToFrac val) >> return (i + 1)) 0 pnts

        writeStream strm (fromIntegral $ totalLen `div` channels) out
        runFunc fileData frames channels out strm
        
withBlockingIO :: SF.Info -> MVar (V.Vector Float) -> IO (Either Error ())
withBlockingIO info fileData = do
    let channels = SF.channels info
    withDefaultStream 0 channels (realToFrac $ SF.samplerate info) (Just framesPerBuffer) Nothing Nothing $ \strm -> do
        allocaBytes (framesPerBuffer * channels) $ \out -> do
            out' <- newForeignPtr_ out
            startStream strm
            runFunc fileData framesPerBuffer channels out' strm
            stopStream strm
            return $ Right ()
            

main :: IO ()
main = do
    [inFile] <- getArgs
    (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile

    let vecData = VSF.fromBuffer x
        channels = SF.channels info
            
    fileData <- newMVar vecData    
    putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
    putStrLn $ "channels: "    ++ (show $ SF.channels info)
    putStrLn $ "frames: "      ++ (show $ SF.frames info)
    putStrLn $ "dataLen: "     ++ (show $ V.length vecData)
    
    
    -- Choose either Blocking IO or the Callback system
    withPortAudio $ withBlockingIO info fileData
    --withPortAudio $ withCallbackIO info fileData
                
    return ()