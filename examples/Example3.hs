{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sound.PortAudio.Base
import Sound.PortAudio

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF

import Data.List (transpose)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad (foldM, foldM_, forM_, when, unless, zipWithM_)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, forkIO)
import Text.Printf

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import qualified UI.HSCurses.Curses as Curses (move, refresh, scrSize, endWin)
import qualified UI.HSCurses.CursesHelper as CursesH (gotoTop, drawLine, start, end)

import System.Environment (getArgs)
import Transform ( kPointFFT )

data TimedBuffer = TimedBuffer {
    dataPoints :: V.Vector Float,
    timeStamp  :: Double
}

framesPerBuffer :: Int
framesPerBuffer = 300

type FFTPrintFunc = MVar [ Float ] -> IO ()

rawPrintFunc :: FFTPrintFunc
rawPrintFunc mvar = do
    dta <- takeMVar mvar
    unless (null dta) $ do
        print dta
        rawPrintFunc mvar
        
cursesRawPrintFunc :: FFTPrintFunc
cursesRawPrintFunc mvar = CursesH.start >> workerFunc >> CursesH.end where
    workerFunc = do
        dta <- takeMVar mvar
        unless (null dta) $ do
            (maxRows, maxCols) <- Curses.scrSize

            let compressed = transpose $ take maxCols $ asciiDisplayRaw maxRows 5 (0, 25) (take maxCols dta) '*'

                asciiDisplayRaw :: Int -> Int -> (Float, Float) -> [Float] -> Char -> [String]
                asciiDisplayRaw height barWidth (x,y) points symbol = result where
                    result = concatMap (\str -> replicate barWidth str) pure
                    pure = map (\p -> let req = (func p) in (replicate (height - req) ' ') ++ (replicate req symbol)) points
                    func pnt | pnt > y   = height
                             | pnt < x   = 0
                             | otherwise = ceiling $ (realToFrac height) * (pnt - x) / spanning

                    spanning = y - x
                    len = length points         
                
                displayBars :: Int -> [String] -> IO ()
                displayBars width items = zipWithM_ (\row string -> Curses.move row 0 >> CursesH.drawLine width string) [0..] items
            
            displayBars (maxCols - 1) compressed >> Curses.refresh

            workerFunc
            


processFourierData :: Chan TimedBuffer -> Int -> Int -> IO ()
processFourierData readChannel channels pointsFFT = do
    fourierDrawing <- newMVar (U.fromList [])
    toPrinter      <- newEmptyMVar
    fftVec         <- MV.new pointsFFT     
    forkIO $ defaultPrintFunc toPrinter

    let workerFunc pos lastTime mutVec fftUpdate = do
        if pos == pointsFFT
            then do
                frozen <- U.freeze mutVec
                swapMVar fourierDrawing (kPointFFT frozen)
                workerFunc 0 lastTime mutVec True
            else do
                TimedBuffer pnts ts <- readChan readChannel
                if (V.null pnts) then (putMVar toPrinter [] >> putStrLn "Ending!") else do
                    let numPoints              = V.length pnts
                        fftPrint               = (ts - lastTime >= updateInterval && fftUpdate)
                        slotsAvail             = pointsFFT - pos
                        toFourier              = V.take slotsAvail pnts
                        newFFTUpdate           = if fftPrint then False else fftUpdate
                        newTime                = if fftPrint then ts else lastTime
                                    
                    forM_ [0..(V.length toFourier - 1)] $ \i -> MV.unsafeWrite mutVec (pos + i) (toFourier V.! i)
                    
                    when fftPrint $ do
                        drawData <- readMVar fourierDrawing
                        putMVar toPrinter (U.toList drawData)
                    
                    workerFunc (pos + V.length toFourier) newTime mutVec newFFTUpdate

    workerFunc 0 0 fftVec False
    

runFunc :: MVar (V.Vector Float) -> Chan TimedBuffer -> Int -> Int -> ForeignPtr CFloat -> Stream CFloat CFloat -> IO ()
runFunc fileData fourierChannel frames channels out strm = do
    pnts <- modifyMVar fileData (\vec -> let (x, y) = V.splitAt (fromIntegral frames * channels) vec in return (y, x))
    if (V.null pnts) then (writeChan fourierChannel (TimedBuffer (V.fromList []) 0)) else do
        let totalLen = V.length pnts
        withForeignPtr out $ \p -> V.foldM_ (\i val -> pokeElemOff p i (realToFrac val) >> return (i + 1)) 0 pnts
        
        (Right (PaTime tme)) <- getStreamTime strm
        writeStream strm (fromIntegral $ totalLen `div` channels) out
        writeChan fourierChannel (TimedBuffer pnts (realToFrac tme))
        
        runFunc fileData fourierChannel frames channels out strm
        

withBlockingIO :: SF.Info -> MVar (V.Vector Float) -> IO (Either Error ())
withBlockingIO info fileData = do
    let channels = SF.channels info
    fourierChannel <- newChan
    forkIO $ processFourierData fourierChannel channels pointsPerTransform
    
    outInfo <- getDefaultOutputInfo
    case outInfo of
        Left err -> return $ Left err
        Right (devIndex, devInfo) -> do
            let outInfo = Just $ StreamParameters devIndex 2 (defaultHighOutputLatency devInfo) 
            withStream Nothing outInfo (realToFrac $ SF.samplerate info) (Just framesPerBuffer) [ClipOff] Nothing Nothing $ \strm -> do
                allocaBytes (framesPerBuffer * channels) $ \out -> do
                out' <- newForeignPtr_ out
                startStream strm            
                runFunc fileData fourierChannel framesPerBuffer channels out' strm
                stopStream strm
                return $ Right ()




updateInterval :: Double
updateInterval = 0.5

pointsPerTransform :: Int
pointsPerTransform = 1024            

defaultPrintFunc :: FFTPrintFunc
defaultPrintFunc = cursesRawPrintFunc

main :: IO ()
main = do
    [inFile] <- getArgs
    (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile
    let vecData = V.convert $ VSF.fromBuffer x
        channels = SF.channels info
        
    fileData <- newMVar vecData
    withPortAudio $ withBlockingIO info fileData

    return ()