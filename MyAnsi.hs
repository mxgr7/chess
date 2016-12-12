module MyAnsi (outputWithDelay, outputWithKey) where

import System.Console.ANSI
import System.Posix.Signals
import Control.Concurrent
import Control.Monad
import System.IO

outputWithDelay :: Int -> [String] -> IO ()
outputWithDelay d = outputFold $ threadDelay $ d * 1000

outputWithKey :: [String] -> IO ()
outputWithKey = outputFold $ do
  prevBuff <- hGetBuffering stdin
  prevEcho <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetBuffering stdin prevBuff
  hSetEcho stdin prevEcho
  return c

outputFold :: IO a -> [String] -> IO ()
outputFold action xs = do
  hideCursor
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch $ backToNormal >> killThread tid) Nothing
  foldM_ (\x' x -> do
    eraseString x'
    putStr x
    _ <- action
    return x) "" xs
  backToNormal

backToNormal :: IO ()
backToNormal = do
  setSGR [Reset]
  clearFromCursorToScreenEnd
  putStrLn ""
  setCursorColumn 0
  showCursor

eraseString :: String -> IO ()
eraseString x = do
  let height = length (filter (=='\n') x)
  when (height > 0) $ cursorUpLine height
  setCursorColumn 0
  clearFromCursorToScreenEnd
