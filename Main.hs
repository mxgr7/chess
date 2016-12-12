module Main (main) where

import BasicChess
import ChessTree
import PrettyBoard
import MyAnsi

main :: IO ()
main = do
  putStrLn "You will now see a chess tree."
  let bs = boardList initialBoard
  let t = chessTree initialBoard
  mapM_ (print . length) $ levels t
  outputWithKey $ map prettyBoard bs
  return ()

