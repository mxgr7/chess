module ChessTree (boardList, boardList', chessTree, levels) where

import BasicChess
import Data.Tree (Tree(..), unfoldTree, flatten, levels)

flattenUniq :: (Eq a) => Tree a -> [a]
flattenUniq (Node r ts) = r : f [r] ts
  where f ex ts' = xs ++ concatMap (f (ex++xs) . subForest) newTs
                   where newTs = filter ((`notElem` ex) . rootLabel) ts'
                         xs    = map rootLabel newTs

chessTree :: Board -> Tree Board
chessTree = unfoldTree (\b -> (b, legalPlys b))

boardList :: Board -> [Board]
boardList = flattenUniq . chessTree

boardList' :: Board -> [Board]
boardList' = flatten . chessTree
