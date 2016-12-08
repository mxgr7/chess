module PrettyBoard (prettyBoard, putBoard) where
import BasicChess
import qualified Data.Map as Map

prettyPiece :: PlayerPiece -> String
prettyPiece (PPiece White King)   = "♔"
prettyPiece (PPiece White Queen)  = "♕"
prettyPiece (PPiece White Rook)   = "♖"
prettyPiece (PPiece White Bishop) = "♗"
prettyPiece (PPiece White Knight) = "♘"
prettyPiece (PPiece White Pawn)   = "♙"
prettyPiece (PPiece Black King)   = "♚"
prettyPiece (PPiece Black Queen)  = "♛"
prettyPiece (PPiece Black Rook)   = "♜"
prettyPiece (PPiece Black Bishop) = "♝"
prettyPiece (PPiece Black Knight) = "♞"
prettyPiece (PPiece Black Pawn)   = "♟"

prettyPieceInverse :: PlayerPiece -> String
prettyPieceInverse (PPiece Black p) = prettyPiece (PPiece White p)
prettyPieceInverse (PPiece White p) = prettyPiece (PPiece Black p)

prettyBoard :: Board -> String
prettyBoard board = unlines $ letters:rows
  where rows = [columns v (bMap board) | v <- reverse [1..bDim]]
        columns v b = unwords $ show v : [prettySquare h v $ Map.lookup (h,v) b | h <- [1..bDim]]
        prettySquare _ _ (Just p) = prettyPieceInverse p
        prettySquare h v _ | (h+v) `mod` 2 == 0 = " "
                           | otherwise          = "▥"
        letters = unwords $ " " : [l:"" | l <- ['A'..'H']] ++ [show $ enPassant board] ++ [show $ kingStatus board]

putBoard :: Board -> IO ()
putBoard = putStrLn . prettyBoard 

simpleBoard :: Board
simpleBoard = Board bm White Nothing (False,False)
  where bm = 
             Map.insert (5,1) (PPiece White King) .
             Map.insert (6,1) (PPiece Black Bishop) .
             Map.insert (8,8) (PPiece Black King) .
             id $ Map.empty

b1 :: Board
b1 = simpleBoard
bs :: [Board]
bs = legalPlys b1
b2 :: Board
b2 = bs !! 0
