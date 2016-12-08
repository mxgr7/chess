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
        letters = unwords $ " " : [l:"" | l <- ['A'..'H']] ++ [show $ enPassant board]

putBoard :: Board -> IO ()
putBoard = putStrLn . prettyBoard 

simpleBoard :: Board
simpleBoard = Board bm White Nothing
  where bm = 
             Map.insert (1,4) (PPiece White King) .
             Map.insert (4,4) (PPiece White Pawn) .
             Map.insert (8,4) (PPiece Black Rook) .
             Map.insert (2,1) (PPiece Black Rook) .
             Map.insert (8,8) (PPiece Black King) .
             id $ Map.empty

b1 :: Board
b1 = simpleBoard
bs :: [Board]
bs = legalPlys b1
b2 :: Board
b2 = bs !! 0
