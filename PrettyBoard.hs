module PrettyBoard (prettyBoard) where
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
prettyBoard board = unlines $ (letters++" "++meta):rows
  where rows = [columns v (bMap board) | v <- reverse [1..8]]
        columns v b = unwords $ show v :
                      [prettySquare h v $ Map.lookup (Sq h v) b | h <- [1..8]]
        prettySquare _ _ (Just p) = prettyPieceInverse p
        prettySquare h v _ | (h+v) `mod` 2 == 0 = " "
                           | otherwise          = "▥"
        letters = unwords $ " " : [l:"" | l <- ['A'..'H']]
        meta = show (enPassant board) ++ show (castling board)
