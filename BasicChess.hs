module BasicChess where

import Data.Int (Int8)
import Data.Word (Word8)
import Data.Maybe (isNothing)
import qualified Data.Map as Map

bDim :: Word8
bDim = 8

data Player = White | Black
              deriving (Show, Eq)

data Piece = Rook | Knight | Bishop | Queen | King | Pawn
             deriving (Show, Eq)

data PlayerPiece = PPiece {
                     player :: Player
                   , piece  :: Piece
                   }
                   deriving (Show, Eq)

data Board = Board {
                     bMap :: BoardMap
                   , bPlayer :: Player
                   , enPassant :: Maybe Square
                   }

type Square = (Word8, Word8)
type SquareTrans = (Int8, Int8)

type BoardMap = Map.Map Square PlayerPiece

turnBoard :: Board -> Board
turnBoard (Board m p _) = Board m (opponent p) Nothing

bLookup :: Square -> Board -> Maybe PlayerPiece
bLookup s b = Map.lookup s $ bMap b

legalPlys :: Board -> [Board]
legalPlys b = do
  (s,p) <- pcsByPlayer b $ bPlayer b
  st <- basicTrans p
  let s' = s /+/ st
  b' <- if   onBoard s'
          && not (occByPl s' b)
          && isLegal b p s s'
        then performPly p s s' b
        else []
  let pl = bPlayer b
  if sqUnderAttack b' (opponent pl) $ kingSq b' pl
    then []
    else return b'

kingSq :: Board -> Player -> Square
kingSq b pl = (fst . head . Map.toList . Map.filter f) $ bMap b
  where f (PPiece pl' King) | pl' == pl = True
        f _ = False


-- This function assumes:
--  * The target square is on the board and is not occupied by a piece of the
--    same color.
--  * There are conditions under which the move between the two squares is legal
--    (e.g. a pawn advancing more than 2 squares is never legal, while 2 squares
--    are legal some of the times).
-- This function does *not* take into account the "in check" state that results
-- from the movement.
isLegal :: Board -> PlayerPiece -> Square -> Square -> Bool

isLegal b (PPiece pl Pawn) s@(h,v) s'@(h',v') =
  (case v'-v of
    2  -> pl == White && v == 2
    -2 -> pl == Black && v == 7
    _  -> True) &&
  (case h'-h of
    0 -> all (emptySq b) $ path s s' -- Straight movement: path must be clear
    _ -> case bLookup s' b of
          Just (PPiece pl' _) | pl' /= pl  -> True -- Direct capture
          Nothing | enPassant b == Just s' -> True -- En-Passant capture
          _ -> False)

isLegal _ (PPiece _ Knight) _ _  = True
isLegal b (PPiece _ King  ) s s' = all (emptySq b) (init $ path s s')

isLegal b (PPiece _ _     ) s s' = all (emptySq b) (init $ path s s')

sqUnderAttack :: Board -> Player -> Square -> Bool
sqUnderAttack b pl s = not $ null (attackers :: [PlayerPiece])
  where attackers = do
          (s',p) <- pcsByPlayer b pl
          st <- basicTrans p
          if s' /+/ st == s && isLegal b p s' s
            then return p
            else []

performPly :: PlayerPiece -> Square -> Square -> Board -> [Board]

performPly (PPiece pl Pawn) s s' b = do
    let b' = setPassant $ movePiece s s' b
    pc' <- if snd s' `elem` [1,8]
      then [Rook,Knight,Bishop,Queen]
      else [Pawn]
    return $ mLift (Map.insert s' (PPiece pl pc')) b'
  where setPassant (Board bm pl' _) = Board bm pl' pass
        pass = case snd (s' |-| s) of
                 2  -> Just $ s' /-/ (0,1)
                 -2 -> Just $ s' /+/ (0,1)
                 _  -> Nothing

performPly _ s s' b = [clearPassant $ movePiece s s' b]
  where clearPassant (Board bm pl' _) = Board bm pl' Nothing

pcsByPlayer :: Board -> Player -> [(Square,PlayerPiece)]
pcsByPlayer b pl = do
  (s,p) <- Map.toList $ bMap b
  if player p == pl
    then return (s,p)
    else []

-- Move piece from square s1 to square s2.
movePiece :: Square -> Square -> Board -> Board
movePiece s s' = turnBoard . insertFrom s s'

insertFrom :: Square -> Square -> Board -> Board
insertFrom s s'@(h',v') b = mLift (delPassant . insertMaybe s' p . Map.delete s) b
  where p = Map.lookup s (bMap b)
        delPassant = case p of
          Just (PPiece _ Pawn) -> if enPassant b == Just s'
                                        then case v' of
                                          3 -> Map.delete (h',4)
                                          6 -> Map.delete (h',5)
                                          _ -> id
                                        else id
          _ -> id

insertMaybe :: Square -> Maybe PlayerPiece -> BoardMap -> BoardMap
insertMaybe s v m = case v of
                      Nothing -> m
                      Just p  -> Map.insert s p m

emptySq :: Board -> Square -> Bool
emptySq b s = isNothing $ bLookup s b

-- Path from one square to another, excluding first square.
path :: Square -> Square -> [Square]
path (x1,y1) (x2,y2) | x1 == x2 && y1 == y2 = []
                     | otherwise = let x' | x2 > x1   = x1 + 1
                                          | x2 < x1   = x1 - 1
                                          | otherwise = x1
                                       y' | y2 > y1   = y1 + 1
                                          | y2 < y1   = y1 - 1
                                          | otherwise = y1
                                   in (x',y') : path (x',y') (x2,y2)

onBoard :: Square -> Bool
onBoard (h,v) | min h v < 1    = False
              | max h v > bDim = False
              | otherwise      = True

-- Basic movements that a piece can make (e.g. a bishop can move diagonally)
basicTrans :: PlayerPiece -> [SquareTrans]
basicTrans (PPiece pl pc) = case pc of
    King     -> [(h,v) | h <- [-1..1], v <- [-1..1], (h,v) /= (0,0)]
    Queen    -> combine [Rook, Bishop]
    Rook     -> [(h,v) | h <- [-8..8], v <- [-8..8], h == 0 || v == 0, (h,v) /= (0,0)]
    Bishop   -> [(h,v) | h <- [-8..8], v <- [-8..8], v == h || h == -v, (h,v) /= (0,0)]
    Knight   -> [(h,v) | h <- [-2,-1,1,2], v <- [-2,-1,1,2], v /= h, v /= -h]
    Pawn -> case pl of
                White -> (0, 2) : [(h, 1) | h <- [-1..1]]
                Black -> (0,-2) : [(h,-1) | h <- [-1..1]]
  where combine = concatMap (basicTrans . PPiece White)

opponent :: Player -> Player
opponent White = Black
opponent Black = White

mLift :: (BoardMap -> BoardMap) -> Board -> Board
mLift f (Board m p e) = Board (f m) p e

occByPl :: Square -> Board -> Bool
occByPl s b = case bLookup s b of
  Just p  -> player p == bPlayer b
  _       -> False

(/+/) :: Square -> SquareTrans -> Square
(h,v) /+/ (ht,vt) = (h',v')
  where h' = fromIntegral $ fromIntegral h + ht
        v' = fromIntegral $ fromIntegral v + vt
infixl 6 /+/

(/-/) :: Square -> SquareTrans -> Square
(h,v) /-/ (h',v') = (ht,vt)
  where ht = fromIntegral $ fromIntegral h - h'
        vt = fromIntegral $ fromIntegral v - v'
infixl 6 /-/

(|-|) :: Square -> Square -> SquareTrans
(h,v) |-| (h',v') = (ht,vt)
  where ht = fromIntegral $ fromIntegral h - h'
        vt = fromIntegral $ fromIntegral v - v'
infixl 6 |-|

initialBoard :: Board
initialBoard = Board bm White Nothing
  where bm = Map.fromList [
                   ((1,1), PPiece White Rook   )
                 , ((2,1), PPiece White Knight )
                 , ((3,1), PPiece White Bishop )
                 , ((4,1), PPiece White Queen  )
                 , ((5,1), PPiece White King   )
                 , ((6,1), PPiece White Bishop )
                 , ((7,1), PPiece White Knight )
                 , ((8,1), PPiece White Rook   )

                 , ((1,2), PPiece White Pawn   )
                 , ((2,2), PPiece White Pawn   )
                 , ((3,2), PPiece White Pawn   )
                 , ((4,2), PPiece White Pawn   )
                 , ((5,2), PPiece White Pawn   )
                 , ((6,2), PPiece White Pawn   )
                 , ((7,2), PPiece White Pawn   )
                 , ((8,2), PPiece White Pawn   )

                 , ((1,7), PPiece Black Pawn   )
                 , ((2,7), PPiece Black Pawn   )
                 , ((3,7), PPiece Black Pawn   )
                 , ((4,7), PPiece Black Pawn   )
                 , ((5,7), PPiece Black Pawn   )
                 , ((6,7), PPiece Black Pawn   )
                 , ((7,7), PPiece Black Pawn   )
                 , ((8,7), PPiece Black Pawn   )

                 , ((1,8), PPiece Black Rook   )
                 , ((2,8), PPiece Black Knight )
                 , ((3,8), PPiece Black Bishop )
                 , ((4,8), PPiece Black Queen  )
                 , ((5,8), PPiece Black King   )
                 , ((6,8), PPiece Black Bishop )
                 , ((7,8), PPiece Black Knight )
                 , ((8,8), PPiece Black Rook   )
                 ]
