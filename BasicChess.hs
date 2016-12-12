module BasicChess ( Player(..)
                  , Board(..)
                  , PlayerPiece(..)
                  , Figurine(..)
                  , Square(..)
                  , initialBoard
                  , legalPlys
                  ) where

import Data.Int (Int8)
import Data.Word (Word8)
import Data.Maybe (isNothing)
import Control.Monad (guard)
import qualified Data.Map as Map

data Player = White | Black deriving (Show, Eq)

data Figurine = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show,Eq)

data PlayerPiece = PPiece { player :: Player, figurine :: Figurine }
                   deriving (Show,Eq)

data Board = Board { bMap :: BoardMap
                   , bPlayer :: Player
                   , enPassant :: Maybe Square
                   , castling :: ((Bool,Bool),(Bool,Bool))
                   } deriving (Eq,Show)

data Square = Sq { sqRank :: Word8, sqFile :: Word8 } deriving (Show,Eq,Ord)

onBoard :: Square -> Bool
onBoard (Sq r f) = r >= 1 && f >= 1 && r <= 8 && f <= 8

type Move = (Int8,Int8)

type BoardMap = Map.Map Square PlayerPiece

turnBoard :: Board -> Board
turnBoard b = b { bPlayer = opponent $ bPlayer b }

bLookup :: Square -> Board -> Maybe PlayerPiece
bLookup s b = Map.lookup s $ bMap b

legalPlys :: Board -> [Board]
legalPlys b = do
  let pl = bPlayer b
  (s,p) <- pcsByPlayer pl b
  st    <- basicTrans p
  s'    <- maybe [] return (s /+/ st)
  guard $  (not . playerOnSq pl s') b && isLegal p s s' b
  b'    <- performPly p s s' b
  guard $  not $ sqUnderAttack (opponent pl) (kingSq pl b') b'
  return b'

kingSq :: Player -> Board -> Square
kingSq pl b = (fst . head . Map.toList . Map.filter f) $ bMap b
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
isLegal :: PlayerPiece -> Square -> Square -> Board -> Bool

isLegal (PPiece pl Pawn) s@(Sq h v) s'@(Sq h' v') b =
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

isLegal (PPiece _ Knight) _ _ _ = True

isLegal (PPiece pl King) s s' b =
  case s' |-| s of
    (1,_)  -> all (emptySq b) (init $ path s s')
    (2,0)  -> canCastleLeft  pl b && all (emptySq b) (path s s{sqFile=7})
    (-2,0) -> canCastleRight pl b && all (emptySq b) (path s s{sqFile=2})
    _      -> False
  where canCastleLeft  White = fst . fst . castling
        canCastleLeft  Black = fst . snd . castling
        canCastleRight White = snd . fst . castling
        canCastleRight Black = snd . snd . castling

isLegal (PPiece _ _     ) s s' b = all (emptySq b) (init $ path s s')

sqUnderAttack :: Player -> Square -> Board -> Bool
sqUnderAttack pl s b = not $ null (attackers :: [PlayerPiece])
  where attackers = do
          (s',p) <- pcsByPlayer pl b
          st <- basicTrans p
          guard $ s' /+/ st == Just s && isLegal p s' s b
          return p

performPly :: PlayerPiece -> Square -> Square -> Board -> [Board]

performPly (PPiece pl Pawn) s s' b = do
    let b' = setPassant pass $ movePiece s s' b
    pc' <- if sqFile s' `elem` [1,8]
      then [Rook,Knight,Bishop,Queen]
      else [Pawn]
    return $ mLift (Map.insert s' (PPiece pl pc')) b'
  where pass = case snd (s' |-| s) of
                 2  -> s' /-/ (0,1)
                 -2 -> s' /+/ (0,1)
                 _  -> Nothing

performPly (PPiece pl King) s s' b = [setCastling (False,False) pl $ mvRook $ movePiece s s' b]
  where mvRook = case s of
                   (Sq 5 v) -> 
                     case s' of
                       (Sq 7 _) -> insertFrom (Sq 8 v) (Sq 6 v)
                       (Sq 3 _) -> insertFrom (Sq 1 v) (Sq 4 v)
                       _ -> id
                   _ -> id

performPly (PPiece pl Rook) s@(Sq 1 _) s' b = [setCastling (False,True) pl $ movePiece s s' b]
performPly (PPiece pl Rook) s@(Sq 8 _) s' b = [setCastling (True,False) pl $ movePiece s s' b]

performPly _ s s' b = [movePiece s s' b]

setCastling :: (Bool,Bool) -> Player -> Board -> Board
setCastling o pl b = b { castling = s pl }
  where s White = (wc `pairAnd` o,bc)
        s Black = (wc,bc `pairAnd` o)
        (wc,bc) = castling b
        pairAnd (i,j) (i',j') = (i&&i',j&&j')

setPassant :: Maybe Square -> Board -> Board
setPassant ms b = b { enPassant = ms }

clearPassant :: Board -> Board
clearPassant = setPassant Nothing

pcsByPlayer :: Player -> Board -> [(Square,PlayerPiece)]
pcsByPlayer pl b = do
  (s,p) <- Map.toList $ bMap b
  guard $ player p == pl
  return (s,p)

-- Move piece from square s1 to square s2.
movePiece :: Square -> Square -> Board -> Board
movePiece s s' = clearPassant . turnBoard . insertFrom s s'

insertFrom :: Square -> Square -> Board -> Board
insertFrom s s'@(Sq h' v') b = mLift (delPassant . insertMaybe s' p . Map.delete s) b
  where p = Map.lookup s (bMap b)
        delPassant = case p of
          Just (PPiece _ Pawn) -> if enPassant b == Just s'
                                        then case v' of
                                          3 -> Map.delete (Sq h' 4)
                                          6 -> Map.delete (Sq h' 5)
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
path (Sq x1 y1) (Sq x2 y2) | x1 == x2 && y1 == y2 = []
                           | otherwise = let x' | x2 > x1   = x1 + 1
                                                | x2 < x1   = x1 - 1
                                                | otherwise = x1
                                             y' | y2 > y1   = y1 + 1
                                                | y2 < y1   = y1 - 1
                                                | otherwise = y1
                                         in Sq x' y' : path (Sq x' y') (Sq x2 y2)

-- Basic movements that a piece can make (e.g. a bishop can move diagonally)
basicTrans :: PlayerPiece -> [Move]
basicTrans (PPiece pl pc) = case pc of
    King     -> [(h,v) | h <- [-2..2], v <- [-1..1], (h,v) /= (0,0)]
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
mLift f b = b { bMap = f $ bMap b }

playerOnSq :: Player -> Square -> Board -> Bool
playerOnSq p s b = case bLookup s b of
  Just p' -> player p' == p
  _       -> False

(/+/) :: Square -> Move -> Maybe Square
(Sq r f) /+/ (h,v) | onBoard s' = Just s'
                   | otherwise  = Nothing
                   where s' = Sq r' f'
                         r' = fromIntegral $ fromIntegral r + h
                         f' = fromIntegral $ fromIntegral f + v

(/-/) :: Square -> Move -> Maybe Square
s /-/ (h',v') = s /+/ (-h',-v')

(|-|) :: Square -> Square -> Move
(Sq r f) |-| (Sq r' f') = (h,v)
  where h = fromIntegral $ fromEnum r - fromEnum r'
        v = fromIntegral $ fromEnum f - fromEnum f'

infixl 6 /+/, /-/, |-|

initialBoard :: Board
initialBoard = Board bm White Nothing ((False,False),(False,False))
  where bm = Map.fromList [
                   (Sq 1 1, PPiece White Rook   )
                 , (Sq 2 1, PPiece White Knight )
                 , (Sq 3 1, PPiece White Bishop )
                 , (Sq 4 1, PPiece White Queen  )
                 , (Sq 5 1, PPiece White King   )
                 , (Sq 6 1, PPiece White Bishop )
                 , (Sq 7 1, PPiece White Knight )
                 , (Sq 8 1, PPiece White Rook   )

                 , (Sq 1 2, PPiece White Pawn   )
                 , (Sq 2 2, PPiece White Pawn   )
                 , (Sq 3 2, PPiece White Pawn   )
                 , (Sq 4 2, PPiece White Pawn   )
                 , (Sq 5 2, PPiece White Pawn   )
                 , (Sq 6 2, PPiece White Pawn   )
                 , (Sq 7 2, PPiece White Pawn   )
                 , (Sq 8 2, PPiece White Pawn   )

                 , (Sq 1 7, PPiece Black Pawn   )
                 , (Sq 2 7, PPiece Black Pawn   )
                 , (Sq 3 7, PPiece Black Pawn   )
                 , (Sq 4 7, PPiece Black Pawn   )
                 , (Sq 5 7, PPiece Black Pawn   )
                 , (Sq 6 7, PPiece Black Pawn   )
                 , (Sq 7 7, PPiece Black Pawn   )
                 , (Sq 8 7, PPiece Black Pawn   )

                 , (Sq 1 8, PPiece Black Rook   )
                 , (Sq 2 8, PPiece Black Knight )
                 , (Sq 3 8, PPiece Black Bishop )
                 , (Sq 4 8, PPiece Black Queen  )
                 , (Sq 5 8, PPiece Black King   )
                 , (Sq 6 8, PPiece Black Bishop )
                 , (Sq 7 8, PPiece Black Knight )
                 , (Sq 8 8, PPiece Black Rook   )
                 ]
