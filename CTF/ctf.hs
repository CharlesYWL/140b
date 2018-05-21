import Data.Char
import Data.Ix
--capture::[String]->Char->Int->String
--capture l c n

--basic helpful function
findRow::String->Int
findRow l = toInt (sqrt (fromIntegral  (length l)))
getPosCol::Int->Int->Int
getPosCol row pos = mod pos row
getPosRow::Int->Int->Int
getPosRow row pos = quot pos row
isSameRow::Int->Int->Int->Bool
isSameRow row pos pos2 = getPosRow row pos==getPosRow row pos2
isSameCol::Int->Int->Int->Bool
isSameCol row pos pos2 = getPosCol row pos==getPosCol row pos2
toInt :: Float -> Int
toInt x = round x
opp::Char->Char --find oppenents
opp s =if s=='w' then 'b' else 'w'
one2one::[Bool]->[Int]->[Int] -- Ex. [T,F] [1,2] -> [1]
one2one bl list
  |null bl = []
  |head bl = head list:(one2one (tail bl) (tail list))
  |otherwise = one2one (tail bl) (tail list)

--this function will return the evaluation
--evalueBoard::String->Char->Int
--evalueBoard board side
--  |side=='w' =evBw board
--  |otherwise =evBb board
evBw::String->Int
evBw board
  |or[not (elem 'B' board), (countNum 'b' board) ==0,not (canImove board 'b')] = 1000 --means white wins!
  |or[not (elem 'W' board), (countNum 'w' board) ==0,not (canImove board 'w')] = -1000 --means white loses.
  |otherwise = (countNum 'w' board) - (countNum 'b' board)
evBb::String->Int
evBb board
  |or[not (elem 'W' board), (countNum 'w' board) ==0,not (canImove board 'w')] = 1000 --means white wins!
  |or[not (elem 'B' board), (countNum 'b' board) ==0,not (canImove board 'b')] = -1000 --means white loses.
  |otherwise = (countNum 'b' board) - (countNum 'w' board)

--countNum will count how many elem of target in list
countNum::Char->String->Int
countNum _ [] = 0
countNum c l
  |c== head l = 1 + countNum c (tail l)
  |otherwise = countNum c (tail l)
canImove::String->Char->Bool
canImove board s = canImove_h board s 0
canImove_h::String->Char->Int->Bool
canImove_h board s pos
  |pos >= length board = False--test till last pos, means all fail
  |and[(board !! pos)==s,or (map (isfree board) (getPPos board (findRow board) pos s))] = True--test pawn, inside or[],freespace aviliable
  |and[(board !! pos)==(toUpper s),or (map (isfree board) (getFPos (findRow board) pos))] =True--test flag
  |otherwise = canImove_h board s (pos+1) --this cannot move/or not yours, next

--free func test if a space is empty
isfree::String->Int->Bool
isfree board pos = (board !! pos)=='-'

getFPos::Int->Int->[Int] --get aviliable slots for Flag
getFPos row pos = one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row)],
                           and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1)],
                           and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1)],
                           and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row)]] [pos-row,pos-1,pos+1,pos+row]


getPPos::String->Int->Int->Char->[Int]--get aviliable slots for pawns
getPPos board row pos side = one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row), side=='b'], --white cannot move up
                                      and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1)],
                                      and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1)],
                                      and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row),side=='w']] [pos-row,pos-1,pos+1,pos+row] --black cannot move down
                                       ++ (getPPos_h board row pos side)
getPPos_h::String->Int->Int->Char->[Int] -- pawns can jump over oppenents
getPPos_h board row pos side
  =  one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row),side=='b',(board !! (pos-row))==(opp side),
                  isSameCol row pos (pos-row-row), inRange (0,row^2-1) (pos-row-row)],
              and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1),(board !! (pos-1))==(opp side),
                  isSameRow row pos (pos-2), inRange (0,row^2-1) (pos-2)],
              and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1),(board !! (pos+1))==(opp side),
                  isSameRow row pos (pos+2), inRange (0,row^2-1) (pos+2)],
              and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row),side=='w',(board !! (pos+row))==(opp side),
                  isSameCol row pos (pos+row+row), inRange (0,row^2-1) (pos+row+row)]
             ] [pos-row-row,pos-2,pos+2,pos+row+row]
