import Data.Char

--capture::[String]->Char->Int->String
--capture l c n

findRow::String->Float
findRow l = sqrt (fromIntegral  (length l))

--this function will return the evaluation
--evalueBoard::String->Char->Int
--evalueBoard board side
--  |side=='w' =evBw board
--  |otherwise =evBb board
evBw::String->Int
evBw board
  |or[not (elem 'B' board), (countNum 'b' board) ==0,not (canImove board 'b')] = 1000 --means white wins!
  |or[not (elem 'W' board), (countNum 'w' board) ==0] = -1000 --means white loses.
  |otherwise = (countNum 'w' board) - (countNum 'b' board)

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
  |and[(board !! pos)==s,or[] ] = True--test pawn, inside or[],freespace aviliable
  |and[(board !! pos)==(toUpper s),or[]] = True--test flag
  |otherwise = canImove_h board s (pos+1) --this cannot move, next
--free func test if a space is empty
isfree::String->Int->Bool
isfree board pos = (board !! pos)=='-'
