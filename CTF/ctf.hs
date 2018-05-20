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
  |not (elem 'B' board) = 1000 --means white wins!
  |not (elem 'W' board) = -1000 --means white loses.
  |canImove board 'b'   = 1000 --if opponent cannot move on next turn
  |otherwise = (countNum 'w' board) - (countNum 'b' board)

--countNum will count how many elem of target in list
countNum::Char->String->Int
countNum _ [] = 0
countNum c l
  |c== head l = 1 + countNum c (tail l)
  |otherwise = countNum c (tail l)
