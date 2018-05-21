--auther: Weili Yin,912603171. Zheng Xu
--ECS140b, professor: Kurt
import Data.Char
import Data.Ix

type Board = String
type Eboard = (Int,Board)
type Eboards = [Eboard]
data Node = Node{history::[Board],value::Int,children::[Node]} deriving (Eq,Show,Read)


--basic helpful function
findRow::Board->Int
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
notelem list target
  |null list =True
  |head list == target = False
  |otherwise =notelem (tail list) target


--this function will return the evaluation
evalueBoard::Board->Char->Int
evalueBoard board side
  |side=='w' =evBw board
  |otherwise =evBb board
evBw::Board->Int
evBw board
  |or[not (elem 'B' board), (countNum 'b' board) ==0,not (canImove board 'b')] = 1000 --means white wins!
  |or[not (elem 'W' board), (countNum 'w' board) ==0,not (canImove board 'w')] = -1000 --means white loses.
  |otherwise = (countNum 'w' board) - (countNum 'b' board)
evBb::Board->Int
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
canImove::Board->Char->Bool
canImove board s = canImove_h board s 0
canImove_h::Board->Char->Int->Bool
canImove_h board s pos
  |pos >= length board = False--test till last pos, means all fail
  |and[(board !! pos)==s,or (map (isfree board) (getPPos board (findRow board) pos s))] = True--test pawn, inside or[],freespace aviliable
  |and[(board !! pos)==(toUpper s),or (map (isfree board) (getFPos (findRow board) pos))] =True--test flag
  |otherwise = canImove_h board s (pos+1) --this cannot move/or not yours, next

--free func test if a space is empty
isfree::Board->Int->Bool
isfree board pos = (board !! pos)=='-'

getFPos::Int->Int->[Int] --get all aviliable slots for Flag
getFPos row pos = one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row)],
                           and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1)],
                           and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1)],
                           and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row)]] [pos-row,pos-1,pos+1,pos+row]


getPPos::Board->Int->Int->Char->[Int]--get all aviliable slots for pawns
getPPos board row pos side = one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row), side=='b'], --white cannot move up
                                      and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1)],
                                      and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1)],
                                      and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row),side=='w']] [pos-row,pos-1,pos+1,pos+row] --black cannot move down
                                       ++ (getPPos_h board row pos side)
getPPos_h::Board->Int->Int->Char->[Int] -- pawns can jump over oppenents
getPPos_h board row pos side
  =  one2one [and[isSameCol row pos (pos-row), inRange (0,row^2-1) (pos-row),side=='b',or[(board !! (pos-row))==(opp side),(board !! (pos-row))==(toUpper (opp side))],
                  isSameCol row pos (pos-row-row), inRange (0,row^2-1) (pos-row-row)],
              and[isSameRow row pos (pos-1),inRange (0,row^2-1) (pos-1),or[(board !! (pos-1))==(opp side),(board !! (pos-1))==(toUpper (opp side))],
                  isSameRow row pos (pos-2), inRange (0,row^2-1) (pos-2)],
              and[isSameRow row pos (pos+1),inRange (0,row^2-1) (pos+1),or[(board !! (pos+1))==(opp side),(board !! (pos+1))==(toUpper (opp side))],
                  isSameRow row pos (pos+2), inRange (0,row^2-1) (pos+2)],
              and[isSameCol row pos (pos+row),inRange (0,row^2-1) (pos+row),side=='w',or[(board !! (pos+row))==(opp side),(board !! (pos+row))==(toUpper (opp side))],
                  isSameCol row pos (pos+row+row), inRange (0,row^2-1) (pos+row+row)]
             ] [pos-row-row,pos-2,pos+2,pos+row+row]

--capture::[Board]->Char->Int->Board
--capture history side steps =
--  miniMax side steps history

deleteOne::[[Board]]->[[Board]]
deleteOne [] = []
deleteOne list = (drop 1 (head list)):deleteOne (tail list)
evaluall::[[Board]]->Char->[Int]
evaluall histories side
  |null histories = []
  |otherwise      =(evalueBoard  (head (head histories)) side):evaluall (tail histories) side

miniMax::Char->Int->[Board]->[[Board]]
miniMax side steps history = miniMax2 side side steps [history] --now it has all posibilities
miniMax2::Char->Char->Int->[[Board]]->[[Board]]
miniMax2 side whoaction steps result
  |steps== 0  =result
  |otherwise  =miniMax2 side (opp whoaction) (steps-1) (expendRs whoaction result)
--try of data minimax
  miniMax3::[Board]->Char->Int->Node --root node
  miniMax3 history side steps = (makeNode history, findMax(miniMax3_h history side side steps),(miniMax3_h history side side steps))
  miniMax3_h::[Board]->Char->Char->Int->[Node]
  miniMax3_h history side whoaction steps
    |steps==0 = makeNode [],(-1),[]





--Node{history::[Board],value::Int,children::[Node]} deriving (Eq,Show,Read)
makeNode::[Board]->Int->[Node]->Node
makeNode his val nodes= Node{history=his,value=val,children=nodes}





--explore paths
expendRs::Char->[[Board]]->[[Board]]
expendRs side iniRs
  |null iniRs =[]
  |otherwise  = (map (add2head (head iniRs))  (generateNext (head iniRs) side)) ++ (expendRs (opp side) (tail iniRs))

pair::[Int]->[Board]->[Eboard]
pair value list
  |null value =[]
  |otherwise = (head value,head list): pair (tail value) (tail list)

getVal::Eboard->Int
getVal (a,_) = a
getBoard::Eboard->Board
getBoard(_,a)=a
add2head::[String]->String->[String]
add2head history board = [board]++history


generateNext::[Board]->Char->[Board] --generate all possible movements, and reduce history part
generateNext history side = filter (notelem history) (generateNext_h (history !! 0) side 0)
generateNext_h::Board->Char->Int->[Board]
generateNext_h board side pos
  |pos==((findRow board)^2) = []
  |otherwise = (makeMove board side pos) ++ (generateNext_h board side (pos+1))

makeMove::Board->Char->Int->[Board] --it make single move and have some results
makeMove board side pos
  |(board !! pos)==side         =map (moveTo_cap board pos) (filter (isfree board) (getPPos board (findRow board) pos side)) --capture more proiority than move
  |(board !! pos)==toUpper side =map (moveTo board pos) (filter (isfree board) (getFPos (findRow board) pos)) --flag can only move
  |otherwise = []

--Folowing movement NOT make any judgement
moveTo::Board->Int->Int->Board
moveTo board inipos finpos = take finpos (take inipos board ++ ['-'] ++ drop (inipos + 1) board) ++ [(board !! inipos)] ++ drop (finpos+1) (take inipos board ++ ['-'] ++ drop (inipos + 1) board)
eat::Int->Board->Board
eat pos board= take pos board ++ ['-'] ++ drop (pos + 1) board
moveTo_cap::Board->Int->Int->Board
moveTo_cap board inipos finpos --special for dealing with capture 1st:move condition  2nd:jump condition
  |or[abs((getPosCol (findRow board) inipos)-(getPosCol (findRow board) finpos))==1,
      abs((getPosRow (findRow board) inipos)-(getPosRow (findRow board) finpos))==1]  =take finpos (take inipos board ++ ['-'] ++ drop (inipos + 1) board) ++ [(board !! inipos)] ++ drop (finpos+1) (take inipos board ++ ['-'] ++ drop (inipos + 1) board)
  |or[abs((getPosCol (findRow board) inipos)-(getPosCol (findRow board) finpos))==2,
      abs((getPosRow (findRow board) inipos)-(getPosRow (findRow board) finpos))==2]  =eat ((inipos+finpos) `div`  2) (take finpos (take inipos board ++ ['-'] ++ drop (inipos + 1) board) ++ [(board !! inipos)] ++ drop (finpos+1) (take inipos board ++ ['-'] ++ drop (inipos + 1) board))
  |otherwise =[]
