%HW1 for ECS140B, professor: Kurt
%authr: Weili Yin,xlyin@ucdavis.edu,912603171
%published on github
%it varify if row/col/suqare have same number&indomain
valid([]).
valid([H|T]):-
    indomain(H),diff(H),valid(T).
%check if Head is atom of the rest list
diff([]).
diff([H|T]):- not(member(H,T)),diff(T).
%check every atom sets in 1~9
indomain([]).
indomain([H|T]):- between(1,9,H) ,indomain(T).
%given by professor
printsudoku([]).
printsudoku([H|T]) :-write(H),nl,printsudoku(T).

sudoku(Puzzle):-
    %this is how we define the Puzzle
    Puzzle = [[S11,S12,S13,S14,S15,S16,S17,S18,S19],
              [S21,S22,S23,S24,S25,S26,S27,S28,S29],
              [S31,S32,S33,S34,S35,S36,S37,S38,S39],
              [S41,S42,S43,S44,S45,S46,S47,S48,S49],
              [S51,S52,S53,S54,S55,S56,S57,S58,S59],
              [S61,S62,S63,S64,S65,S66,S67,S68,S69],
              [S71,S72,S73,S74,S75,S76,S77,S78,S79],
              [S81,S82,S83,S84,S85,S86,S87,S88,S89],
              [S91,S92,S93,S94,S95,S96,S97,S98,S99]],

    Row1 = [S11,S12,S13,S14,S15,S16,S17,S18,S19],
    Row2 = [S21,S22,S23,S24,S25,S26,S27,S28,S29],
    Row3 = [S31,S32,S33,S34,S35,S36,S37,S38,S39],
    Row4 = [S41,S42,S43,S44,S45,S46,S47,S48,S49],
    Row5 = [S51,S52,S53,S54,S55,S56,S57,S58,S59],
    Row6 = [S61,S62,S63,S64,S65,S66,S67,S68,S69],
    Row7 = [S71,S72,S73,S74,S75,S76,S77,S78,S79],
    Row8 = [S81,S82,S83,S84,S85,S86,S87,S88,S89],
    Row9 = [S91,S92,S93,S94,S95,S96,S97,S98,S99],

    Col1 = [S11,S21,S31,S41,S51,S61,S71,S81,S91],
    Col2 = [S12,S22,S32,S42,S52,S62,S72,S82,S92],
    Col3 = [S13,S23,S33,S43,S53,S63,S73,S83,S93],
    Col4 = [S14,S24,S34,S44,S54,S64,S74,S84,S94],
    Col5 = [S15,S25,S35,S45,S55,S65,S75,S85,S95],
    Col6 = [S16,S26,S36,S46,S56,S66,S76,S86,S96],
    Col7 = [S17,S27,S37,S47,S57,S67,S77,S87,S97],
    Col8 = [S18,S28,S38,S48,S58,S68,S78,S88,S98],
    Col9 = [S19,S29,S39,S49,S59,S69,S79,S89,S99],

    Square1 = [S11,S12,S13,S21,S22,S23,S31,S32,S33],
    Square2 = [S14,S15,S16,S24,S25,S26,S34,S35,S36],
    Square3 = [S17,S18,S19,S27,S28,S29,S37,S38,S39],
    Square4 = [S41,S42,S43,S51,S52,S53,S61,S62,S63],
    Square5 = [S44,S45,S46,S54,S55,S56,S64,S65,S66],
    Square6 = [S47,S48,S49,S57,S58,S59,S67,S68,S69],
    Square7 = [S71,S72,S73,S81,S82,S83,S91,S92,S93],
    Square8 = [S74,S75,S76,S84,S85,S86,S94,S95,S96],
    Square9 = [S77,S78,S79,S87,S88,S89,S97,S98,S99],
    valid([Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9,
         Col1,Col2,Col3,Col4,Col5,Col6,Col7,Col8,Col9,
         Square1,Square2,Square3,Square4,Square5,Square6,Square7,Square8,Square9]).
test :-test0, nl,test0a, nl,test0b, nl,test0c.
test0 :-L = [[9,6,3,1,7,4,2,5,8],[1,7,8,3,2,5,6,4,9],[2,5,4,6,8,9,7,3,1],[8,2,1,4,3,7,5,9,6],[4,9,6,8,5,2,3,1,7],[7,3,5,9,6,1,8,2,4],[5,8,9,7,1,3,4,6,2],[3,1,7,2,4,6,9,8,5],[6,4,2,5,9,8,1,7,3]],
        sudoku(L),printsudoku(L).
test0a :-L = [[9,_,3,1,7,4,2,5,8],[_,7,_,3,2,5,6,4,9],[2,5,4,6,8,9,7,3,1],[8,2,1,4,3,7,5,_,6],[4,9,6,8,5,2,3,1,7],[7,3,_,9,6,1,8,2,4],[5,8,9,7,1,3,4,6,2],[3,1,7,2,4,6,9,8,5],[6,4,2,5,9,8,1,7,3]],
        sudoku(L),printsudoku(L).
test0b :-L = [[9,_,3,1,7,4,2,5,_],[_,7,_,3,2,5,6,4,9],[2,5,4,6,_,9,_,3,1],[_,2,1,4,3,_,5,_,6],[4,9,_,8,_,2,3,1,_],[_,3,_,9,6,_,8,2,_],[5,8,9,7,1,3,4,6,2],[_,1,7,2,_,6,_,8,5],[6,4,2,5,9,8,1,7,3]],
        sudoku(L),printsudoku(L).
test0c :-L = [[_,9,3,1,7,4,2,5,8],[_,7,_,3,2,5,6,4,9],[2,5,4,6,8,9,7,3,1],[8,2,1,4,3,7,5,_,6],[4,9,6,8,5,2,3,1,7],[7,3,_,9,6,1,8,2,4],[5,8,9,7,1,3,4,6,2],[3,1,7,2,4,6,9,8,5],[6,4,2,5,9,8,1,7,3]],
        sudoku(L),printsudoku(L).
test0d :-L = [[9,_,3,1,_,4,2,5,_],[_,7,_,3,2,5,6,4,9],[2,5,4,6,_,9,_,3,1],[_,2,1,4,3,_,5,_,6],[4,9,_,8,_,2,3,1,_],[_,3,_,9,6,_,8,2,_],[5,8,9,7,1,3,4,6,2],[_,1,7,2,_,6,_,8,5],[6,4,2,5,_,8,1,7,3]],
        sudoku(L),printsudoku(L).

test1 :-L = [[_,6,_,1,_,4,_,5,_],[_,_,8,3,_,5,6,_,_],[2,_,_,_,_,_,_,_,1],[8,_,_,4,_,7,_,_,6],[_,_,6,_,_,_,3,_,_],[7,_,_,9,_,1,_,_,4],[5,_,_,_,_,_,_,_,2],[_,_,7,2,_,6,9,_,_],[_,4,_,5,_,8,_,7,_]],
        sudoku(L),printsudoku(L).
