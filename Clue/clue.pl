%Clue for ECS140B, author:Weili Yin 912603171, Zheng Xu 912970419
%publish on github
%professor: Kurt
%dynamic setting, list contiains all, nonlist only ture if it is not the answer
:- dynamic player_num/1.
:- dynamic user_name/1.
:- dynamic player_name/1.
:- dynamic player_list/1.
:- dynamic weapon/1.
:- dynamic weapon_list/1.
:- dynamic room/1.
:- dynamic room_list/1.
:- dynamic answer/3. %final answer
:- dynamic card_list/1. %represent cards player can have,it's a list
:- dynamic card/2. %single card and who hold it.
%every preset here
player_list([mustard,scarlet,plum,green,peacock]).
room_list([study,hall,lounge,dining,kitchen,ball,conservatory,billiard,library]).
weapon_list([rope,lead_pipe,knife,wrench,candlestick,pistol]).
inrange(3).
inrange(4).
inrange(5).
inrange(6).

%type startgame. to startgame
startgame:-
  initial,%initial everything
  dealing,%give everyone what they have
  interface.
%get everything initialed
initial:- write("Welcome to Clue.\n Let's startgame\n How many players?(3-6) end with '.'\n"),
  getPlayer_Num(PN),assert(player_num(PN)),
  getPlayer_Name(PNa),assert(user_name(PNa)),
  makePlayerList(PN,PL),retract(player_list(_)),
  random_permutation(PL,PLafterRandom),assert(player_list(PLafterRandom)),
  addPlayer(PLafterRandom),
  room_list(RL),addRoom(RL),
  weapon_list(WL),addWeapon(WL),
  makeAnswer(PLafterRandom,RL,WL).

dealing:- write("deal reaming cards\n"),
  findall(X,(player_name(X);room(X);weapon(X)),L),
  random_permutation(L,LafterRandom),assert(card_list(LafterRandom)),
  addCard(LafterRandom,0),write("dealing finished\n").

interface:- write("The order is:\t"),player_list(PL),printOrder(PL),write("\n"),
  

  printOrder([]).
  printOrder([H|T]):- write(H),write("\t"),printOrder(T).

%read player number from user and check it
getPlayer_Num(Player_Num):-
  read(Value),
  (checkPN(Value) ->
    Player_Num = Value;
    write("Invalid number,please enter number[3-6]."),getPlayer_Num(Player_Num)).
checkPN(Player_Num):- inrange(Player_Num).

%intput username
getPlayer_Name(PNa):-
  write("Please Enter Your Name:(lower case only)\n"),
  read(PNa).

%all player from a list of player
makePlayerList(Num,[UN|Rs]):-
  NextNum is Num-1,takefirst(NextNum,PrePL,Rs),
  player_list(PrePL),user_name(UN).

  %take first n ele in list.
  takefirst(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
  takefirst(_, [], []).
  takefirst(N, [X|Xs], [X|Ys]) :- M is N-1, takefirst(M, Xs, Ys).
%make answer and remove facts for answer parts.
makeAnswer(PLafterRandom,RL,WL):-
  choose(PLafterRandom,A_Player),choose(RL,A_Room),choose(WL,A_Weapon),
  retract(player_name(A_Player)),
  retract(room(A_Room)),
  retract(weapon(A_Weapon)),
  assert(answer(A_Player,A_Room,A_Weapon)).

addPlayer([]).
addPlayer([H|T]) :- assert(player_name(H)),addPlayer(T).

addRoom([]).
addRoom([H|T]) :- assert(room(H)),addRoom(T).

addWeapon([]).
addWeapon([H|T]) :- assert(weapon(H)),addWeapon(T).

addCard([],_).%no card to add
addCard([H|T],MaxNum):- player_num(MaxNum),addCard([H|T],0).%reach Max, return to 0th
addCard([H|T],Num):- assert(card(H,Num)),NextNum is Num+1,addCard(T,NextNum).

%randomly choose
choose([], []).
choose(List, Ele) :-
  length(List, Length),
  random(0, Length, Index),
  nth0(Index, List, Ele).

%roll  dices
roll(Rs):- random(R), Rs is round(1+5*R).
rolltwo(Rs):- roll(A),roll(B),Rs is A+B.
