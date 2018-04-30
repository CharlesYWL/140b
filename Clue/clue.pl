%Clue for ECS140B, author:Weili Yin 912603171, Zheng Xu 912970419
%publish on github
%professor: Kurt
%dynamic setting
:- dynamic player_num/1.
:- dynamic user_name/1.
:- dynamic player_name/1.
:- dynamic player_list/1.
:- dynamic weapon/1.
:- dynamic weapon_list/1.
:- dynamic room/1.
:- dynamic room_list/1.
%every preset here
player_list([mustard,scarlet,plum,green,peacock]).
inrange(3).
inrange(4).
inrange(5).
inrange(6).

%type startgame. to startgame
startgame:- initial.

%get everything initialed
initial:- write("Welcome to Clue.\n Let's startgame\n How many players?(3-6) end with '.'\n"),
  getPlayer_Num(PN),assert(player_num(PN)),
  getPlayer_Name(PNa),assert(user_name(PNa)),
  makePlayerList(PN,PL),retract(player_list(_)),assert(player_list(PL)).

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
makePlayerList(0,[]).
makePlayerList(1,[UN|PL]):- user_name(UN),makePlayerList(0,PL).
makePlayerList(Num,[H|T]):- retractall(player_list([H|TailofPL])),
  assert(player_list(TailofPL)),makePlayerList(Num-1,T).

%randomly choose
choose([], []).
choose(List, Ele) :-
  length(List, Length),
  random(0, Length, Index),
  nth0(Index, List, Ele).

%roll  dices
roll(Rs):- random(R), Rs is round(1+5*R).
rolltwo(Rs):- roll(A),roll(B),Rs is A+B.
