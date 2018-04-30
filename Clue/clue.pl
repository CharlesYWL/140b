%Clue for ECS140B, author:Weili Yin 912603171, Zheng Xu 912970419
%publish on github
%professor: Kurt
%dynamic setting, list contiains all, nonlist only ture if it is not the answer
:- dynamic player_num/1.
:- dynamic user_name/1.
:- dynamic player_name/1.
:- dynamic player_list/1.%it has order information
:- dynamic weapon/1.
:- dynamic weapon_list/1.
:- dynamic room/1.
:- dynamic room_list/1.
:- dynamic answer/3. %final answer
:- dynamic card_list/1. %represent cards player can have,it's a list
:- dynamic card/2. %single card and who hold it.
:- dynamic pos/2.%position
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
  makeAnswer(PLafterRandom,RL,WL),
  initialPos(PLafterRandom).

dealing:- write("deal reaming cards\n"),
  findall(X,(player_name(X);room(X);weapon(X)),L),
  random_permutation(L,LafterRandom),assert(card_list(LafterRandom)),
  addCard(LafterRandom,0),write("dealing finished\n").

interface:- write("The order is:\n"),player_num(PN),IndexMax is PN-1,
  numlist(0,IndexMax,L),printOrder(L),write("\n"),
  player_list(PL),printOrder(PL),write("\n"),
  printYourCard,
  playerTurn(0).%player's turn, if its bot, randomly choose,start on 0

  printOrder([]):- write("\n").
  printOrder([H|T]):- write(H),write("\t"),printOrder(T).
  printYourCard:- user_name(UN),player_list(List),
    nth0(Index,List,UN),findall(Y,(card(Y,Index)),YourCard),
    write("You have cards:\n"),printOrder(YourCard),write("\n").

  playerTurn(MaxNum):-player_num(MaxNum),playerTurn(0).
  playerTurn(Num):-user_name(UN),player_list(List),nth0(Num,List,NthPN),NextNum is Num+1,
    ( UN=NthPN ->
      (yourturn(Num),playerTurn(NextNum)); %if Username=Nth palyername
      botturn(Num),playerTurn(NextNum)). %else

  %to simplify, you can move one to another in one turn
  yourturn(Num):- write("It's Your turn now,You can move from one room to antoher in one turn(or stay the same room)\n"),
    showMap,write("Which room you want to move?\n(you can only suggest or accusate events happening in that room)\n"),makeSuggestion(Num).


  %not yet finished
  botturn(Num):- player_list(List),nth0(Num,List,NthPN),write("now it's "),write(NthPN),write(" turn.\n").
  %suggestion1: move to a new room
  makeSuggestion(Num):- player_list(List),nth0(Num, List, PN),read(Room_name),
    (checkroom(PN,Room_name) ->
      (retract(pos(PN,_)),assert(pos(PN,Room_name))),makeSuggestion2(Room_name,Num);
      write("Wrong inupt,please try again:\n"),makeSuggestion(Num)).
  %suggestion2: after moving, suggest or accusation
  makeSuggestion2(Room_name,Num):-write("s for suggestion, a for accusation\n"),
    read(Input),
    (Input='s' ->
      makeSuggestion3(Room_name,Num);
      (Input='a' ->
        makeAccusation(Room_name,Num);
        write("Invalid Input\n"),makeSuggestion2(Room_name,Num))).
  %suggestion3: suggest anyone and gather information.
  makeSuggestion3(Room_name,Num):- write("Here is weapon list: "),weapon_list(WL),printOrder(WL),
    player_list(PL),write("Here is the player list: "),printOrder(PL),
    write("who and what weapon you want to suggest to next player?\nin [who,weapon]. format\n"),
    read(Suggestion),
    (checkSuggestion(Suggestion) ->
      getFeedback(Suggestion,Room_name,Num);
    write("Invalid Input,try again.\n"),makeSuggestion3(Room_name,Num)).

  %make accusation, correct win,wrong->lose
  makeAccusation(Room_name,Num):- write("Here is weapon list: "),weapon_list(WL),printOrder(WL),
    player_list(PL),write("Here is the player list: "),printOrder(PL),
    write("Here is room list: "),room_list(RL),printOrder(RL),
    write("Enter [Who,Weapon,Where]\n"),
    read(Accusation),
    (checkAccusation(Accusation) ->
      checkAccCorrect(Accusation);
      write("Invalid Input,try again\n"),makeAccusation(Room_name,Num)).

  getFeedback([Who,Weapon],Room_name,Num):- NextNum is Num+1,
    (player_num(NextNum) ->
      (findall(CardName,card(CardName,0),List),checkSugCorrect([Who,Weapon,Room_name],List,0) );%index leak, go ask #0
      (findall(CardName,card(CardName,NextNum),List),checkSugCorrect([Who,Weapon,Room_name],List,NextNum))).

  %1:stay in the same room;2:move to empty room
  checkroom(PN,Room_name):- room_list(RL),member(Room_name,RL),pos(PN,Room_name).
  checkroom(PN,Room_name):- room_list(RL),member(Room_name,RL),not(pos(_,Room_name)).
  checkSuggestion([Who,Weapon]):- player_list(PL),weapon_list(WL),member(Who,PL),member(Weapon,WL).
  checkSugCorrect([Who,Weapon,Room_name],List,Num):- player_list(PL),nth0(Num, PL, PN),
    (member(Who,List) ->
      write(Who),write(" is in "),write(PN),write("'s hand\n");
      (member(Weapon,List) ->
        write(Weapon),write(" is in "),write(PN),write("'s hand\n");
        (member(Room_name,List) ->
          write(Room_name),write(" is in "),write(PN),write("'s hand\n");
            write("None of suggestion matched\n")))).
  checkAccusation([Who,Weapon,Where]):-player_list(PL),weapon_list(WL),room_list(RL),member(Who,PL),member(Weapon,WL),member(Where,RL).
  checkAccCorrect([Who,Weapon,Where]):-
    (answer(Who,Where,Weapon) ->
      write("AWESOME! YOU FOUND THE SUSPECT: "),printOrder([Who,Weapon,Where]),write("tpye any+'.' to quit"),read(T),halt();
      write("SORRY, YOU LOSE THE CHANCE TO WIN\n"),write("tpye any+'.' to quit"),read(T),halt()).
  showMap:- write("\nStudy\tHall\tLonguge\n\nLibrary\n\nBilliard\tBegin\tDining\n\nConservatory\tBall\tKitchen\n\n"),
    write("Suspects'position:\n"),player_list(PL),printOrder(PL),write("\n"),
    findPos(PL,Rs),printOrder(Rs),write("\n").
  findPos([],[]).
  findPos([H|T],[Place|NextTail]):- pos(H,Place),findPos(T,NextTail).
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
  assert(answer(A_Player,A_Room,A_Weapon)),
  write(A_Player),write(A_Room),write(A_Weapon).%for test use

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

initialPos([]).
initialPos([H|T]):- assert(pos(H,begin)),initialPos(T).

%roll  dices
%roll(Rs):- random(R), Rs is round(1+5*R).
%rolltwo(Rs):- roll(A),roll(B),Rs is A+B.
