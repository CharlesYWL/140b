-module(atm).
-export([loop/0]).

loop()->
  receive
    {start,Pid,BankName,Cash,AtmName} ->
      ets:new(AtmName,[named_table,public]),
      ets:insert(AtmName,{cash,Cash}),
      ets:insert(AtmName,{bankname,BankName}),
      ets:insert(AtmName,{pid,Pid}),
      io:format("atm ~p started with ~p dollars cash available\n",[AtmName,Cash]),
      loop();
    {cashsupply,AtmName} ->
      [{cash,Cash}]= ets:lookup(AtmName,cash),
      io:format("atm ~p has ~p dollars on hand\n",[AtmName,Cash]),
      loop();
    {deposit,Name,Cash,AtmName} ->
      [{pid,Pid}] = ets:lookup(AtmName,pid),
      [{bankname,BankName}]= ets:lookup(AtmName,bankname),
      Pid ! {deposit,Name,Cash,BankName},
      loop();
%    {withdraw,Name,Cash,AtmName}->
    {balance,Name,AtmName} ->
      [{pid,Pid}] = ets:lookup(AtmName,pid),
      [{bankname,BankName}]= ets:lookup(AtmName,bankname),
      Pid ! {balance,Name,BankName},
      loop();
    {withdraw,Name,Cash,AtmName} ->
      [{pid,Pid}] = ets:lookup(AtmName,pid),
      [{bankname,BankName}]= ets:lookup(AtmName,bankname),
      [{cash,NowCash}]= ets:lookup(AtmName,cash),
      case (NowCash>=Cash) of
        true ->
          case (Pid ! {withdraw,Name,Cash,BankName}) of
            true ->
            false ->
        false ->
          io:format("sorry, insufficient cash in this atm",[]),
          loop()      
end.
