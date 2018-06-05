-module(atm).
-export([loop/0,getBalance/2]).

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

    {balance,Name,AtmName} ->
      [{pid,Pid}] = ets:lookup(AtmName,pid),
      [{bankname,BankName}]= ets:lookup(AtmName,bankname),
      Pid ! {balance,Name,BankName},
      loop();
    {withdraw,Name,Cash,AtmName} ->
      [{pid,Pid}] = ets:lookup(AtmName,pid),
      [{bankname,BankName}]= ets:lookup(AtmName,bankname),
      [{cash,NowCash}]= ets:lookup(AtmName,cash),
      OldBalance = getBalance(BankName,Name),
      case NowCash >= Cash of
        true ->
            Pid ! {withdraw,Name,Cash,BankName},
            case Cash >= OldBalance of
                true -> loop();
                false ->
                    ets:delete(AtmName,cash),
                    ets:insert(AtmName,{cash,NowCash-Cash}),
                    loop()
            end;
        false ->
          io:format("sorry, insufficient cash in this atm\n",[]),
          loop()
      end
end.

getBalance(BankName,Name) ->
    [{Name,Balance}]=ets:lookup(BankName,Name),
    Balance.
