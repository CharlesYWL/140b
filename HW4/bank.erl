-module(bank).
-export([loop/0,createTable/2,insertTable/3]).

loop() ->
  receive
    {create,List,TableName} ->
      createTable(List,TableName),
      io:format("bank ~p created\n",[TableName]),
      loop();
    {open,Name,Money,TableName} ->
      insertTable(TableName,Name,Money),
      io:format("new account ~p opened with ~p dollars\n",[Name,Money]),
      loop();
    {balance,Name,TableName} ->
      [{Name,Doller}]=ets:lookup(TableName,Name),
      io:format("account ~p has ~p dollars\n",[Name,Doller]),
      loop();
    {deposit,Name,Cash,BankName} ->
      [{Name,OldBalance}]=ets:lookup(BankName,Name),
%      io:format("~p had OldBalance: ~p\n",[Name,OldBalance]),
      NewBalance=(OldBalance+Cash),
      ets:delete(BankName,Name),
      ets:insert(BankName,{Name,NewBalance}),
      io:format("account ~p now has ~p dollars\n",[Name,NewBalance]),
      loop();
    {withdraw,Name,Cash,BankName} ->
      [{Name,OldBalance}]=ets:lookup(BankName,Name),
      NewBalance=(OldBalance-Cash),
      case(NewBalance >= 0) of
        true ->
          ets:delete(BankName,Name),
          ets:insert(BankName,{Name,NewBalance}),
          io:format("~p dollars withdrawn\n",[Cash]),
          io:format("account ~p now has ~p dollars\n",[Name,NewBalance]),
          true;
        false ->
          io:format("sorry, account ~p has only ~p dollars\n",[Name,OldBalance]),
          false;
end.

createTable(List,TableName) ->
  ets:new(TableName,[named_table,public]),
  lists:map(fun(I) -> ets:insert(TableName,I) end, List).

insertTable(TableName,Name,Money)->
        ets:insert(TableName,{Name,Money}).
