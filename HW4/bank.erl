-module(bank).
-export([loop/0,createTable/2,insertTable/3]).

loop() ->
  process_flag(trap_exit, true),
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
      NewBalance=(OldBalance+Cash),
      ets:delete(BankName,Name),
      ets:insert(BankName,{Name,NewBalance}),
      io:format("account ~p now has ~p dollars\n",[Name,NewBalance]),
      loop();
    {withdraw,Name,Cash,BankName} ->
      [{Name,OldBalance}]=ets:lookup(BankName,Name),
      NewBalance=(OldBalance-Cash),
      case NewBalance >= 0 of
        true ->
          {ets:delete(BankName,Name),
          ets:insert(BankName,{Name,NewBalance}),
          io:format("~p dollars withdrawn\n",[Cash]),
          io:format("account ~p now has ~p dollars\n",[Name,NewBalance]),
          loop()};
        false ->
          io:format("sorry, account ~p has only ~p dollars\n",[Name,OldBalance]),
%          exit(insufficient_balance),
%          io:format("still work after throw",[]),
          loop()
      end
end.

createTable(List,TableName) ->
  ets:new(TableName,[named_table,public]),
  lists:map(fun(I) -> ets:insert(TableName,I) end, List).

insertTable(TableName,Name,Money)->
        ets:insert(TableName,{Name,Money}).
