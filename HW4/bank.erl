-module(bank).
-export([loop/0]).

loop() ->
  receive
    {create,List,TableName} ->
      ets:new(TableName,[named_table,public]),
      lists:map(fun(I) -> ets:insert(TableName,I) end, List),
      io:format("bank ~p created\n",[TableName]),
      loop();
    {open,Name,Money,TableName} ->
      ets:insert(TableName,{Name,Money}),
      io:format("new account ~p opened with ~p dollars\n",[Name,Money]),
      loop();
    {balance,Name,TableName} ->
      [{Name,Doller}]=ets:lookup(TableName,Name),
      io:format("account ~p has ~p dollars\n",[Name,Doller]),
      loop()
end.
