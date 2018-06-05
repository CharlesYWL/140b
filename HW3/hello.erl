-module(hello).
-export([loop/0,do_check/1]).

loop() ->
  try do_check(1) of
		  {ok,Result} -> do_thing(Result);
	  catch
		  {error,ErrReason} -> do_error(ErrReason)
end
end.

do_check(Test) ->
   case Test of
        1 -> erlang:throw({error,to_small});
        2 –> {ok,2};
        9 -> erlang:throw({error,too_big})
end.
