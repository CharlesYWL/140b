-module(funs).
-export([myremoveduplicates/1,inter/2]).

myremoveduplicates([]) -> [];
myremoveduplicates([H|T]) ->
case (lists:member(H,T)==true) of
    true->    myremoveduplicates(T);
    false -> [H|myremoveduplicates(T)]
end.

inter
