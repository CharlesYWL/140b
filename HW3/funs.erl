-module(funs).
-export([myremoveduplicates/1,myintersection/2,mylast/1,myreverse/1,rev/2,myreplaceall/3]).
%%author: Weili Yin 912603171 for ECS140b
%%published on GitHuB CharlesYWL
myremoveduplicates([]) -> [];
myremoveduplicates([H|T]) ->
case (lists:member(H,T)==true) of
    true->    myremoveduplicates(T);
    false -> [H|myremoveduplicates(T)]
end.

myintersection([],_) -> [];
myintersection([H|T],List) ->
case(lists:member(H,List)==true) of
    true -> [H|myintersection(T,List)];
    false -> myintersection(T,List)
end.

mylast([]) -> [];
mylast([H|T]) ->
case (T==[]) of
    true -> [H];
    false -> mylast(T)
end.
myreverse(L) ->
    rev(L,[]).
rev([], L) -> L;
rev([X|Xs],L) ->
    rev (Xs,[X|L]).
myreplaceall(_,_,[]) -> [];
myreplaceall(Num,Target,[Target|Tail]) ->
    [Num|myreplaceall(Num,Target,Tail)];
myreplaceall(Num,Target,[Other|Tail]) ->
    [Other|myreplaceall(Num,Target,Tail)].
