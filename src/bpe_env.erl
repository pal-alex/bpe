-module(bpe_env).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-export([append/3,append/4,append/5,find/3,remove/3]).

append(kvs, Feed, Rec) ->
  kvs:append(Rec, Feed);

append(env, Proc, Rec) ->
  Feed = "/bpe/proc",
  S = case find(env, Proc, Rec) of
      {[],Rest} -> Proc#process{docs = [Rec|Rest]};
      {Found,Rest} -> Proc#process{docs = [Rec|Found]++Rest}
  end,
  kvs:append(S, Feed),
  S.

append(env, Proc, Key, Value) -> append(env, Proc, Key, Value, false).
append(env, Proc, Key, Value, Modify) ->
  Feed = "/bpe/proc",
  {Found, Rest} = find(env, Proc, Key),
  Docs = case {Found, Modify} of
             {{Key, [_H|T]}, true} -> [{Key, [Value|T]}|Rest];
             {{Key, List}, false} -> [{Key, [Value|List]}|Rest]; 
                          {[], _} -> [{Key, [Value]}|Rest]
              
        end,
  S = Proc#process{docs = Docs},
  kvs:append(S, Feed),
  S.


find(Rec, Feed) when is_atom(Rec) ->
    lists:partition(fun (R) -> (element(1,R) == Rec) end, Feed)
;

find(Rec,Feed) ->
  Zip = [ {X,Y} || {X,Y} <- lists:zip(lists:seq(1,size(Rec)),
                                      tuple_to_list(Rec)),Y/=[]],
  lists:partition(fun (R) ->
    lists:foldl(fun ({P,X},A) -> A andalso (element(P,R) == X)
    end, true, Zip)
  end, Feed).

find(kvs,Feed,Rec) ->
  find(Rec,kvs:all(Feed));

find(env,Proc,Rec) ->
  find(Rec,Proc#process.docs).

remove(kvs, Feed, Rec) ->
  {X, _Y} = find(kvs, Feed, Rec),
  lists:map(fun(I) -> kvs:delete(Feed,element(2,I)) end, X);

remove(env,Proc,Rec) ->
  {_X, Y} = find(env, Proc, Rec),
  S=Proc#process{docs=Y},
  kvs:append(S, "/bpe/proc"),
  S.