-module(bpe_otp).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2,stop/1,init/1]).

stop(_)      -> ok.
opt()        -> [ set, named_table, { keypos, 1 }, public ].
respawn()    -> spawn(fun () -> [ worker(I) || I <- kvs:feed("/bpe/proc")] end).
start(_,_)   -> 
                [ bpe:reload(I) || I <- application:get_env(bpe,procmodules,[bpe, bpe_proc, bpe_task]) ],
                syn:init(), kvs:join(), kvs:ensure(kvs:writer("/bpe/proc")),
                X = supervisor:start_link({local, ?MODULE}, ?MODULE, []), respawn(), X.
init([])     -> [ ets:new(T,opt()) || T <- [ processes ] ],
                { ok, { { one_for_one, 5, 10 }, [] } }.

worker(#process{id=Id}=Proc) ->

%    case bpe:head(Id) of
        % #hist{time = Time} -> worker_do(calendar:time_difference(Time,calendar:local_time()),P);
        % #hist{time = Time} -> worker_do({0, 0}, P);
        % _ -> broken 
%      end

        {_,{_, T}} = bpe:current_task(Id),
        bpe:start(Proc#process{task=T}, [])
;
worker(P) -> io:format("Unknown: ~p~n",[P]).

% worker_do({Days,_Time},_) when Days >= 14 -> skip;
% worker_do({_,_},P) -> bpe:start(P,[]).

