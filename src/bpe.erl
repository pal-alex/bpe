-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-include("api.hrl").
-export([head/1]).
-compile(export_all).
-define(TIMEOUT, application:get_env(bpe,timeout,60000)).
% -define(TIMEOUT, 600).

load(Id) -> load(Id, []).
load(Id, Def) ->
    case kvs:get("/bpe/proc",Id) of
         {error,_} -> case Def /= [] of
                        true -> io:format("Didn't find bpe/proc with id = ~p, loaded def~n", [Id]), Def
                      end;
         {ok,Proc} -> io:format("load bpe/proc with id = ~p~n", [Id]),
                      {_, {_,T}, D} = current_proc_data(Id),
                      case T of
                          [] -> Proc;
                           _ -> Proc#process{task = T, docs = D} 
                      end
    end
.

cleanup(P) ->
  [ kvs:delete("/bpe/hist",Id) || #hist{id=Id} <- bpe:hist(P) ],
    kvs:delete(writer,"/bpe/hist/" ++ P),
    kvs:delete("/bpe/proc",P)
  .

get_task(ProcId) -> 
    Proc = bpe:load(ProcId),
    Proc#process.task
.
% current_task(Id) ->
%     case bpe:head(Id) of
%          [] -> {0, {task, []}};
%          #hist{id={H,_},task=T} -> {H,T} end.
                    

current_proc_data(Id) -> 
    case bpe:head(Id) of
            [] -> {0, {task, []}, []};
            #hist{id={H,_}, task=T, docs=D} -> {H, T, D} 
    end
.
            


start(Proc0, Options) -> start(Proc0, Options, []).
start(Proc0, Options, Docs) when is_list(Docs) == false -> start(Proc0, Options, [Docs]);
start(Proc0, Options, Docs0) -> 
    Id   = case Proc0#process.id of [] -> kvs:seq([],[]); X -> X end,
    {Hist, T, D} = current_proc_data(Id),
    {Task, Docs} = case Hist of
                    0 -> Key  = "/bpe/hist/" ++ Id,
                         kvs:ensure(#writer{id=Key}),
                         {{task, Proc0#process.beginEvent}, Docs0};
                    _ -> {T, D}
                end,

    Pid  = proplists:get_value(notification,Options,undefined),
    Proc = Proc0#process{id=Id, task=element(2,Task), options=Options, notifications=Pid, started=calendar:local_time(), docs=Docs},

    kvs:append(Proc, "/bpe/proc"),
    % add_hist(Proc),
    % case Hist of
    %     0 -> add_hist(Proc);
    %     _ -> skip
    % end,
    
    Restart = transient,
    Shutdown = ?TIMEOUT,
    ChildSpec = { Id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },
    io:format("start a child with id = ~p~n", [Id]),
    Result = case supervisor:start_child(bpe_otp, ChildSpec) of
                {ok,_} -> {ok,Proc#process.id};
                {ok,_,_} -> {ok,Proc#process.id};
                {error, already_present} -> {ok, Proc#process.id};
                {error, {already_started, _}} -> {ok, Proc#process.id}
                % {error, Error} -> {{error, Error}, Proc#process.id} 
          end,

    % case Hist of
    %         0 -> bpe:add_hist(Proc),
    %              bpe_task:handle_starting_task(Task, Proc);
    %         _ -> skip
    % end,
    Result
.

% finish_created(Proc_Id) ->
%     {Hist, _Task} = current_task(Proc_Id),
%         case Hist of
%                 % a new bp
%                 0 -> bpe:complete('Created', Proc_Id);
%                 _V -> skip
%         end
% .



find_pid(Id) -> bpe:cache({process,Id}).

proc(ProcId)              -> Pid = find_pid(ProcId),
    gen_server:call(Pid,{get},            ?TIMEOUT).

complete(ProcId)          -> complete([], ProcId).
complete(Stage,ProcId)    -> Pid = find_pid(ProcId),
    case Pid of
        undefined -> io:format("undefined Pid for ProcId = ~p~n", [ProcId]),
                     Proc = restore_stage(ProcId),
                     {reply, not_completed, Proc}
            ;
        _ -> gen_server:call(Pid,{complete,Stage}, ?TIMEOUT)
    end
    .
run(ProcId)               -> gen_server:call(find_pid(ProcId),{run},            ?TIMEOUT).
until(ProcId,Task)        -> gen_server:call(find_pid(ProcId),{until,Task},     ?TIMEOUT).
amend(ProcId,Form)        -> gen_server:call(find_pid(ProcId),{amend,Form},     ?TIMEOUT).
amend(ProcId,Form,noflow) -> gen_server:call(find_pid(ProcId),{amend,Form,true},?TIMEOUT).
append_doc(ProcId, Form)  -> gen_server:call(find_pid(ProcId),{append_doc, Form}, ?TIMEOUT).
discard(ProcId,Form)      -> gen_server:call(pid(ProcId),{discard,Form},   ?TIMEOUT).
modify(ProcId,Form,Arg)   -> gen_server:call(pid(ProcId),{modify,Form,Arg},?TIMEOUT).
event(ProcId,Event)       -> gen_server:call(find_pid(ProcId),{event,Event},    ?TIMEOUT).

start_task(ProcId) ->
    Task = get_task(ProcId),
    start_task(ProcId, Task)
.

start_task(ProcId, Task) ->
    Pid = find_pid(ProcId),
    case Pid of
        undefined -> io:format("(start_task) undefined Pid for ProcId = ~p~n", [ProcId]);
        _ -> gen_server:cast(Pid, {start, Task})
    end
.


delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                   lists:member(Task#task.name,Tasks) ] }.

% BPE for now supports only MNESIA and ROCKS backends.

restore_stage(Proc0) when is_tuple(Proc0) ->
    ProcId = Proc0#process.id,
    {_Hist, T, Docs} = current_proc_data(ProcId),
    Task = case T of
                      {task, []} -> {task, Proc0#process.task};
                      _ -> T
                   end,
    Proc = Proc0#process{id=ProcId, task=element(2,Task), started=calendar:local_time(), docs=Docs},
    Proc
;
restore_stage(ProcId) ->
    Proc0 = kvs:get_value('/bpe/proc', ProcId),
    restore_stage(Proc0)
.

head(ProcId) ->
  Feed = "/bpe/hist/" ++ ProcId,
  case kvs:get(writer, Feed) of
        {ok, #writer{count = C} = _W} ->  %io:format("head hist = ~p~n", [W]),
                                     case kvs:get(Feed, {C - 1, ProcId}) of
                                         {ok, X} -> X; 
                                         _ -> [] 
                                     end;
                              _ -> []
end.

 
  

hist(ProcId)   -> kvs:feed("/bpe/hist/" ++ ProcId).
hist(ProcId,N) -> case application:get_env(kvs,dba,kvs_mnesia) of
                       kvs_mnesia -> case kvs:get(hist,{N,ProcId}) of
                                          {ok,Res} -> Res;
                                          {error,_Reason} -> [] end;
                       kvs_rocks  -> case kvs:get("/bpe/hist/" ++ ProcId,{N,ProcId}) of
                                          {ok,Res} -> Res;
                                          {error,_Reason} -> [] end end .

source(Name, Proc) ->
    case [ Task || Task <- events(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #beginEvent{};
         E -> E end.

step(Name, Proc) -> 
    case [ Task || Task <- tasks(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #task{};
         E -> E end.

doc (R, Proc) -> {X, _Y} = bpe_env:find(env, Proc, R), case X of [A] -> A; _ -> X end.
% doc(RecordName, Proc) when is_atom(RecordName) -> doc({RecordName}, Proc);
% doc(Rec, Proc) ->
%     case [ Doc || Doc <- docs(Proc), element(1,Doc) == element(1,Rec)] of
%          [D] -> D;
%          [] -> [];
%          E -> E end.

docs  (Proc) -> Proc#process.docs.
tasks (Proc) -> Proc#process.tasks.
events(Proc) -> Proc#process.events.

% Process Schema

new_task(Proc,GivenTask) ->
   Existed = [ Task || Task<- Proc#process.tasks, Task#task.name == GivenTask#task.name],
   case Existed of
        [] -> Proc#process{tasks=[GivenTask|Proc#process.tasks]};
         _ -> {error,exist,Existed} end.

delete(_Proc) -> ok.

val(Document,Proc,Cond) -> val(Document,Proc,Cond,fun(_,_)-> ok end).
val(Document,Proc,Cond,Action) ->
    case Cond(Document,Proc) of
         true -> Action(Document,Proc), {reply,Proc};
         {false,Message} -> {{reply,Message},Proc#process.task,Proc};
         ErrorList -> io:format("BPE:val/4 failed: ~tp~n",[ErrorList]),
                      {{reply,ErrorList},Proc#process.task,Proc} end.

pid(Id) -> bpe:cache({process,Id}).
cache(Key, undefined) -> ets:delete(processes,Key);
cache(Key, Value) -> cache(Key, Value, till(calendar:local_time(), ttl())).

cache(Key, Value, Till) -> ets:insert(processes,{Key,Till,Value}),
                            % io:format("cache inserted Id = ~p, pid = ~p~n", [Key, Value]),
                            Value.
cache(Key) ->
    Res = ets:lookup(processes,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,infinity,X} -> X;
                {_,Expire,X} -> case Expire < calendar:local_time() of
                                  true ->  ets:delete(processes,Key), undefined;
                                  false -> X end end.

ttl() -> application:get_env(bpe,ttl,60*15).

till(Now,TTL) ->
    case is_atom(TTL) of
        true -> TTL;
        false -> calendar:gregorian_seconds_to_datetime(
                    calendar:datetime_to_gregorian_seconds(Now) + TTL)
    end.

send(Pool, Message) -> syn:publish(term_to_binary(Pool),Message).
reg(Pool) -> reg(Pool,undefined).
reg(Pool, Value) ->
    case get({pool,Pool}) of
         undefined -> syn:register(term_to_binary(Pool),self(),Value),
                      syn:join(term_to_binary(Pool),self()),
                      erlang:put({pool,Pool},Pool);
          _Defined -> skip end.
unreg(Pool) ->
    case get({pool,Pool}) of
         undefined -> skip;
          _Defined -> syn:leave(Pool, self()),
                      erlang:erase({pool,Pool}) end.

reload(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} ->
            {reloaded, Module};
        {error, Reason} ->
            {load_error, Module, Reason}
    end.

add_hist(Proc) -> add_hist(Proc, []).
add_hist(Proc, T) ->
        ProcId = Proc#process.id,
        Task = case T of
                    [] -> {task, Proc#process.task};
                    _ -> T 
                end,
        Key = "/bpe/hist/" ++ ProcId,

        Writer = kvs:writer(Key),
        kvs:append(#hist{id = {Writer#writer.count, ProcId},
                        name = Proc#process.name,
                        time = calendar:local_time(),
                        docs = Proc#process.docs,
                        task = Task}, Key)
.