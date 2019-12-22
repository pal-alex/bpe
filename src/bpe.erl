-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-include("api.hrl").
-compile(export_all).
-define(TIMEOUT, application:get_env(bpe,timeout,60000)).
% -define(TIMEOUT, 600).

load(Id) -> load(Id, []).
load(Id, Def) ->
    case kvs:get("/bpe/proc",Id) of
         {error,_} -> case Def /= [] of
                        true -> io:format("Didn't find bpe/proc with id = ~p, loaded def~n", [Id]), 
                                Def;
                        false -> load_arc(Id)
                      end;
         {ok,Proc} -> io:format("load bpe/proc with id = ~p~n", [Id]),
                      Proc
    end
.
load_arc(Id) ->
    case kvs:get("/bpe/archive", Id) of
         {error, _} -> io:format("Didn't find bpe/archive with id = ~p~n", [Id]), 
                      [];
                        
         {ok, Proc} -> io:format("load bpe/archive with id = ~p~n", [Id]),
                      Proc
    end
.



test() -> io:format("test 17~n", []).


cleanup(P) ->
  [ kvs:delete("/bpe/hist",Id) || #hist{id=Id} <- bpe:hist(P) ],
    kvs:delete(writer,"/bpe/hist/" ++ P),
    kvs:delete("/bpe/proc",P)
  .
task(Tasks, TaskName) when is_list(Tasks) ->
    case [ Task || Task <- Tasks, element(2, Task) == TaskName] of
        [T] -> T;
        [] -> []
    end;
task(ProcId, TaskName) -> 
    Proc = bpe:load(ProcId),
    bpe:step(TaskName, Proc)
.
step(TaskName, Proc) -> 
    Tasks = tasks(Proc),
    bpe:task(Tasks, TaskName)
.



% .
% current_task(Id) ->
%     case bpe:head(Id) of
%          [] -> {0, {task, []}};
%          #hist{id={H,_},task=T} -> {H,T} end.

get_significant_history(Hist) -> get_significant_history(Hist, false).
get_significant_history(Hist, CheckOnlyId) ->
    SH = lists:foldl(fun(H, Acc) ->
                        T = H#hist.task,
                        TId = T#bpeTask.id,
                        TName = T#bpeTask.name,
                        Existed = lists:any(fun(H0) ->
                                                    T0 = H0#hist.task,
                                                    T0#bpeTask.id == TId andalso (CheckOnlyId orelse T0#bpeTask.name == TName)
                                            end, Acc),
                        case Existed of
                            true -> Acc;
                            false -> [H|Acc]
                        end
                    end, [], Hist),
    SH
.

current_tasks(ProcId) -> 
    Hist = bpe:hist(ProcId),
    Result = case Hist of
                  [] -> {[], []};
                   _ -> SH = get_significant_history(Hist),
                        FH = lists:filter(fun(H) -> H#hist.stage /= finish end, SH),
                        lists:foldl(fun(#hist{task = Task, id = {N, _}}, {_, Tasks}) -> {N, [Task|Tasks]} end, {-1, []}, FH)
            end,
    Result
.

start(Proc0) -> start(Proc0, [], []).
start(Proc0, Options) -> start(Proc0, Options, []).
start(Proc0, Options, Docs) when is_list(Docs) == false -> start(Proc0, Options, [Docs]);
start(Proc0, Options, Docs0) -> 
    Id   = case Proc0#process.id of [] -> kvs:seq([],[]); X -> X end,
    % {N0, T0} = bpe:current_tasks(Id),
    Hist = bpe:hist(Id),
    Docs = case Hist of
                [] -> Key  = "/bpe/hist/" ++ Id,
                    kvs:ensure(#writer{id=Key}),
                    
                    %   BeginEvent = Proc0#process.beginEvent,
                    %   Task = #bpeTask{id = kvs:seq([],[]), 
                    %                     name = BeginEvent#beginEvent.name,
                    %                     type = task},
                    %   bpe:trace(Proc0, Task, start),
                    %   {0, [Task], Docs0}
                    % {0, [], Docs0};
                    Docs0;
                        
                _ -> D0 = case Docs0 of
                                [] -> Proc0#process.docs;
                                _ -> Docs0
                            end,
                    D0
            end,

    Pid  = proplists:get_value(notification, Options, undefined),
    Proc = Proc0#process{id=Id, options=Options, notifications=Pid, started=calendar:local_time(), docs=Docs},

    kvs:append(Proc, "/bpe/proc"),
    
    Restart = transient,
    Shutdown = ?TIMEOUT,
    ChildSpec = { Id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },
    % io:format("start a child with id = ~p~n", [Id]),
    Result = case supervisor:start_child(bpe_otp, ChildSpec) of
                {ok,_} -> {ok,Proc#process.id};
                {ok,_,_} -> {ok,Proc#process.id};
                {error, already_present} -> {ok, Proc#process.id};
                {error, {already_started, _}} -> {ok, Proc#process.id}
                % {error, Error} -> {{error, Error}, Proc#process.id} 
          end,

    Result
.


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
% run(ProcId)               -> gen_server:call(find_pid(ProcId),{run},            ?TIMEOUT).
% until(ProcId,Task)        -> gen_server:call(find_pid(ProcId),{until,Task},     ?TIMEOUT).
amend(ProcId,Form)        -> gen_server:call(find_pid(ProcId),{amend,Form},     ?TIMEOUT).
amend(ProcId,Form,noflow) -> gen_server:call(find_pid(ProcId),{amend,Form,true},?TIMEOUT).
% append_doc(ProcId, Form)  -> gen_server:call(find_pid(ProcId),{append_doc, Form}, ?TIMEOUT).
discard(ProcId,Form)      -> gen_server:call(pid(ProcId),{discard,Form},   ?TIMEOUT).
modify(ProcId,Form,Arg)   -> gen_server:call(pid(ProcId),{modify,Form,Arg},?TIMEOUT).
event(ProcId,Event)       -> gen_server:call(find_pid(ProcId),{event,Event},    ?TIMEOUT).



delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                   lists:member(Task#task.name,Tasks) ] }.

% BPE for now supports only MNESIA and ROCKS backends.

restore_stage(Proc0) when is_tuple(Proc0) ->
    % ProcId = Proc0#process.id,
  
    Proc = Proc0#process{started=calendar:local_time()},
    Proc
;
restore_stage(ProcId) ->
    Proc0 = kvs:get_value('/bpe/proc', ProcId),
    restore_stage(Proc0)
.

% head(ProcId) ->
%   Feed = "/bpe/hist/" ++ ProcId,
%   case kvs:get(writer, Feed) of
%         {ok, #writer{count = C} = _W} ->  %io:format("head hist = ~p~n", [W]),
%                                      case kvs:get(Feed, {C - 1, ProcId}) of
%                                          {ok, X} -> X; 
%                                          _ -> [] 
%                                      end;
%                               _ -> []
% end.

 
  

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

% step(Proc) -> step(Proc#process.task, Proc).


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

get_task({HId, ProcId}) when is_integer(ProcId) ->
    get_task({HId, integer_to_list(ProcId)});
get_task(HistId = {_HId, ProcId}) ->
    % Hist = bpe:hist(ProcId),
    Key = "/bpe/hist/" ++ ProcId,
    H = kvs:get_value(Key, HistId),
    Task = H#hist.task,
    Task
.

finish_process(Proc) when is_tuple(Proc) ->
    ProcId = Proc#process.id,
    finish_process(Proc, ProcId);
finish_process(ProcId) ->
    Proc = load(ProcId),
    finish_process(Proc, ProcId)
.
finish_process(Proc0, ProcId) ->
    case bpe:current_tasks(ProcId) of
        {-1, []} -> % TODO: copy to archive
                    Proc = Proc0#process{finished = calendar:local_time()},
                    Feed = "/bpe/proc",
                    kvs:append(Proc, Feed),
                    % FeedArc = "/bpe/archive",
                    % kvs:move(Proc, Feed, FeedArc),
                    % kvs:append(Proc, FeedArc),
                    % kvs:delete(Feed, ProcId),
                    % TODO: terminate process and not infinity life for archive processes
                    {stop, finish, Proc};
        {_N, _StartedTasks} -> {reply, not_all_finished, Proc0}
    end
.     

% delete(_Proc) -> ok.

% val(Document,Proc,Cond) -> val(Document,Proc,Cond,fun(_,_)-> ok end).
% val(Document,Proc,Cond,Action) ->
%     case Cond(Document,Proc) of
%          true -> Action(Document,Proc), {reply,Proc};
%          {false,Message} -> {{reply,Message},Proc#process.task,Proc};
%          ErrorList -> io:format("BPE:val/4 failed: ~tp~n",[ErrorList]),
%                       {{reply,ErrorList},Proc#process.task,Proc} end.

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

trace(Proc, Tasks, Stage) when is_list(Tasks) ->
    lists:foreach(fun(T) -> trace(Proc, T, Stage) end, Tasks);
trace(Proc, Task, Stage) -> 
        ProcId = Proc#process.id,
        Key = "/bpe/hist/" ++ ProcId,
        Writer = kvs:writer(Key),
        kvs:append(#hist{id = {Writer#writer.count, ProcId},
                        name = Proc#process.name,
                        time = calendar:local_time(),
                        stage = Stage,
                        task = Task}, Key)
.