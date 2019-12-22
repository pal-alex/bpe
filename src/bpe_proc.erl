-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_event(Event,Proc) ->
    EventName = element(#messageEvent.name,Event),
    Targets = bpe_task:targets(EventName,Proc),

    {Status,{Reason,Target},ProcState} = bpe_event:handle_event(Event,bpe_task:find_flow(Targets),Proc),

    % Key = "/bpe/hist/" ++ ProcState#process.id,
    % Writer = kvs:writer(Key),

    % the reason we need compund keys here for id field
    % is that in mnesia backend all hist entries are stored in one table
    % so step position is not enough. For RocksDB you can use just writer.count.

    % kvs:append(#hist{ id = {Writer#writer.count,ProcState#process.id},
    %                 name = [],
    %                 time = calendar:local_time(),
    %                 docs = ProcState#process.docs,
    %                 task = {event, element(#messageEvent.name,Event) }}, Key),
    Task = {event, element(#messageEvent.name,Event) },
    bpe:trace(ProcState, finish, Task),

    io:format("Process: ~p Event: ~p Targets: ~p~n",[Proc#process.id,EventName,Targets]),
    io:format("Target: ~p Status: ~p Reason: ~p",[Target,Status,Reason]),

    NewProcState = ProcState,
    begin fix_reply({Status,{Reason,Target},NewProcState}) end.



fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({get},              _,Proc) -> { reply,Proc,Proc };

% handle_call({amend,Form},        _,Proc) -> try bpe:processFlow(bpe_env:append(env,Proc,Form)) catch X:Y:Z -> {reply,{error,'amend/2',Z},Proc} end;
% handle_call({discard,Form},      _,Proc) -> try bpe:processFlow(bpe_env:remove(env,Proc,Form)) catch X:Y:Z -> {reply,{error,'discard/2',Z},Proc} end;
% handle_call({event,Event},       _,Proc) -> try process_event(Event,Proc) catch X:Y:Z -> {reply,{error,'event/2',Z},Proc} end;
% handle_call({modify,Form,append},_,Proc) -> try process_task([],bpe_env:append(env,Proc,Form),true) catch X:Y:Z -> {reply,{error,'append/2',Z},Proc} end;
% handle_call({modify,Form,remove},_,Proc) -> try process_task([],bpe_env:remove(env,Proc,Form),true) catch X:Y:Z -> {reply,{error,'remove/2',Z},Proc} end;

handle_call({start},            _,Proc) ->   bpe_task:process_tasks(Proc);
handle_call({complete},         _,Proc) ->   bpe_task:process_tasks(Proc);
handle_call({complete, _Stage},   _,Proc) ->   bpe_task:process_tasks(Proc);
handle_call(Command,_,Proc)             -> { reply,{unknown,Command},Proc }.

init(Process) ->
    ProcId = Process#process.id,
    Till = infinity,
    % Till = bpe:till(calendar:local_time(), application:get_env(bpe,ttl,24*60*60)),
    bpe:cache({process, ProcId},self(),Till),

    Proc = bpe:load(ProcId, Process),
    io:format("Process ~p spawned as ~p.~n",[ProcId, self()]),
    [ bpe:reg({messageEvent, element(1, EventRec), ProcId}) || EventRec <- bpe:events(Proc) ],
    {ok, Proc}
    % {ok, Proc#process{timer=erlang:send_after(rand:uniform(10000),self(),{timer,ping})}}
    .

handle_cast(Msg, State) ->
    io:format("Unknown API async: ~p.~n", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,5}).

% handle_info({timer,ping}, State=#process{timer=Timer,id=Id, events=Events, notifications=Pid}) ->
%     case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
%     Wildcard = '*',

%     Terminal= case lists:keytake(Wildcard,#messageEvent.name,Events) of
%                    {value,Event,_} -> {Wildcard,element(1,Event),element(#messageEvent.timeout,Event)};
%                              false -> {Wildcard,boundaryEvent,{99999,ping()}} end,

%     {Name,Record,{Days,Pattern}} = case lists:keytake(Task, #messageEvent.name, Events) of
%                                        {value,Event2,_} -> {Task,element(1,Event2),element(#messageEvent.timeout,Event2)};
%                                        false -> Terminal end,
%     Time2 = calendar:local_time(),

%     {DD,Diff} = case bpe:head(Id) of
%                      #hist{time=Time1} -> calendar:time_difference(Time1,Time2);
%                                     [] -> {immediate, deleted};
%                                      _ -> {immediate, timeout} end,

% %   io:format("Ping: ~p, Task: ~p Hist: ~p~n", [Id,Task,Hist]),

%     case {{DD,Diff} < {Days,Pattern}, Record} of
%         {true,_} -> {noreply,State#process{timer=timer_restart(ping())}};
%         {false,timeoutEvent} ->
%             io:format("BPE ~p: next step by timeout.~nDiff: ~p.~n",[Id,{DD,Diff}]),
%             case process_task([],State) of
%                 {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
%                 {stop,normal,_,NewState} -> {stop,normal,NewState} end;
%         {false, _} -> io:format("BPE ~p: clearing spawn and cache (~nDiff: ~p).~n",[Id, {DD,Diff}]),
%             case is_pid(Pid) of
%                 true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
%                 false ->  bpe:cache({process,Id},undefined) 
%             end,
           
%             {stop,normal,State} end;

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #process{id=Id}) ->
    io:format(?MODULE, "connection closed, shutting down session: ~p.~n", [Msg]),
    bpe:cache({process,Id},undefined),
    {stop, normal, State};

handle_info(Info, State=#process{}) ->
    io:format("Unrecognized info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #process{id=Id}) ->
    io:format("Terminating session Id cache: ~p~n Reason: ~p~n", [Id,Reason]),
    spawn(fun() -> supervisor:delete_child(bpe_otp,Id) end),
    bpe:cache({process,Id},undefined),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


 