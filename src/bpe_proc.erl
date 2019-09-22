-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

append_doc(Form, Proc) when is_list(Form) == false -> append_doc([Form], Proc, true);
append_doc(Forms, Proc) -> append_doc(Forms, Proc, true).

append_doc(Form, Proc, AddHist) when is_list(Form) == false -> append_doc([Form], Proc, AddHist);
append_doc(Forms, Proc, AddHist) ->
    NewDocs = lists:foldl(fun(Form, Acc) -> 
                                [Form | lists:filter(fun(X)-> is_tuple(X) andalso element(1, X) /= element(1, Form) end, Acc)]    
                         end, Proc#process.docs, Forms),
    NewProc = Proc#process{docs = NewDocs}, 
    case AddHist of
        true -> bpe:add_hist(NewProc);
        false -> skip
    end,
    {reply, docs, NewProc}  
.

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
    bpe:add_hist(ProcState, Task),

    io:format("Process: ~p Event: ~p Targets: ~p~n",[Proc#process.id,EventName,Targets]),
    io:format("Target: ~p Status: ~p Reason: ~p",[Target,Status,Reason]),

    NewProcState = ProcState#process{task = Target},
    begin fix_reply({Status,{Reason,Target},NewProcState}) end.

process_task(Stage,Proc) -> process_task(Stage,Proc,false).
process_task(Stage,Proc,NoFlow) ->
    Curr = Proc#process.task,
    Task = bpe:step(Curr,Proc),
    Targets = case NoFlow of
                   true -> noflow;
                   _ -> bpe_task:targets(Curr,Proc) end,

    {Status,{Reason,Target},ProcState} = case {Targets, Curr, Stage} of
                                                {noflow,_,_} -> {reply,{complete,Curr},Proc};
                                                {[],[],_}    -> bpe_task:already_finished(Proc);
                                                {[],Curr,_}  -> bpe_task:handle_task(Task,Curr,Curr,Proc);
                                                {[],_,_}     -> bpe_task:denied_flow(Curr,Proc);
                                                {List,_,[]}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(Stage,List),Proc);
                                                {List,_,_}   -> {reply,{complete,bpe_task:find_flow(Stage,List)},Proc} 
                                         end,
    ProcId = ProcState#process.id,
    NewProcState = ProcState#process{task = Target},                                         

    % Key = "/bpe/hist/" ++ ProcId,
    % Writer = kvs:writer(Key),
    % kvs:append(#hist{   id = {Writer#writer.count,ProcState#process.id},
    %                 name = [],
    %                 time = calendar:local_time(),
    %                 docs = ProcState#process.docs,
    %                 task = {task, Target} }, Key),

    bpe:add_hist(NewProcState),

    io:format("Process: ~p Task: ~p Targets: ~p ~n",[ProcId, Curr, Targets]),

    case Curr /= Target andalso (Stage == [] orelse Curr == 'Created') of
            true -> {S_Status,{S_Reason, S_Target}, S_ProcState} = bpe_task:handle_starting_task(Target, NewProcState),
                    case S_Reason of
                        started -> fix_reply({S_Status,{S_Reason, S_Target}, S_ProcState});
                        complete -> process_task([], S_ProcState) 
                    end; 
            false -> fix_reply({Status,{Reason,Target},NewProcState})
    end

    % case Curr /= Target andalso (Stage == [] orelse Curr == 'Created') of
    %     true -> bpe_task:handle_starting_task(Target, NewProcState); 
    %     false -> skip
    % end,
    % fix_reply({Status,{Reason,Target},NewProcState})

.

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({get},              _,Proc) -> { reply,Proc,Proc };
handle_call({run},              _,Proc) ->   run(final,Proc);
handle_call({until,Stage},      _,Proc) ->   run(Stage,Proc);
handle_call({event,Event},      _,Proc) ->   process_event(Event,Proc);
handle_call({start},            _,Proc) ->   process_task([],Proc);
handle_call({complete},         _,Proc) ->   process_task([],Proc);
handle_call({complete,Stage},   _,Proc) ->   process_task(Stage,Proc);
handle_call({amend,Form,true},  _,Proc) ->   process_task([],Proc#process{docs=[Form]},true);
handle_call({amend,Form},       _,Proc) ->   process_task([],Proc#process{docs=[Form]});
handle_call({append_doc, Form}, _,Proc) ->   append_doc(Form, Proc);
handle_call({remove,Form},      _,Proc) ->   process_task([],Proc#process{docs=[
                                         { remove,element(1,Form),element(2,Form)}]},true);
handle_call(Command,_,Proc)             -> { reply,{unknown,Command},Proc }.

init(Process) ->
    ProcId = Process#process.id,
    Till = infinity,
    % Till = bpe:till(calendar:local_time(), application:get_env(bpe,ttl,24*60*60)),
    bpe:cache({process, ProcId},self(),Till),

    Proc = bpe:load(ProcId, Process),
    io:format("Process ~p spawned as ~p.~n",[ProcId, self()]),
    % New_Proc = case bpe:head(ProcId) of
    %                 [] -> bpe:add_hist(Proc),
    %                     %   {_,_,StartedProc} = bpe_task:handle_starting_task(Proc#process.task, Proc),
    %                     {_,_,StartedProc} = bpe:start_task(ProcId),
    %                       StartedProc;
    %                 _ -> Proc
    %             end,
    New_Proc = Proc,
    case bpe:head(ProcId) of
        [] -> bpe:add_hist(Proc),
            %   {_,_,StartedProc} = bpe_task:handle_starting_task(Proc#process.task, Proc),
            bpe:start_task(ProcId, Proc#process.task);
        _ -> skip
    end,

    [ bpe:reg({messageEvent,element(1,EventRec),New_Proc#process.id}) || EventRec <- bpe:events(New_Proc) ],
    {ok, New_Proc}
    % {ok, Proc#process{timer=erlang:send_after(rand:uniform(10000),self(),{timer,ping})}}
    .

handle_cast({start, Stage}, Proc) ->
    {_,_,NewProc} = bpe_task:handle_starting_task(Stage, Proc),
    {noreply, NewProc}
;

handle_cast(Msg, State) ->
    io:format("Unknown API async: ~p.~n", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,5}).

handle_info({timer,ping}, State=#process{task=Task,timer=Timer,id=Id,events=Events,notifications=Pid}) ->
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
    Wildcard = '*',

    Terminal= case lists:keytake(Wildcard,#messageEvent.name,Events) of
                   {value,Event,_} -> {Wildcard,element(1,Event),element(#messageEvent.timeout,Event)};
                             false -> {Wildcard,boundaryEvent,{99999,ping()}} end,

    {Name,Record,{Days,Pattern}} = case lists:keytake(Task,#messageEvent.name,Events) of
                                       {value,Event2,_} -> {Task,element(1,Event2),element(#messageEvent.timeout,Event2)};
                                       false -> Terminal end,
    Time2 = calendar:local_time(),

    {DD,Diff} = case bpe:head(Id) of
                     #hist{time=Time1} -> calendar:time_difference(Time1,Time2);
                                    [] -> {immediate, deleted};
                                     _ -> {immediate, timeout} end,

%   io:format("Ping: ~p, Task: ~p Hist: ~p~n", [Id,Task,Hist]),

    case {{DD,Diff} < {Days,Pattern}, Record} of
        {true,_} -> {noreply,State#process{timer=timer_restart(ping())}};
        {false,timeoutEvent} ->
            io:format("BPE ~p: next step by timeout.~nDiff: ~p.~n",[Id,{DD,Diff}]),
            case process_task([],State) of
                {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
                {stop,normal,_,NewState} -> {stop,normal,NewState} end;
        {false, _} -> io:format("BPE ~p: clearing spawn and cache (~nDiff: ~p).~n",[Id, {DD,Diff}]),
            case is_pid(Pid) of
                true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
                false ->  bpe:cache({process,Id},undefined) 
            end,
           
            {stop,normal,State} end;

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

run(final, Process) -> run(Process#process.endEvent, Process);
run(Task,Process) ->
    CurrentTask = Process#process.task,
    case bpe_proc:process_task([],Process,false) of
         {reply,{complete,Reached},NewProc}
           when Reached /= CurrentTask andalso Reached /= Task -> run(Task,NewProc);
         Else -> Else end.

transient(#process{docs=Docs}=Process) ->
    Process#process{docs=lists:filter(
        fun (X) -> not lists:member(element(1,X),
            application:get_env(bpe,transient,[])) end,Docs)}.


 