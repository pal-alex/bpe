-module(bpe_task).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").
-include_lib("eunit/include/eunit.hrl").

process_tasks(Proc) -> 
    ProcId = Proc#process.id,
    {N, StartedTasks} = bpe:current_tasks(ProcId),
    case {N, StartedTasks} of
        {[], _} ->BeginEvent = bpe:step(Proc#process.beginEvent, Proc),
                  Task = #bpeTask{id = kvs:seq([],[]), 
                                   name = BeginEvent#beginEvent.name,
                                   created = calendar:local_time(),
                                   type = {task, beginEvent},
                                   module = BeginEvent#beginEvent.module}, 
                  process_task(start, Task, Proc);
        {-1, _} -> {reply, all_finished, Proc};
        {_, []} -> {reply, all_finished, Proc};
        _ -> handle_tasks(finish, StartedTasks, Proc)
    end
. 

handle_tasks(Stage, Tasks, Proc) -> 
    % Если на вход список на старт, то вначале его весь в трэйс, чтобы инклюзив гейты работали корректно
    case Stage of
        start -> lists:foreach(fun(Task) -> bpe:trace(Proc, Task, Stage) end, Tasks);        
            _ -> skip
    end,
    lists:foldl(fun(T, {_, _, Proc0}) -> bpe_task:process_task(Stage, T, Proc0, false) end, {[], [], Proc}, Tasks).

process_task(Stage, Task, Proc) -> bpe_task:process_task(Stage, Task, Proc, true).
process_task(Stage, Task=#bpeTask{module = Module}, Proc, IsTrace) ->
    {Kind, Type} = Task#bpeTask.type,
    case {Kind, Type} of
        {gateway, Type} when Type /= parallel -> bpe_gate:action(Type, Module, Stage, Task, Proc);
                                            _ -> case {Stage, IsTrace} of
                                                      {start, true} -> bpe:trace(Proc, Task, Stage);        
                                                                  _ -> skip
                                                 end,
                                                 task_action(Module, Stage, Task, Proc)
    end          
.

       
task_action(Module, Stage, Task, Proc) ->
    {StageAfter, TaskAfter, ProcAfter} = Module:action(Stage, Task, Proc),
    ?assert(TaskAfter /= []),    

    Result = case StageAfter of
                start -> case is_equal(Task, TaskAfter) of
                          {true, true} -> bpe:trace(ProcAfter, TaskAfter, idle),
                                          handle_tasks(finish, to_list(TaskAfter), ProcAfter);
                         {true, false} -> bpe:trace(Proc, Task, finish),
                                          bpe:trace(ProcAfter, TaskAfter, idle),
                                          handle_tasks(finish, to_list(TaskAfter), ProcAfter); 
                                 {_,_} -> bpe:trace(Proc, Task, finish),
                                          handle_tasks(start, to_list(TaskAfter), ProcAfter)
                        end;
                finish -> finish_task(TaskAfter, ProcAfter);
                idle -> bpe:trace(ProcAfter, TaskAfter, StageAfter),
                        {reply, {idle, TaskAfter}, ProcAfter}
            end,
    Result
.

is_equal(Task1, Task2) when is_list(Task1), is_list(Task2) ->
    % T1 = lists:map(fun(T) -> T#bpeTask.id end, Task1),
    % T2 = lists:map(fun(T) -> T#bpeTask.id end, Task2),
    T1_1 = lists:map(fun(T) -> T#bpeTask.name end, Task1),
    T1_2 = lists:map(fun(T) -> T#bpeTask.name end, Task2),
    ByName = lists:sort(T1_1) == lists:sort(T1_2),
    T2_1 = lists:map(fun(T) -> T#bpeTask.id end, Task1),
    T2_2 = lists:map(fun(T) -> T#bpeTask.id end, Task2),
    ById = lists:sort(T2_1) == lists:sort(T2_2),
    {ByName, ById}
;
is_equal(Task1, Task2) -> is_equal(to_list(Task1), to_list(Task2)).

finish_task(Tasks, Proc) when is_list(Tasks) ->
    lists:foldl(fun(T, _Acc) -> finish_task(T, Proc) end, {reply, Tasks, Proc}, Tasks);
finish_task(Task, Proc) ->
    bpe:trace(Proc, Task, finish),
    NextTasks = bpe_task:get_next_tasks(Task, Proc),
    case NextTasks of
        [] -> bpe:finish_process(Proc);
        _ -> handle_tasks(start, NextTasks, Proc)
    end
.


get_next_tasks(Task, Proc) -> get_next_tasks(Task, Proc, []).
    
get_next_tasks(Task, Proc, TaskNameAfter) ->
        Name = Task#bpeTask.name,
        Flows = Proc#process.flows,
        Targets0 = lists:foldl(fun(F, Acc) ->
                        TargetName = case TaskNameAfter of
                                        [] ->  F#sequenceFlow.target;
                                        _ -> TaskNameAfter
                                    end,
                        case F#sequenceFlow.source == Name of
                            true -> case lists:member(TargetName, Acc) of
                                        true -> Acc;
                                        false -> [TargetName|Acc]
                                    end;
                            false -> Acc
                        end
                    end, [], Flows),
        lists:foldl(fun(TargetName, Acc) -> 
                        Targets = [ T || T <- Proc#process.tasks, element(2, T) == TargetName],
                        BPE_Tasks = lists:map(fun(T) -> 
                                            Type = case element(1, T) of
                                                    gateway -> {gateway, T#gateway.type};
                                                    Type0 -> {task, Type0}
                                                end,
                                            bpe_task:new(element(2, T), Type, element(3, T), Task#bpeTask.docs)
                                        end, Targets),
                        lists:append(BPE_Tasks, Acc)
                    end, [], Targets0) 
    
.


new(Name, Type, Module, Docs) ->
    #bpeTask{id = kvs:seq([],[]), 
        name = Name,
        type = Type,
        created = calendar:local_time(),
        module = Module,
        docs = Docs}
.
copy_new(Task) ->
    Task#bpeTask{id = kvs:seq([],[]), 
        created = calendar:local_time()
        }
.



to_list(Value) when Value == undefined -> [];
to_list(Value) when is_list(Value) -> Value;
to_list(Value) -> [Value].
