-module(bpe_task).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

find_flow(List) -> [H|_] = List, H.
find_flow([],List) -> find_flow(List);
find_flow(Stage,List) -> case lists:member(Stage,List) of
                              true -> Stage;
                              _ -> find_flow(List) end.

targets(Curr,Proc) ->
    lists:flatten([ Target || #sequenceFlow{source=Source,target=Target}
                           <- Proc#process.flows,  Source==Curr]).

next_task(Curr, Proc) ->
    Targets = targets(Curr, Proc),
    find_flow(Targets).


denied_flow(Curr,Proc) ->
    {reply,{denied_flow,Curr},Proc}.

already_finished(Proc) ->
    {stop,{normal,[]},Proc}.

task_action(Module, CurrentTask, Target, Proc) ->
    {H, T, _} = bpe:current_proc_data(Proc#process.id),
    Action = case {H, T} of
            {0, _} -> start;
            {_, CurrentTask} -> complete;
            _ -> start
    end,
    task_action(Module,Action,CurrentTask,Target,Proc)
.        
task_action(Module,Action,CurrentTask,Target,Proc) ->
    case Module:action({Action,CurrentTask},Proc) of
         {run,State}                  -> bpe_proc:run(final,State);
         {until,Task,State}           -> bpe_proc:run(Task,State);
         {reply,State}                -> {reply,{complete,Target},State};
         {error,Message,Task,State}   -> {reply,{error,Message,Task},State};
         {{reply,Message},Task,State} -> {reply,{{complete,Message},Task},State}; % TODO: REFACTOR
         {reply,Task,State}           -> {reply,{complete,Task},State} end.

handle_task(#beginEvent{module=Module}, CurrentTask, Target, Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#userTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#receiveTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#serviceTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#gateway{type=parallel, module=Module}, Src, Dst, Proc) ->
    task_action(Module,Src,Dst,Proc);

handle_task(#gateway{type=inclusive, name=Name, module=Module}, Src, Dst, Proc) ->
    io:format("inclusive gateway ~p~n", [Name]),    
    task_action(Module,Src,Dst,Proc);
    


handle_task(#endEvent{module=Module}, CurrentTask, Target, Proc) ->
    task_action(Module, CurrentTask, Target, Proc),
    {stop,{normal,Target},Proc};

handle_task(_,_,Target,Proc) ->
    {reply,{unknown_task,Target},Proc}.


