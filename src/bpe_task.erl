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

task_action(Module,CurrentTask,Target,Proc) ->
    case Module:action({complete,CurrentTask},Proc) of
         {run,State}                  -> bpe_proc:run(final,State);
         {until,Task,State}           -> bpe_proc:run(Task,State);
         {reply,State}                -> {reply,{complete,Target},State};
         {error,Message,Task,State}   -> {reply,{error,Message,Task},State};
         {{reply,Message},Task,State} -> {reply,{{complete,Message},Task},State}; % TODO: REFACTOR
         {reply,Task,State}           -> {reply,{complete,Task},State} end.

handle_task(#beginEvent{},_CurrentTask,Target,Proc) ->
    {reply,{complete,Target},Proc};

handle_task(#userTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#receiveTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#serviceTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#endEvent{},_CurrentTask,Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_task(_,_,Target,Proc) ->
    {reply,{unknown_task,Target},Proc}.


handle_starting_task(Curr, Proc) ->
    Task = bpe:step(Curr, Proc),
    Module = element(3, Task),
    Reply = case is_atom(Module) of
                true -> %io:format("handle_starting_task in a module ~p for a task ~p and a proc_id = ~p~n", [Module, Curr, Proc#process.id]),
                        try Module:action({start, Curr}, Proc) of
                            {reply, {complete, Curr}, Proc} -> bpe_proc:process_task([], Proc);    %task_action(Module,Curr,Curr,Proc);
                            {reply, {complete, Target}, Proc} -> bpe_proc:run(Target, Proc);
                            {reply, Proc} -> {reply, {started, Curr}, Proc};
                            {reply, Curr, Proc} -> {reply, {started, Curr}, Proc};
                            R -> R
                        catch
                            error:undef -> {reply, {started, Curr}, Proc};
                            error:function_clause -> {reply, {started, Curr}, Proc}

                        end;
                false -> %io:format("Didn't find a module ~p for a task ~p and a proc_id = ~p~n", [Module, Curr, Proc#process.id]),
                        {reply, {started, Curr}, Proc}
            end,
    Reply
.

