-module(bpe_gate).
-compile(export_all).
-include("bpe.hrl").

action(inclusive, Module, Stage, Task, Proc) ->
    % # 1.0) проверить, что нет незавершенных входящих flow - bpe_proc?
    % # вход в start на inclusive gateway только тогда, когда нет незавершенных входящих флоу - передать все входящие tasks
    % # 3.0) при выполнении - завершить остальные InControlFinish в состоянии start|idle - bpe_proc?

    {IsInclusive, Tasks} = get_inclusive_tasks(Task, Proc),
    case IsInclusive of
        true -> bpe_task:task_action(Module, Stage, Tasks, Proc);
        false -> {reply, {idle, Task}, Proc}
    end
.

%инклюзивность проверяется только на один уровень выше
get_inclusive_tasks(Task, Proc) -> 
    TaskName = Task#bpeTask.name,
    TaskType = Task#bpeTask.type,
    Flows = Proc#process.flows,
    Inputs = [F#sequenceFlow.source || F <- Flows, F#sequenceFlow.target == TaskName],
    History = bpe:hist(Proc#process.id),
    SH = get_significant_history(History),
    {IsInclusive, FinishedTasks} = lists:foldl(fun(H, Acc = {IsInclusive0, FinishedTasks0}) ->
                                                    T0 = H#hist.task,
                                                    Stage0 = H#hist.stage,
                                                    TaskName0 = T0#bpeTask.name,
                                                    Res = lists:member(TaskName0, Inputs),
                                                    case {Res, Stage0} of
                                                        {true, finish} -> {IsInclusive0, [T0#bpeTask{name = TaskName, type = TaskType}|FinishedTasks0]};
                                                        {false, _} -> Acc;
                                                        {true, _} -> {false, FinishedTasks0}
                                                    end
                                            end, {true, []}, SH),
    % IsInclusive = IsInclusive1 and (length(FinishedTasks) == length(Inputs)),                   
%TODO: проверка, что среди Inputs нет таска, который не попал в SH
    {IsInclusive, FinishedTasks}
    
.


get_significant_history(History) -> 
    lists:foldl(fun(H, Acc) ->
                    T = H#hist.task,
                    TId = T#bpeTask.id,
                    Existed = lists:any(fun(H0) ->
                                            T0 = H0#hist.task,
                                            T0#bpeTask.id == TId
                                        end, Acc),
                    case Existed of
                        true -> Acc;
                        false -> [H|Acc]
                    end
                end, [], History)
.
