-module(bpe_gate).
% -export([action/5]).
-compile(export_all).
-include("bpe.hrl").
-include_lib("eunit/include/eunit.hrl").

action(inclusive, Module, Stage, Task, Proc) ->
    % # 1.0) проверить, что нет незавершенных входящих flow - bpe_proc?
    % # вход в start на inclusive gateway только тогда, когда нет незавершенных входящих флоу - передать все входящие tasks
    % # 3.0) при выполнении - завершить остальные InControlFinish в состоянии start|idle - bpe_proc?

    {IsInclusive, Tasks} = get_inclusive_tasks(Task, Proc),
    case IsInclusive of
        true -> bpe_task:task_action(Module, Stage, Tasks, Proc);
        false -> {reply, {idle, Task}, Proc}
    end
    % case {IsInclusive, Tasks} of
    %     {true, []} -> bpe_task:task_action(Module, Stage, Task, Proc); 
    %     {true, _} -> bpe_task:task_action(Module, Stage, Tasks, Proc);
    %     false -> {reply, {idle, Task}, Proc}
    % end
;
action(exclusive, Module, _Stage, Task, Proc) ->
    TaskName = Task#bpeTask.name,
    Flows = Proc#process.flows,
    Tasks = Proc#process.tasks,
    Conditions = [{list_to_atom(F#sequenceFlow.condition), F#sequenceFlow.target} || F <- Flows, F#sequenceFlow.source == TaskName],
    Gate = lists:keyfind(TaskName, 2, Tasks),
    case Gate#gateway.condition of
        [] -> io:format("Gateway ~p without condition! Can't lookup suitable sequenceFlow", [Gate]),
             {reply, {idle, Task}, Proc};
        _ -> Function = list_to_atom(Gate#gateway.condition),
             Target = lists:foldl(fun({Cond, Target0}, Acc) ->
                            case Acc of
                                [] -> case apply(Module, Function, [Task, Proc]) == Cond of
                                            true -> Target0;
                                            false -> []
                                      end;
                                _ -> Acc
                            end             
                        end, [], Conditions),
              case Target of
                    [] -> bpe:trace(Proc, Task, idle),
                        {reply, {idle, Task}, Proc};
                    _ ->  bpe:trace(Proc, Task, finish),
                        bpe_task:handle_tasks(start, bpe_task:get_next_tasks(Task, Proc, Target), Proc)
              end
    end
.

%инклюзивность проверяется только на один уровень выше
get_inclusive_tasks(Task, Proc) -> 
    TaskName = Task#bpeTask.name,
    TaskType = Task#bpeTask.type,
    Flows = Proc#process.flows,
    Inputs = lists:usort([F#sequenceFlow.source || F <- Flows, F#sequenceFlow.target == TaskName]),
    History = bpe:hist(Proc#process.id),
    SH = bpe:get_significant_history(History, true),
    ShortHist = 'Elixir.BPE.Ext':get_short_history(SH),
    io:format("get_inclusive_tasks task in = ~p~n short history: ~n~p~n", [TaskName, ShortHist]),
    {AllOtherFinished, UnfinishedTasks} = lists:foldl(fun(H, {AllOtherFinished0, UnfinishedTasks0} = Acc) ->
                                                                    T0 = H#hist.task,
                                                                    Stage0 = H#hist.stage,
                                                                    TaskName0 = T0#bpeTask.name,
                                                                    IsGate = TaskName0 == TaskName,
                                                                    IsGateOrInput = IsGate orelse lists:member(TaskName0, Inputs),
                                                                    case IsGateOrInput of
                                                                        true -> AllOtherFinished1 = case {AllOtherFinished0, Stage0, IsGate} of
                                                                                                {false, _, _} -> false;
                                                                                                {_, finish, _} -> AllOtherFinished0;
                                                                                                {_, _, true} -> AllOtherFinished0;
                                                                                                {_, _, _} -> false
                                                                                        end,
                                                                                UnfinishedTasks1  = case Stage0 of
                                                                                                    finish -> UnfinishedTasks0;
                                                                                                    _ -> [T0|UnfinishedTasks0]
                                                                                                end,

                                                                                {AllOtherFinished1, UnfinishedTasks1};
                                                                        false -> Acc
                                                                    end
                                                end, {true, []}, SH),
    IsInclusive =  AllOtherFinished == true,
    UnfinishedTasksQty = length(UnfinishedTasks),
    io:format("get_inclusive_tasks is inclusive - ~p~n  unfinished tasks qty ~p~n", [IsInclusive, UnfinishedTasksQty]),
    % ?assert(IsInclusive == true andalso (FinishedTasksQty == length(Inputs) orelse AllOtherFinished) orelse IsInclusive == false),
    {IsInclusive, UnfinishedTasks}
    
.

get_inclusive_tasks_old(Task, Proc) -> 
    TaskName = Task#bpeTask.name,
    TaskType = Task#bpeTask.type,
    Flows = Proc#process.flows,
    Inputs = lists:usort([F#sequenceFlow.source || F <- Flows, F#sequenceFlow.target == TaskName]),
    History = bpe:hist(Proc#process.id),
    SH = bpe:get_significant_history(History, true),
    ShortHist = 'Elixir.BPE.Ext':get_short_history(SH),
    io:format("get_inclusive_tasks task in = ~p~n short history: ~n~p~n", [TaskName, ShortHist]),
    {IsOtherUnfinished, AllOtherFinished, FinishedTasks, FinishedNamesUnsorted} = lists:foldl(fun(H, {IsOtherUnfinished0, AllOtherFinished0, FinishedTasks0, FinishedNames0}) ->
                                                                    T0 = H#hist.task,
                                                                    Stage0 = H#hist.stage,
                                                                    TaskName0 = T0#bpeTask.name,
                                                                    AllOtherFinished1 = case {AllOtherFinished0, Stage0, TaskName0 == TaskName} of
                                                                                                {false, _, _} -> false;
                                                                                                {_, finish, _} -> true;
                                                                                                {_, _, true} -> AllOtherFinished0;
                                                                                                {_, _, _} -> false
                                                                                        end,
                                                                    
                                                                    Res = lists:member(TaskName0, Inputs),
                                                                    case {Res, Stage0} of
                                                                         {true, finish} -> {IsOtherUnfinished0, AllOtherFinished1, [T0#bpeTask{name = TaskName, type = TaskType}|FinishedTasks0], [TaskName0|FinishedNames0]};
                                                                         {true, _} -> {true, AllOtherFinished1, FinishedTasks0, FinishedNames0};
                                                                         _ -> {IsOtherUnfinished0, AllOtherFinished1, FinishedTasks0, FinishedNames0}
                                                                    end
                                                end, {false, [], [], []}, SH),
    FinishedNames = lists:usort(FinishedNamesUnsorted),
    Delta = lists:subtract(Inputs, FinishedNames),
    IsInclusive = IsOtherUnfinished == false andalso (Delta == [] orelse AllOtherFinished) andalso (FinishedTasks /= []),
    FinishedTasksQty = length(FinishedNames),
    io:format("get_inclusive_tasks is inclusive - ~p~n finished tasks qty ~p~n", [IsInclusive, FinishedTasksQty]),
    ?assert(IsInclusive == true andalso (FinishedTasksQty == length(Inputs) orelse AllOtherFinished) orelse IsInclusive == false),
    {IsInclusive, FinishedTasks}
    
.



evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),    % scan the code into tokens
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),     % parse the tokens into an abstract form
    {value, Result, _} = erl_eval:exprs(Parsed, []),  % evaluate the expression, return the value
    Result.
