-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).
-import(lists,[keyfind/3, keyreplace/4]).

test() -> io:format("test 1", []).

attr(E) -> [ {N,V} || #xmlAttribute{name=N, value=V} <- E].

find(E=[#xmlText{}, #xmlElement{name='bpmn:conditionExpression'} | _], []) ->
  [{X, [V], attr(A)} || #xmlElement{name=X, attributes=A, content=[#xmlText{value=V}]} <- E];
%%It is expected that bpmn:flowNodeRef are present only in bpmn:lane and only #xmlText{} and #xmlElement{name='bpmn:flowNodeRef'} are present there 
find(E=[#xmlText{}, #xmlElement{name='bpmn:flowNodeRef'} | _], []) ->
  [{X, [], {value, V}} || #xmlElement{name=X, content=[#xmlText{value=V}]} <- E];
find(E, []) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].

def() -> load("priv/sample.bpmn").

nextTask(Acc, Flows, Task, EndEvent) when Task == EndEvent orelse Flows == [] -> Acc;
nextTask(Acc, Flows, Task, EndEvent) -> 
  % io:format("acc before ~p task=~p~n", [Acc,Task]),
  {F, Other} = lists:partition(fun(F0) -> F0#sequenceFlow.source == Task end, Flows),
  IsTask = lists:any(fun(T) -> T==Task end, Acc), 
  AccT = case IsTask of
          true -> Acc;
          false -> [Task|Acc]
        end,
  % io:format("next acc ~p~n", [AccT]),
  lists:foldl(fun(F0, Acc0) -> nextTask(Acc0, Other, F0#sequenceFlow.target, EndEvent) end, AccT, F)
.

load_proc(File, Module) ->
    {ok,Bin} = file:read_file(File),
    {#xmlElement{name='bpmn:definitions', content = C}, _} = xmerl_scan:string(binary_to_list(Bin)),
    [{'bpmn:process', Elements, Attrs}] = find(C, 'bpmn:process'),
    Name = list_to_atom(proplists:get_value(id, Attrs)),
    % io:format("~p~n", [Elements]),
    Proc = reduce(Elements,#process{name=Name},Module),
    Proc
.
  

load(File) -> load(File, ?MODULE).

load(File,Module) ->
  % {ok,Bin} = file:read_file(File),
  % {#xmlElement{name='bpmn:definitions', content = C}, _} = xmerl_scan:string(binary_to_list(Bin)),
  
  % _E = {'bpmn:definitions', [{'bpmn:process', Elements, Attrs}], _} = {N, find(C, 'bpmn:process'), attr(C)},
  
  % [{'bpmn:process', Elements, Attrs}] = find(C, 'bpmn:process'),
  % io:format("load_1: ~p~n", [C]),

  % Name = list_to_atom(proplists:get_value(id, Attrs)),
  % Proc = reduce(Elements,#process{name=Name},Module),
  Proc = load_proc(File, Module),
  Tasks = Proc#process.tasks,
  % io:format("tasks before: ~p~n", [Tasks]),
  Flows = Proc#process.flows,
  % BeginEvent = list_to_atom(Proc#process.beginEvent),
  % EndEvent = list_to_atom(Proc#process.endEvent),
  BeginEvent = Proc#process.beginEvent,
  EndEvent = Proc#process.endEvent,
  SortTasks0 = nextTask([BeginEvent], Flows, BeginEvent, EndEvent),
  SortTasks = lists:reverse(lists:map(fun(ST) -> 
                                          lists:keyfind(ST, 2, Tasks)
                                        end, [Proc#process.endEvent | SortTasks0])),
  % io:format("load_1: ~p~n", [SortTasks]),

  % Tasks = fillInOut(Proc#process.tasks, Proc#process.flows),
  Tasks1 = fixRoles(SortTasks, Proc#process.roles),
  Proc#process{ id=[],
                tasks = Tasks1,
                roles=[],
                events = [ #boundaryEvent{name='*', timeout=#timeout{spec={0,{0,30,0}}}}
                         | Proc#process.events ] }.

reduce([], Acc, _Module) ->
  Acc;

reduce([{'bpmn:task',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#task{module=Module,name=Name}|Tasks]}, Module);

reduce([{'bpmn:userTask',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
      Name = list_to_atom(proplists:get_value(id,Attrs)),
      reduce(T,Process#process{tasks=[#userTask{module=Module,name=Name}|Tasks]}, Module);

reduce([{'bpmn:serviceTask',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
          Name = list_to_atom(proplists:get_value(id,Attrs)),
          reduce(T,Process#process{tasks=[#serviceTask{module=Module,name=Name}|Tasks]}, Module);
        

reduce([{'bpmn:startEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#beginEvent{module=Module,name=Name}|Tasks],beginEvent=Name}, Module);

reduce([{'bpmn:endEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#endEvent{module=Module,name=Name}|Tasks],endEvent=Name}, Module);

reduce([{'bpmn:sequenceFlow',Body,Attrs}|T],#process{flows=Flows} = Process, Module) ->
  % Name   = proplists:get_value(id,Attrs),
  Source = list_to_atom(proplists:get_value(sourceRef,Attrs)),
  Target = list_to_atom(proplists:get_value(targetRef,Attrs)),
  Name   = list_to_atom(get_sequenceFlow_name(Source, Target)),
  F = #sequenceFlow{name=Name,source=Source,target=Target},
  Flow = reduce(Body,F,Module),
  % io:format("Flow = ~p~n", [ Flow ]),
  reduce(T,Process#process{flows=[Flow|Flows]}, Module);

reduce([{'bpmn:conditionExpression',Body,_Attrs}|T],#sequenceFlow{} = Flow, Module) ->
  % Cond = list_to_term(hd(Body)),
  Cond = hd(Body),
  reduce(T,Flow#sequenceFlow{condition=Cond},Module);

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=parallel}|Tasks]}, Module);

reduce([{'bpmn:exclusiveGateway', Body, Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  Condition = get_condition(Body),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name, condition = Condition, type=exclusive}|Tasks]}, Module);


reduce([{'bpmn:inclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=inclusive}|Tasks]}, Module);

reduce([{'bpmn:complexGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=complex}|Tasks]}, Module);

reduce([{'bpmn:gateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = list_to_atom(proplists:get_value(id,Attrs)),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=none}|Tasks]}, Module);

reduce([{'bpmn:laneSet',Lanes,_Attrs}|T], Process, Module) ->
  reduce(T,Process#process{roles = Lanes}, Module);

%%TODO? Maybe add support for those intries and remove them from this guard
reduce([{SkipType,_Body,_Attrs}|T],#process{} = Process, Module)
  when SkipType == 'bpmn:dataObjectReference';
       SkipType == 'bpmn:dataObject';
       SkipType == 'bpmn:association';
       SkipType == 'bpmn:textAnnotation' ->
  skip,
  reduce(T,Process, Module).

%%TODO?: Maybe use incoming/outgoing from XML itself instead of fillInOut
fillInOut(Tasks, []) -> Tasks;
fillInOut(Tasks, [#sequenceFlow{name=Name,source=Source,target=Target}|Flows]) ->
  Tasks1 = key_push_value(Name, #gateway.in, Source, #gateway.name, Tasks),
  Tasks2 = key_push_value(Name, #gateway.out, Target, #gateway.name, Tasks1),
  fillInOut(Tasks2, Flows). 

key_push_value(Value, ValueKey, ElemId, ElemIdKey, List) ->
  Elem = keyfind(ElemId, ElemIdKey, List),
  % io:format("~p - ~p - ~p~n", [ElemId, ElemIdKey, List]),
  RecName = element(1, Elem),
  if 
    RecName == beginEvent -> List;
    RecName == endEvent -> List;
    true ->
      NewElem = setelement(ValueKey, Elem, [Value|element(ValueKey,Elem)]),
      keyreplace(ElemId, ElemIdKey, List, NewElem)
  end.

fixRoles(Tasks, []) -> Tasks;
fixRoles(Tasks, [Lane|Lanes]) ->
  LaneAttributes = element(3,Lane),
  RoleName = proplists:get_value(id,LaneAttributes),
  Role = list_to_atom(RoleName),
  TaskIdsToUpdateRoles = [list_to_atom(T) || {'bpmn:flowNodeRef',[],{value,T}} <- element(2,Lane)],
  fixRoles(update_roles(TaskIdsToUpdateRoles, Tasks, Role), Lanes).

update_roles([], AllTasks, _Role) -> AllTasks;
update_roles([TaskId|Rest], AllTasks, Role) ->
  update_roles(Rest,key_push_value(Role, #task.roles, TaskId, #task.name, AllTasks),Role).

get_condition([]) -> [];
get_condition([{'bpmn:extensionElements', [BH|BT], _Attrs}|_T]) ->
      case get_condition(BH) of
        [] -> get_condition(BT);
        Value -> Value
      end;  
get_condition({'camunda:properties', [BH|BT], _Attrs}) ->
      case get_condition(BH) of
        [] -> get_condition(BT);
        Value -> Value
      end;  
get_condition({'camunda:property', _Body0, Attrs}) ->
        Name = proplists:get_value(name, Attrs),
        case Name of
          "condition" -> proplists:get_value(value, Attrs);
          _ -> []
        end;
        
get_condition(Body=[{_, _Body0, _Attrs}|T]) ->
    get_condition(T)
.


action({request,_,_},P) -> {reply,P}.

get_sequenceFlow_name(Source, Target) ->
  "sf_" ++ atom_to_list(Source) ++ "_" ++ atom_to_list(Target).

auth(_) -> true.

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.
