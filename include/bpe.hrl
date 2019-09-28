-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/kvs.hrl").

% BPMN 2.0 API
-define(REQ, name=[] :: [] | atom(),
             module=[] :: [] | atom(),
             prompt=[] :: list(tuple())).

-record(task,         { ?REQ,
                        roles=[] :: [] | binary() }).
-record(userTask,     { ?REQ,
                        roles=[] :: [] | binary() }).
-record(serviceTask,  { ?REQ,
                        roles=[] :: [] | binary() }).
-record(receiveTask,  { ?REQ,
                        roles=[] :: [] | binary() }).

-record(messageEvent, { ?REQ,
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | {integer(),{integer(),integer(),integer()}} }).
-record(boundaryEvent,{ ?REQ,
                        payload=[] :: [] | binary(),
                        timeout=[] :: {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() }).
-record(timeoutEvent, { ?REQ,
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() }).
-record(beginEvent,   { ?REQ}).
-record(endEvent,     { ?REQ}).

-record(sequenceFlow, { ?REQ,
                        condition=[] :: term(),
                        source=[] :: [] | atom(),
                        target=[] :: [] | atom() | list(atom()) }).

-record(gateway,      { ?REQ,
                        type= none :: gate()
                        }).

-record(bpe_task, {id,
                    name,
                    module,
                    type,
                    docs}).


-type histId() :: [] | integer() | {atom()|string(),any()}.

-record(hist,         { id = [] :: histId(),
                        container=feed :: [] | atom(),
                        feed_id=[] :: any(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        feeds=[] :: list(),
                        name=[] :: [] | binary(),
                        task=[] :: [] | #bpe_task{}, %atom() | {atom()|string(),any()},
                        stage=start  :: start|finish|idle,
                        docs=[] :: list(tuple()),
                        time=[] :: term() }).

-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #beginEvent{} | #endEvent{}.
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.
-type procId() :: [] | integer() | {atom(),any()}.
-type gate()   :: none | exclusive | parallel | inclusive | complex | event.

-record(process,      { id = [] :: procId(),
                        container=feed :: [] | atom(),
                        feed_id=[] :: [] | atom() | term(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        name=[] :: [] | binary() | string() | atom(),
                        feeds=[] :: list(),
                        roles      = [] :: list(),
                        tasks      = [] :: list(tasks()),
                        events     = [] :: list(events()),
                        hist       = [] :: [] | term(),
                        flows      = [] :: list(#sequenceFlow{}),
                        rules      = [] :: [] | term(),
                        docs       = [] :: list(tuple()),
                        options    = [] :: term(),
                        % started_tasks = [] | list(#bpe_task{}),
                        timer      = [] :: [] | reference(),
                        notifications=[] :: [] | term(),
                        result     = [] :: [] | binary(),
                        started    = [] :: [] | calendar:datetime(),
                        beginEvent = [] :: [] | atom(),
                        endEvent   = [] :: [] | atom()}).




% BPE API

-record('Comp', { id=[]   :: [] | integer() }).
-record('Proc', { id=[]   :: [] | integer() }).
-record('Load', { id=[]   :: [] | integer() }).
-record('Hist', { id=[]   :: [] | integer() }).
-record('Make', { proc=[] :: [] | #process{} | binary(), docs=[] :: [] | list(tuple()) }).
-record('Amen', { id=[]   :: [] | integer(), docs=[] :: [] | list(tuple()) }).

-endif.
