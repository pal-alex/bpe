-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/kvs.hrl").

% BPMN 2.0 API
-define(REQ, name=[] :: [] | atom(),
             module=[] :: [] | atom(),
             prompt=[] :: list(tuple())
             ).
-define(TASK, ?REQ,
              roles=[] :: list(atom()),
              etc=[] :: term() ).

-record(timeout,      { spec= [] :: term() }).
-define(EVENT, ?REQ, 
                payload=[] :: [] | binary(),
                timeout=[] :: [] | #timeout{}).
-define(CYCLIC, timeDate=[] :: [] | binary(),
                timeDuration=[] :: [] | binary(),
                timeCycle=[] :: [] | binary() ).

-type procId() :: [] | integer() | {atom(),any()}.
-type gate()   :: exclusive | parallel | inclusive | complex | event.
-type histId() :: [] | integer() | {atom()|string(),any()}.

-record(beginEvent,   { ?TASK }).
-record(endEvent,     { ?TASK }).
-record(task,         { ?TASK }).
-record(userTask,     { ?TASK }).
-record(serviceTask,  { ?TASK }).
-record(receiveTask,  { ?TASK }).
-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #beginEvent{} | #endEvent{}.


-record(messageEvent, { ?EVENT }).
-record(boundaryEvent,{ ?EVENT, ?CYCLIC }).
-record(timeoutEvent, { ?EVENT, ?CYCLIC }).
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.


-record(sequenceFlow, { ?REQ,
                        condition=[] :: term(),
                        source=[] :: [] | atom(),
                        target=[] :: [] | atom() }).

-record(gateway,      { ?TASK,
                        type= parallel :: gate(),
                        condition=[] :: term(),
                        in=[] :: list(atom()),
                        out=[] :: list(atom())
                        }).

-record(bpeTask, {id,
                    name,
                    module,
                    type,
                    docs = [],
                    created
                }).



-record(hist,         { id = [] :: histId(),
                        container=feed :: [] | atom(),
                        feed_id=[] :: any(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        feeds=[] :: list(),
                        name=[] :: [] | binary(),
                        task=[] :: [] | #bpeTask{}, 
                        stage=start  :: start|finish|idle,
                        time=[] :: term()
                      }).



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
                        timer      = [] :: [] | reference(),
                        notifications=[] :: [] | term(),
                        result     = [] :: [] | binary(),
                        started    = [] :: [] | calendar:datetime(),
                        finished   = [] :: [] | calendar:datetime(),
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
