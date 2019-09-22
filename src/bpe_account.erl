-module(bpe_account).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-export([def/0]).
-compile(export_all).

def() ->
    #process { name = 'IBAN Account',
        flows = [
            #sequenceFlow{source='Init',      target='Upload'},
            #sequenceFlow{source='Upload',    target='Payment'},
            #sequenceFlow{source='Payment',   target=['Signatory','Process']},
            #sequenceFlow{source='Process',   target=['Process','Final']},
            #sequenceFlow{source='Signatory', target=['Process','Final']} ],
        tasks = [
            #userTask    { name='Init',      module = bpe_account },
            #userTask    { name='Upload',    module = bpe_account },
            #userTask    { name='Signatory', module = bpe_account },
            #serviceTask { name='Payment',   module = bpe_account },
            #serviceTask { name='Process',   module = bpe_account },
            #endEvent    { name='Final',     module = bpe_account } ],
        beginEvent = 'Init',
        endEvent = 'Final',
        events = [ #messageEvent{name='PaymentReceived'} ] }.

action({starting, Stage = 'Init'}, Proc) ->
        {reply, Proc};
        

action({complete,'Init'}, Proc) ->
    {reply,Proc};

action({complete,'Payment'}, Proc) ->
    Payment = bpe:doc({payment_notification},Proc),
    case Payment of
         [] -> {reply,'Process',Proc#process{docs=[#tx{}]}};
          _ -> {reply,'Signatory',Proc} end;

action({complete,'Signatory'}, Proc) ->
    {reply,'Process',Proc};

action({complete,'Process'}, Proc) ->
    case bpe:doc(#close_account{},Proc) of
         #close_account{} -> {reply,'Final',Proc};
                        _ -> {reply,Proc} end;

action({complete,'Upload'}, Proc) ->
    {reply,Proc};

action({complete,'Final'}, Proc) ->
    {reply,Proc}.
