-module(sample_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init(Name) ->
        {ok, State} = {ok, Name},
        {ok, State}.

handle_event({message, Message}, State) ->
        io:format("SAMPLE_HANDLER - Message Received for User to Handle: ~n~p~n", [Message]),
        {ok, State};

handle_event(_, State) ->
        {ok, State}.

handle_call(_, State) ->
        {ok, ok, State}.

handle_info(_, State) ->
        {ok, State}.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

terminate(_Reason, _State) ->
        ok.


