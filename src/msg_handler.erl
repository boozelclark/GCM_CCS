-module(msg_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init(Name) ->
        {ok, State} = {ok, Name},
        {ok, State}.

handle_event({message, Message}, State) ->
        io:format("MSG_HANDLER - Handling Message: ~n~p~n", [Message]),
	%Handle users handler
        {ok, State};

handle_event({data, Sender, D}, State) ->
        io:format("MSG_HANDLER - Handling Event: ~n~p~n", [D]),
	handle_msg_in(Sender, D),
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

%%=======================================================================
handle_msg_in(Sender, Packet) ->
	%io:format("Stanza: ~p",[Packet]),
    	
	{ok,PKG} = application:get_env(gcm_ccs,pkg),
	PKG_b = list_to_binary(PKG),
	%Get Data from Msg
	%XFrom = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    	%XTo = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
	
	Data = exmpp_xml:get_element(Packet,"gcm"),
	
	%get Data
	JSONData = exmpp_xml:get_cdata(Data),
	{DataIn} = jiffy:decode(JSONData),
	
	case lists:keyfind(<<"message_type">>, 1, DataIn) of
	
		{<<"message_type">>,<<"ack">>} ->
			io:format("MSG_HANDLER - Ack Msg~n");
		{<<"message_type">>,<<"nack">>} ->
			io:format("MSG_HANDLER - Nack Msg~n");
		{<<"message_type">>,<<"control">>} ->
			io:format("MSG_HANDLER - Control Msg~n"),
			{<<"control_type">>,<<"CONNECTION_DRAINING">>} = lists:keyfind(<<"control_type">>, 1, DataIn),
			%Add code to drain connections.
			io:format("Connection Draining Cmd~n");
			
		_ ->
			case lists:keyfind(<<"category">>, 1, DataIn) of
				{<<"category">>,PKG_b} ->
					io:format("MSG_HANDLER - Incoming Msg~n"),
					{<<"from">>,DataFrom} = lists:keyfind(<<"from">>, 1, DataIn),
					{<<"message_id">>,DataMsgId} = lists:keyfind(<<"message_id">>, 1, DataIn),
					gcm:sendAck(Sender, DataFrom, DataMsgId),
					{<<"data">>,DataMsg} = lists:keyfind(<<"data">>, 1, DataIn),
					
					gen_event:notify(gcm_xmpp_handler, {message,DataMsg});
				_ ->
					io:format("MSG_HANDLER - Unknown Message Type~n")
			end
 	end
	.
