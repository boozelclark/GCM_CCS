%%%----------------------------------------------------------------------------
%%% @doc An OTP GCM CSS Server
%%% @author C Clark
%%% @end
%%%----------------------------------------------------------------------------

-module(gcm).         % Nothing new in this section except for the
                               %  next line where we tell the compiler that
-behaviour(gen_server).        %  this module implements the gen_server
                               %  behaviour. The compiler will warn us if
-define(SERVER, ?MODULE).      %  we do not provide all callback functions
                               %  the behaviour announces. It knows what
-record(state, {session}).     %  functions to expect by calling 
                               %  gen_server:behaviour_info(callbacks). Try it.

%%==============================================
%%	GCM SETTINGS
%%==============================================
-define(DEFAULT_TTL,0).				%TTL for messages sent to GCM
-define(GCM_SERVER,"gcm.googleapis.com").	%-|_Can be changed to the dev server
-define(GCM_PORT,5235).				%-|
-define(GCM_JID,"devices@gcm.googleapis.com").	%Should not need to be changed

-record(received_packet,
                   {
                   packet_type, % message, iq, presence
                   type_attr,   % depend on packet. Example: set, get, subscribe, etc
                   from,        % JID
                   id,          % Packet ID
                   queryns,     % IQ only: Namespace of the query
                   raw_packet   % raw exmpp record
}).
%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([                       
  start_link/0,                % - starts and links the process in one step
  stop/0,                      % - stops it
  send/3,
  sendAck/3,
  sendAck/2,
  attach_handler/2,
  remove_handler/2]).                     % - sends a GCM Message

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([                      % The behaviour callbacks
  init/1,                      % - initializes our process
  handle_call/3,               % - handles synchronous calls (with response)
  handle_cast/2,               % - handles asynchronous calls  (no response)
  handle_info/2,               % - handles out of band messages (sent with !)
  terminate/2,                 % - is called on shut-down
  code_change/3]).             % - called to handle code changes

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->                % start_link spawns and links to a new 
    gen_server:start_link(     %  process in one atomic step. The parameters:
      {local, ?SERVER},        %  - name to register the process under locally
      ?MODULE,                 %  - the module to find the init/1 callback in 
      [],                      %  - what parameters to pass to init/1
      []).                     %  - additional options to start_link

stop() ->                      %  Instead
    gen_server:cast(           %  we use cast to send a message asynch. to
      ?SERVER,                 %  the registered name. It is asynchronous
      stop).                   %  because we do not expect a response.

send(RegId, Msg, MsgId) ->
    gen_server:cast(           
      ?SERVER,                 
      {send,RegId, Msg, MsgId}).


sendAck(Sender, RegId, MsgId) ->
	gen_server:cast(           
      Sender,                 
      {ack,RegId, MsgId}).

sendAck(RegId, MsgId) ->
	gen_server:cast(           
      ?SERVER,                 
      {ack,RegId, MsgId}).

attach_handler(Handler_module, Name) ->
	gen_event:add_handler(gcm_xmpp_handler, Handler_module, [Name])
	.

remove_handler(Handler_module, Name) ->
	gen_event:delete_handler(gcm_xmpp_handler, Handler_module, [Name])
	.

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->                    % these are the behaviour callbacks. init/1 is
    %{ok, #state{session=0}}.  % called in response to gen_server:start_link/4
                               
    io:format("GCM - Init Called~n"),

	gen_event:start({local, gcm_xmpp_handler}),
        gen_event:add_handler(gcm_xmpp_handler, msg_handler, [internal_gcm_xmpp_handler]),

    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    Ses = exmpp_session:start({1,0}),
    %% Get Credentials set in app file ENV
    {ok,GCM_Id} = application:get_env(gcm_ccs,gcm_sender_id),
    {ok,PWD} = application:get_env(gcm_ccs,pwd),

    %% Create XMPP ID (Session Key):
    Jid = exmpp_jid:parse(GCM_Id),
    %% Create a new session:
    exmpp_session:auth_info(Ses, Jid, PWD),
    %% Connect Using SSL:
    {ok, _,_StreamId} = exmpp_session:connect_SSL(Ses, ?GCM_SERVER, ?GCM_PORT,[{starttls, enabled}]),
    Session = session(Ses, Jid, PWD),    
    {ok, #state{session=Session}}.

handle_call(_, _From, #state{session=Session}) -> 
    {reply, 
     ok,                    
     #state{session=Session}     
    }.

handle_cast(stop, State) ->    % this is the first handle_case clause that
    {stop,                     % deals with the stop atom. We instruct the
     normal,                   % gen_server to stop normally and return
     State                     % the current State unchanged.
    };                         

handle_cast({send, RegId, Msg, MsgId}, State) -> 
    io:format("GCM - Cast Send~n"), 
    sendMessage(State#state.session,RegId, Msg, MsgId),    
    {noreply,                  
    #state{session=
      State#state.session}
    };


handle_cast({ack, RegId , MsgId}, State) -> 
    io:format("GCM - Cast Ack~n"), 
    send_ack(State#state.session,RegId, MsgId),    
    {noreply,                  
    #state{session=
      State#state.session}
    }.

handle_info(#received_packet{} = Packet, #state{session = Session} = State) ->
    io:format("GCM - Message From GCM:~n~p~n", [Packet]),
    gen_event:notify(gcm_xmpp_handler, {data, ?SERVER, Packet#received_packet.raw_packet}),
    {noreply, State};

handle_info(Info, State) ->      % handle_info deals with out-of-band msgs, ie
    io:format("GCM - Handel Unknown Message~n~p~n", [Info]), % msgs that weren't sent via cast
    {noreply, State}.          % or call. Here we simply log such messages.

terminate(_Reason, _State) ->  % terminate is invoked by the gen_server
    io:format("GCM - terminating~n"), % container on shutdown.
    ok.                        % we log it and acknowledge with ok.

code_change(_OldVsn, State, _Extra) -> % called during release up/down-
    {ok, State}.               % grade to update internal state. 

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
session(MySession, _MyJID, Password) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession, "PLAIN")
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("GCM - Register~n"),
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, Password),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
	io:format("GCM - Ready~n"),
    %% We explicitely send presence, may not be needed with GCM:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Ready")),
	MySession.

sendMessage(MySession,RegId, Msg, MsgId) ->
	sendMessage(MySession,RegId, Msg, MsgId,0,false).
	
sendMessage(MySession,RegId, Msg, MsgId, TTL, DelayWhileIdle) ->

	MsgOut = exmpp_message:normal(),
	MsgOut3 = exmpp_xml:set_attribute(MsgOut,<<"to">>,<<"devices@gcm.googleapis.com">>),

	DataElement = exmpp_xml:element("google:mobile:data", "gcm"),
	
	Data = "{\"to\":\""++RegId++"\",\"message_id\":\""++MsgId++"\"\"data\":"++Msg++"\"time_to_live\":"++integer_to_list(TTL)++",\"delay_while_idle\":"++atom_to_list(DelayWhileIdle)++"}",

	DataElement2 = exmpp_xml:append_cdata(DataElement,Data),
	
	MsgOut4 = exmpp_xml:append_child(MsgOut3, DataElement2),
	io:format("GCM - Send Message~n~p~n",[MsgOut4]),
	exmpp_session:send_packet(MySession, MsgOut4)
	.

send_ack(MySession, ToRegId, MsgId) ->
	io:format("GCM - Send Ack Call~n"),
	%Prepare Data Payload
	DataOut = [{<<"to">>,ToRegId},{<<"message_id">>,MsgId},{<<"message_type">>,<<"ack">>}],
	NewData = jiffy:encode({DataOut}),
	%Create Message Stanza
	MsgOut = exmpp_message:normal(),
	MsgOut2 = exmpp_xml:set_attribute(MsgOut,<<"to">>,<<"devices@gcm.googleapis.com">>),
	%Create XML element containing the Payload	
	DataElement = exmpp_xml:element("google:mobile:data", "gcm"),
	DataElement2 = exmpp_xml:append_cdata(DataElement,NewData),
	%Add data element to message stanze
	MsgOut3 = exmpp_xml:append_child(MsgOut2, DataElement2),
	%Send
	exmpp_session:send_packet(MySession, MsgOut3)	
	.

