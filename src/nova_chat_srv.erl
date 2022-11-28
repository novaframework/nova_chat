%%%-------------------------------------------------------------------
%%% @author daniel <daniel@daniel-G501JW>
%%% @copyright (C) 2019, daniel
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2019 by daniel <daniel@daniel-G501JW>
%%%-------------------------------------------------------------------
-module(nova_chat_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 publish/2,
	 subscribe/2,
	 unsubscribe/2,
	 online/2,
	 offline/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("../nova/include/nova_pubsub.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

publish(Topic, Body) ->
	nova_pubsub:broadcast(chat, <<"message">>, {Topic, Body}).

subscribe(Topic, User) ->
    gen_server:call(?MODULE, {subscribe, Topic, User}).

unsubscribe(User, Topic) ->
    gen_server:call(?MODULE, {unsubscribe, User, Topic}).

online(User, Socket) ->
    gen_server:call(?MODULE, {online, User, Socket}).

offline(User, Socket) ->
    gen_server:call(?MODULE, {offline, User, Socket}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init([]) ->
    process_flag(trap_exit, true),
    self() ! start,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call({subscribe, Topic, User}, _, State) ->
    true = ets:insert(subscribers, {Topic, User}),
    {reply, ok, State};
handle_call({unsubscribe, User, Topic}, _, State) ->
    case ets:match_object(subscribers, {Topic, User}) of
	[{Topic, User}] ->
	    true = ets:delete_object(subscribers, {Topic, User}),
	    {reply, ok, State};
	_ ->
	    logger:info("Nothing to unsubscibe user: ~p topic: ~p~n", [User, Topic]),
	    {reply, ok, State}
    end;
handle_call({online, User, Socket}, _, State) ->
    true = ets:insert(online, {User, Socket}),
    {reply, ok, State};
handle_call({offline, User, Socket}, _, State) ->
    true = ets:delete_object(online, {User, Socket}),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(start, State) ->
    ets:new(channels, [named_table, bag]),
    ets:new(subscribers, [named_table, bag]),
    ets:new(online, [named_table, bag]),
	nova_pubsub:join(chat, self()),
    {noreply, State};
handle_info(#nova_pubsub{topic = <<"message">>, payload = {Topic, Body}}, State) ->
    Subscribers = ets:match(subscribers, {'$1', Topic}),
    [send(User, Body, online_sockets(User)) || [User] <- Subscribers],
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(_, _, []) ->
    ok;
send(_User, Body, Sockets) ->
    [Socket ! Body || [Socket] <- Sockets].

online_sockets(User) ->
    ets:match(online, {User, '$1'}).
