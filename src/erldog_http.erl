%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <pedro.silva@I.lan>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2015 by Pedro Marques da Silva <pedro.silva@I.lan>
%%%-------------------------------------------------------------------
-module(erldog_http).
-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([validate/1, metrics/1, events/1]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(URL(S, H, P, Pa), S ++ "://" ++ H ++ ":" ++ integer_to_list(P) ++ Pa).

-record(http_state, {
  url :: string()
}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, _Pid} | ignore | {error, _Error}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% validates the API Key
%%
%% @spec validate(_APIKey) ->{ok, _Response} | {error, _Error}
%% @end
%%--------------------------------------------------------------------

-spec validate(_APIKey) -> {ok, _Response}  | {error, _Error}.
validate(APIKey) ->
  gen_server:call(?MODULE, {validate, APIKey}).

%%--------------------------------------------------------------------
%% @doc
%% 
%% The metrics end-point allows you to post metrics data so it can
%%    be graphed on Datadog's dashboards.
%%
%%
%% @spec metrics(_APIKey) ->{ok, _Response} | {error, _Error}
%% @end
%%--------------------------------------------------------------------
-spec metrics(Metric :: list(map()) | map()) -> {ok, _Response} | {error, _Error}.
metrics(Metrics) when is_list(Metrics) ->
  gen_server:call(?MODULE, {metrics, Metrics});
metrics(Metric) ->
  metrics([Metric]).


%%--------------------------------------------------------------------
%% @doc
%% The events service allows you to programatically post events to the
%% stream and fetch events from the stream.
%% @end
%%--------------------------------------------------------------------
-spec events(map()) -> {ok, _Response} | {error, _Error}.
events(Event) ->
  gen_server:call(?MODULE, {event, Event}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, Scheme} = application:get_env(dd_scheme),
  {ok, Host} = application:get_env(dd_host),
  {ok, Port} = application:get_env(dd_port),
  {ok, Path} = application:get_env(dd_path),
  {ok, #http_state{url = ?URL(Scheme, Host, Port, Path)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({metrics, Metrics}, _, State = #http_state{url = URL}) ->
  {ok, APIKey} = application:get_env(dd_api_key),
  ServiceUrl = URL ++ "series?api_key=" ++ APIKey,
  Struct = #{<<"series">> => Metrics},
  JSON = jsx:encode(Struct),
  Reply = reply(lhttpc:request(ServiceUrl, post, [], JSON, 1000)),
  {reply, Reply, State};
handle_call({event, Event}, _, State = #http_state{url = URL}) ->
  {ok, APIKey} = application:get_env(dd_api_key),
  ServiceUrl = URL ++ "events?api_key=" ++ APIKey,
  JSON = jsx:encode(Event),
  Reply = reply(lhttpc:request(ServiceUrl, post, [], JSON, 1000)),
  {reply, Reply, State};
handle_call({validate, APIKey}, _, State = #http_state{url = URL}) ->
  ServiceUrl = URL ++ "validate?api_key=" ++ APIKey,
  Reply = reply(lhttpc:request(ServiceUrl, get, [], 1000)),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
reply({ok, {{200, _}, _, Body}}) ->
  Response = jsx:decode(Body),
  {ok, Response};
reply({_, Other}) ->
  {error, Other}.