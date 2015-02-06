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
-include("erldog_types.hrl").

%% API
-export([start_link/0]).

-export([
            validate/1,
            gauge/3,
            gauge/4
               ]).

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

-record(http_state, {
          dd_scheme = "https"             :: string(),
          dd_host   = "app.datadoghq.com" :: string(),
          dd_port   = 443                 :: non_neg_integer(),
          dd_path   = "/api/vi/"          :: string()
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
start_link()->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
    .
%%--------------------------------------------------------------------
%% @doc
%% validates the API Key
%%
%% @spec validate(_APIKey) ->{ok, _Response} | {error, _Error}
%% @end
%%--------------------------------------------------------------------
 
-spec validate(_APIKey) -> {ok, _Response}  | {error, _Error}.
validate(APIKey) ->
    gen_server:call(?MODULE, {validate,APIKey}, 5000).

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

gauge(Metric, Value, Host, Tags) ->
    metrics(Metric, [[ erldog_lib:unix_timestamp(), Value ]], Host, Tags,  "gauge").

gauge(Metric, Value, Tags) ->
    metrics(Metric, [[ erldog_lib:unix_timestamp(), Value ]], undefined, Tags, "gauge").



-spec metrics(Metric :: metric_t, Host :: string(), Tags :: list(binary()), MetricType :: metric_type_t, Points :: list({non_neg_integer,number}) ) -> {ok, _Response} | {error, _Error}.
metrics(Metric, Points, Host, Tags, MetricType) when is_list(Tags) ->
    gen_server:call(?MODULE, {metrics, Metric, Points, Host, Tags, MetricType, Points}, 5000).
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
    {ok,DatadogScheme} = application:get_env(dd_scheme),
    {ok,DatadogHost} = application:get_env(dd_host),
    {ok,DatadogPort} = application:get_env(dd_port),
    {ok,DatadogPath} = application:get_env(dd_path),
    State = #http_state{
                dd_scheme=DatadogScheme,
                dd_host=DatadogHost,
                dd_port=DatadogPort,
                dd_path=DatadogPath
                },

   %% lager:info("Parameters: ~p, ~p , ~p, ~p",[DatadogScheme,DatadogHost,DatadogPort,DatadogPath]),
    {ok, State}.

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
handle_call({metrics, Metric, Points, _Host, Tags, MetricType, Points}, _From, State) ->   
    {ok,APIKey} = application:get_env(dd_api_key),
    URL = base_url(State),
    ServiceUrl = URL ++ "series?api_key="++APIKey,
    lager:info(" Metrics API URL: ~p",[ServiceUrl]),
    Struct = [
        {<<"series">>, [[
            {<<"metric">>, list_to_binary(Metric) },
            {<<"points">>, Points},
            {<<"type">>, list_to_binary(MetricType)  },
            {<<"tags">>, Tags}
        ]]}
    ],
     %%{<<"host">>, list_to_binary(Host) },
    JSON = jsx:encode(Struct),

  %%  lager:info("Method call URL: ~p",[ServiceUrl]),
    Reply = case lhttpc:request(ServiceUrl , post,[], JSON, 1000) of
        {ok, {{202, _}, _, Body}} ->
            Response = jsx:decode(Body),
            {ok,Response};
        { _,Other} ->
            {error,Other}
    end,
    {reply, Reply, State};

handle_call({validate, APIKey}, _From, State) ->    

    URL = base_url(State),
    ServiceUrl = URL ++ "validate?api_key="++APIKey,
    lager:info("Validate  APIKey URL: ~p",[ServiceUrl]),
    Reply = case lhttpc:request(ServiceUrl , get, [], 1000) of
        {ok, {{200, _}, _, Body}} ->
            Response = jsx:decode(Body),
            {ok,Response};
        { _,Other} ->
            {error,Other}
    end,

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
base_url(HttpState) when is_record(HttpState,http_state) ->
    URL = HttpState#http_state.dd_scheme ++"://" ++ 
            HttpState#http_state.dd_host  ++ ":" ++ 
            integer_to_list(HttpState#http_state.dd_port) ++ 
            HttpState#http_state.dd_path,
    URL.
    
