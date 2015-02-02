%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <pedro.silva@I.lan>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2015 by Pedro Marques da Silva <pedro.silva@I.lan>
%%%-------------------------------------------------------------------
-module(erldog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
	DataStore = 'erldog_ets_store',
	HttpModule = 'erldog_http',
    DataStoreChild = {DataStore, {DataStore, start_link, []}, Restart, Shutdown, Type, [DataStore]},
	HttpChild = {HttpModule, {HttpModule, start_link, []}, Restart, Shutdown, Type, [HttpModule]},

    {ok, {SupFlags, [DataStoreChild, HttpChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
