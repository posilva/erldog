%%
%% erldog.erl
%% erldog entry point
%%
-module(erldog).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    erldog_sup:start_link().

start() ->
    application:start(erldog).

stop() ->
    application:stop(erldog).

