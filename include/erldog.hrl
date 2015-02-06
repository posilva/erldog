%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <pedro.silva@I.lan>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2015 by Pedro Marques da Silva <pedro.silva@I.lan>
%%%-------------------------------------------------------------------
-module(erldog).



%% define some helper types for spec
-type optional(V)       :: V | undefined.
-type proplist(K, V)    :: [{K, V}].

%% define some helper types for return types
-type success(V)        :: {ok, V}.
-type success()         :: ok.
-type fail(E)           :: {error, E}.

-type metric_t ()       :: string() | binary().
-type metric_type_t()   :: "gauge" | "counter". 
%%erldog_http:gauge("erldog.test.metric",[[1423182075,123]], "splash001a",[]).

%% Get unix timestamp 
unix_timestamp()->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.