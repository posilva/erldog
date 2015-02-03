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
-type optional(V)    :: V | undefined.
-type proplist(K, V) :: [{K, V}].

%% define some helper types for return types
-type success(V)     :: {ok, V}.
-type success()      :: ok.
-type fail(E)        :: {error, E}.
