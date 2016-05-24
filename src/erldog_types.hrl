%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <pedro.silva@I.lan>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2015 by Pedro Marques da Silva <pedro.silva@I.lan>
%%%-------------------------------------------------------------------

%% define some helper types for spec
-type optional(V) :: V | undefined.
-type proplist(K, V) :: [{K, V}].

%% define some helper types for return types
-type success(V) :: {ok, V}.
-type success() :: ok.
-type fail(E) :: {error, E}.

-type metric_t() :: binary().
-type metric_type_t() :: binary().

-export_type([metric_t/0, metric_type_t/0]).

-define(GAUGE, <<"gauge">>).
-define(COUNTER, <<"counter">>).
-define(HISTOGRAM, <<"histogram">>).
-define(SET, <<"set">>).

-record(dd_event,
{
  title :: binary(),
  text :: binary(),
  date_happened = erldog_lib:unix_timestamp() :: integer(),
  priority = <<"normal">> :: binary(),
  host :: binary(),
  tags :: list(),
  alert_type = <<"info">> :: binary(),
  aggregation_key,
  source_type_name
}).