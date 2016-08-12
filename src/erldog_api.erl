%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2016 13:41
%%%-------------------------------------------------------------------
-module(erldog_api).
-author("tihon").

-include("erldog_types.hrl").

%% API
-export([create_points/1, send_metric/1, create_metrics/5, send_event/1]).
-export([create_counter/2, create_counter/3, create_counter/4]).
-export([create_gauge/2, create_gauge/3, create_gauge/4]).
-export([create_histogram/2, create_histogram/3, create_histogram/4]).
-export([create_set/2, create_set/3, create_set/4]).

create_counter(Name, Values) ->
  create_counter(Name, Values, undefined).

create_counter(Name, Values, Host) ->
  create_counter(Name, Values, Host, []).

-spec create_counter(Name :: metric_t(), Points :: list(list()) | number(), Host :: binary(), Tags :: list(binary())) ->
  any().
create_counter(Name, Value, Host, Tags) when is_number(Value) ->
  create_counter(Name, create_points(Value), Host, Tags);
create_counter(Name, Points, Host, Tags) ->
  create_metrics(Name, Points, Host, Tags, ?COUNTER).

create_gauge(Name, Values) ->
  create_gauge(Name, Values, undefined).

create_gauge(Name, Values, Host) ->
  create_gauge(Name, Values, Host, []).

-spec create_gauge(Name :: metric_t(), Points :: list(list()) | number(), Host :: binary(), Tags :: list(binary())) ->
  any().
create_gauge(Name, Value, Host, Tags) when is_number(Value) ->
  create_gauge(Name, create_points(Value), Host, Tags);
create_gauge(Name, Points, Host, Tags) ->
  create_metrics(Name, Points, Host, Tags, ?GAUGE).

create_histogram(Name, Values) ->
  create_histogram(Name, Values, undefined).

create_histogram(Name, Values, Host) ->
  create_histogram(Name, Values, Host, []).

-spec create_histogram(Name :: metric_t(), Points :: list(list()) | number(), Host :: binary(), Tags :: list(binary())) ->
  any().
create_histogram(Name, Value, Host, Tags) when is_number(Value) ->
  create_histogram(Name, create_points(Value), Host, Tags);
create_histogram(Name, Points, Host, Tags) ->
  create_metrics(Name, Points, Host, Tags, ?HISTOGRAM).

create_set(Name, Values) ->
  create_set(Name, Values, undefined).

create_set(Name, Values, Host) ->
  create_set(Name, Values, Host, []).

-spec create_set(Name :: metric_t(), Points :: list(list()) | number(), Host :: binary(), Tags :: list(binary())) ->
  any().
create_set(Name, Value, Host, Tags) when is_number(Value) ->
  create_set(Name, create_points(Value), Host, Tags);
create_set(Name, Points, Host, Tags) ->
  create_metrics(Name, Points, Host, Tags, ?SET).

-spec create_metrics(Name :: metric_t(), Points :: list(list()), Host :: binary(), Tags :: list(binary()), Type :: metric_type_t()) ->
  any().
create_metrics(Name, Points, Host, Tags, Type) when is_list(Name) ->
  create_metrics(list_to_binary(Name), Points, Host, Tags, Type);
create_metrics(Name, Points, Host, Tags, Type) ->
  Base = #{
    <<"metric">> => Name,
    <<"points">> => Points,
    <<"type">> => Type
  },
  with_tags(with_host(Base, Host), Tags).

-spec create_points(list(number())) -> list(list()).
create_points(Values) when is_list(Values) ->
  lists:map(fun(Value) -> [erldog_lib:unix_timestamp(), Value] end, Values);
create_points(Value) ->
  create_points([Value]).

-spec send_metric(map() | list(map())) -> {ok, _Response} | {error, _Error}.
send_metric(Metrics) ->
  erldog_http:metrics(Metrics).

send_event(#dd_event{title = Title, text = Text, date_happened = Date, priority = Priority,
  host = Host, tags = Tags, alert_type = Alert, aggregation_key = Key, source_type_name = Source}) ->
  Base = #{<<"title">> => Title, <<"text">> => Text, <<"date_happened">> => Date,
    <<"priority">> => Priority, <<"alert_type">> => Alert},
  erldog_http:events(
    with_host(
      with_tags(
        with_aggregation_key(
          with_source_type_name(Base, Source),
          Key),
        Tags),
      Host)).

%% @private
with_host(Body, undefined) -> Body;
with_host(Body, Host) -> Body#{<<"host">> => Host}.

%% @private
with_tags(Body, []) -> Body;
with_tags(Body, Tags) -> Body#{<<"tags">> => Tags}.

%% @private
with_source_type_name(Body, undefined) -> Body;
with_source_type_name(Body, Tags) -> Body#{<<"source_type_name">> => Tags}.

%% @private
with_aggregation_key(Body, undefined) -> Body;
with_aggregation_key(Body, Tags) -> Body#{<<"aggregation_key">> => Tags}.