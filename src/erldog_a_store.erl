%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <pedro.silva@I.lan>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%% This module is a frontend for a generic store api 
%%% @end
%%% Created : 25 Jan 2015 by Pedro Marques da Silva <pedro.silva@I.lan>
%%%-------------------------------------------------------------------
-module(erldog_a_store).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: this must go to a header 
%%%%%%%%%
%% generic result
-type gen_result() ::  'ok' | tuple('error', Reason :: string()). 

%% metrics type specification
-type metric_name() :: atom() | string() | binary().
-type metric_type() :: counter | gauge | histogram | set.
-type metric_value() :: number().

%% API
-callback init_store(Args :: list(term())) -> gen_result().
-callback store({Metric :: metric_name(), Type :: metric_type(), Value :: metric_value()}) -> gen_result().
                                                                                             

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
