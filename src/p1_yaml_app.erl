%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2013, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2013 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-module(p1_yaml_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case p1_yaml:load_nif() of
        ok ->
            p1_yaml_sup:start_link();
        Err ->
            Err
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
