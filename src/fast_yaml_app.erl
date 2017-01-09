%%%----------------------------------------------------------------------
%%% File    : fast_yaml_app.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : YAML parser application
%%% Created : 7 Aug 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%----------------------------------------------------------------------

-module(fast_yaml_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case fast_yaml:load_nif() of
        ok ->
            fast_yaml_sup:start_link();
        Err ->
            Err
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
