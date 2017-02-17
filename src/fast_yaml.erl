%%%----------------------------------------------------------------------
%%% File    : fast_yaml.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : YAML parser
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

-module(fast_yaml).

-compile(no_native).

%% API
-export([load_nif/0, decode/1, decode/2, start/0, stop/0,
         decode_from_file/1, decode_from_file/2, encode/1, format_error/1]).

-type option() :: {plain_as_atom, boolean()} | plain_as_atom.
-type options() :: [option()].
-type parser_error() :: {parser_error, binary(), integer(), integer()}.
-type scanner_error() :: {scanner_error, binary(), integer(), integer()}.
-type yaml_error() :: parser_error() | scanner_error() |
                      memory_error | unexpected_error.

-define(PLAIN_AS_ATOM, 1).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(fast_yaml).

stop() ->
    application:stop(fast_yaml).

load_nif() ->
    SOPath = p1_nif_utils:get_so_path(?MODULE, [fast_yaml], "fast_yaml"),
    load_nif(SOPath).

load_nif(SOPath) ->
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load fast_yaml NIF: ~p~n", [Err]),
            Err
    end.

-spec format_error(atom() | yaml_error() | file:posix()) -> string().

format_error({Tag, Reason, Line, Column}) when Tag == parser_error;
                                               Tag == scanner_error ->
    lists:flatten(
      io_lib:format(
        "Syntax error on line ~p at position ~p: ~s",
        [Line+1, Column+1, Reason]));
format_error(memory_error) ->
    "Memory error";
format_error(Reason) when is_atom(Reason) ->
    file:format_error(Reason);
format_error(_) ->
    "Unexpected error".

-spec decode(iodata()) -> {ok, term()} | {error, yaml_error()}.

decode(Data) ->
    decode(Data, []).

-spec decode_from_file(file:filename()) -> {ok, term()} |
					   {error, yaml_error() | file:posix()}.

decode_from_file(File) ->
    decode_from_file(File, []).

-spec decode_from_file(file:filename(), options()) -> {ok, term()} |
						      {error, yaml_error() | file:posix()}.

decode_from_file(File, Opts) ->
    case file:read_file(File) of
        {ok, Data} ->
            decode(Data, Opts);
        Err ->
            Err
    end.

-spec decode(iodata(), options()) -> {ok, term()} | {error, yaml_error()}.

decode(Data, Opts) ->
    nif_decode(Data, make_flags(Opts)).

-spec encode(term()) -> iolist().

encode(Term) ->
    NL = io_lib:nl(),
    case encode(Term, 0) of
        [[NL|T1]|T2] ->
            [T1|T2];
        T ->
            T
    end.

encode([{_, _}|_] = Terms, N) ->
    [[io_lib:nl(), indent(N), encode_pair(T, N)] || T <- Terms];
encode([_|_] = Terms, N) ->
    [[io_lib:nl(), indent(N), "- ", encode(T, N+2)] || T <- Terms];
encode([], _) ->
    "[]";
encode(I, _) when is_integer(I) ->
    integer_to_list(I);
encode(F, _) when is_float(F) ->
    io_lib:format("~f", [F]);
encode(A, _) when is_atom(A) ->
    atom_to_list(A);
encode(B, _) when is_binary(B) ->
    [$",
     lists:map(
       fun($") -> [$\\, $"];
          (C) -> C
       end, binary_to_list(B)),
     $"].

encode_pair({K, V}, N) ->
    [encode(K), ": ", encode(V, N+2)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_flags([{plain_as_atom, true}|Opts]) ->
    ?PLAIN_AS_ATOM bor make_flags(Opts);
make_flags([{plain_as_atom, false}|Opts]) ->
    make_flags(Opts);
make_flags([plain_as_atom|Opts]) ->
    ?PLAIN_AS_ATOM bor make_flags(Opts);
make_flags([Opt|Opts]) ->
    error_logger:warning_msg("fast_yaml: unknown option ~p", [Opt]),
    make_flags(Opts);
make_flags([]) ->
    0.

nif_decode(_Data, _Flags) ->
    error_logger:error_msg("fast_yaml NIF not loaded", []),
    erlang:nif_error(nif_not_loaded).

indent(N) ->
    lists:duplicate(N, $ ).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

load_nif_test() ->
    SOPath = p1_nif_utils:get_so_path(?MODULE, [], "fast_yaml"),
    ?assertEqual(ok, load_nif(SOPath)).

decode_test1_test() ->
    FileName = filename:join(["..", "test", "test1.yml"]),
    ?assertEqual(
       {ok,[[{<<"Time">>,<<"2001-11-23 15:01:42 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Warning">>,
              <<"This is an error message for the log file">>}],
            [{<<"Time">>,<<"2001-11-23 15:02:31 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Warning">>,<<"A slightly different error message.">>}],
            [{<<"Date">>,<<"2001-11-23 15:03:17 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Fatal">>,<<"Unknown variable \"bar\"">>},
             {<<"Stack">>,
              [[{<<"file">>,<<"TopClass.py">>},
                {<<"line">>,23},
                {<<"code">>,<<"x = MoreObject(\"345\\n\")\n">>}],
               [{<<"file">>,<<"MoreClass.py">>},
                {<<"line">>,58},
                {<<"code">>,<<"foo = bar">>}]]}]]},
       decode_from_file(FileName)).

decode_test2_test() ->
    FileName = filename:join(["..", "test", "test2.yml"]),
    ?assertEqual(
       {ok,[[[{step,[{instrument,<<"Lasik 2000">>},
                     {pulseEnergy,5.4},
                     {pulseDuration,12},
                     {repetition,1000},
                     {spotSize,<<"1mm">>}]}],
             [{step,[{instrument,<<"Lasik 2000">>},
                     {pulseEnergy,5.0},
                     {pulseDuration,10},
                     {repetition,500},
                     {spotSize,<<"2mm">>}]}],
             [{step,<<"id001">>}],
             [{step,<<"id002">>}],
             [{step,<<"id001">>}],
             [{step,<<"id002">>}]]]},
       decode_from_file(FileName, [plain_as_atom])).

decode_test3_test() ->
    FileName = filename:join(["..", "test", "test3.yml"]),
    ?assertEqual(
       {ok,[[{<<"a">>,123},
             {<<"b">>,<<"123">>},
             {<<"c">>,123.0},
             {<<"d">>,123},
             {<<"e">>,123},
             {<<"f">>,<<"Yes">>},
             {<<"g">>,<<"Yes">>},
             {<<"h">>,<<"Yes we have No bananas">>}]]},
       decode_from_file(FileName)).

decode_test4_test() ->
    FileName = filename:join(["..", "test", "test4.yml"]),
    ?assertEqual(
       {ok,[[{<<"picture">>,
              <<"R0lGODlhDAAMAIQAAP//9/X\n17unp5WZmZgAAAOfn515eXv\n"
                "Pz7Y6OjuDg4J+fn5OTk6enp\n56enmleECcgggoBADs=mZmE\n">>}]]},
       decode_from_file(FileName)).

-endif.
