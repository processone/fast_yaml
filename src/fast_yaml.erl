%%%----------------------------------------------------------------------
%%% File    : fast_yaml.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : YAML parser
%%% Created : 7 Aug 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2020 ProcessOne, SARL. All Rights Reserved.
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

-on_load(load_nif/0).

%% API
-export([decode/1, decode/2, start/0, stop/0,
         decode_from_file/1, decode_from_file/2, encode/1, format_error/1]).

-type option() :: {plain_as_atom, boolean()} | plain_as_atom | {sane_scalars, boolean()} | sane_scalars.
-type options() :: [option()].
-type parser_error() :: {parser_error, binary(), integer(), integer()}.
-type scanner_error() :: {scanner_error, binary(), integer(), integer()}.
-type yaml_error() :: parser_error() | scanner_error() |
                      memory_error | unexpected_error.

-define(PLAIN_AS_ATOM, 1).
-define(SANE_SCALARS, 2).
-define(MAPS, 4).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(fast_yaml).

stop() ->
    application:stop(fast_yaml).

load_nif() ->
    SOPath = p1_nif_utils:get_so_path(?MODULE, [fast_yaml], "fast_yaml"),
    erlang:load_nif(SOPath, 0).

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
    case file:format_error(Reason) of
	"unknown POSIX error" ->
	    atom_to_list(Reason);
	Res ->
	    Res
    end;
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
    try nif_decode(Data, make_flags(Opts))
    catch error:{parser_error, _, _, _} = Reason ->
	    {error, Reason}
    end.

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
                                                % http://erlang.org/doc/reference_manual/data_types.html#escape-sequences
encode(B, _) when is_binary(B) ->
    [$",
     lists:map(
       fun ($\b) -> [$\\, "b"];  % $\b ==  "backspace"
                                                %($\d) -> [$\\, "d"];  % $\d = "delete" % Encode work, but decode fail
           ($\e) -> [$\\, "e"];  % $\e ==  "escape"
           ($\f) -> [$\\, "f"];  % $\f ==  "from feed"
           ($\n) -> [$\\, "n"];  % $\n == "new line"
           ($\r) -> [$\\, "r"];  % $\r == "carriage return"
           ($\s) -> [$\s];  % $\s ==  "space"
           ($\t) -> [$\\, "t"];  % $\t ==  "tab"
           ($\v) -> [$\\, "v"];  % $\v ==  "vertical tab"
           ($") -> [$\\, $"];    % $"  ==  double quote
           ($\\) -> [$\\, $\\];  % $\\ ==  backslash
           (C) -> C
       end, unicode:characters_to_list(B)),
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
make_flags([{sane_scalars, true}|Opts]) ->
    ?SANE_SCALARS bor make_flags(Opts);
make_flags([{sane_scalars, false}|Opts]) ->
    make_flags(Opts);
make_flags([sane_scalars|Opts]) ->
    ?SANE_SCALARS bor make_flags(Opts);
make_flags([{maps, true}|Opts]) ->
    ?MAPS bor make_flags(Opts);
make_flags([{maps, false}|Opts]) ->
    make_flags(Opts);
make_flags([maps|Opts]) ->
    ?MAPS bor make_flags(Opts);
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

test_file_path(File) ->
    F1 = filename:join(["test", File]),
    case filelib:is_file(F1) of
        true -> F1;
        _ -> filename:join(["..", "test", File])
    end.

temp_file_path() ->
    case filelib:is_dir("test") of
        true ->
            filename:join(["test", "temp_test.yml"]);
        _ ->
            filename:join(["..", "test", "temp_test.yml"])
    end.

decode_test1_test() ->
    FileName = test_file_path("test1.yml"),
    %?assertEqual(file:get_cwd(), "dupa"),
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
    FileName = test_file_path("test2.yml"),
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
    FileName = test_file_path("test3.yml"),
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
    FileName = test_file_path("test4.yml"),
    ?assertEqual(
       {ok,[[{<<"picture">>,
              <<"R0lGODlhDAAMAIQAAP//9/X\n17unp5WZmZgAAAOfn515eXv\n"
                "Pz7Y6OjuDg4J+fn5OTk6enp\n56enmleECcgggoBADs=mZmE\n">>}]]},
       decode_from_file(FileName)).

decode_test5_test() ->
    FileName = test_file_path("test5.yml"),
    ?assertEqual(
        {ok,[[
            {<<"Name">>,<<"Backslash">>},
                {<<"Source">>,<<"\\\\\\\\">>}],
            [{<<"Name">>,<<"Double_Quote">>},
                {<<"Source">>,<<"\"\"">>}],
            [{<<"Name">>,<<"Backslash_and_Double_Quote">>},
                {<<"Source">>,<<"\"\\\"\"">>}],
            [{<<"Name">>,<<"New_Line">>},
                {<<"Source">>,<<"\\n">>}]]
        },
        decode_from_file(FileName)).


decode_test6_test() ->
    FileName = test_file_path("test6.yml"),
    ?assertEqual(
       {ok,[[{<<"ints">>, [1, 2, 3]},
             {<<"value">>, true}
            ],
            [{<<"true">>, true},
             {<<"false">>, false},
             {<<"str">>, <<"123">>},
             {<<"str2">>, <<"123">>},
             {<<"int">>, 123},
             {<<"null">>, undefined},
             {<<"null2">>, undefined},
             {<<"null3">>, undefined}
            ],
            [{<<"inbox">>,
              [
               {<<"enabled">>, true},
               {<<"filters">>, [[{<<"icon">>, <<"inbox">>}, {<<"label">>, <<"Inbox">>}]]}
              ]}]
           ]
       },
       decode_from_file(FileName, [sane_scalars])).

decode_test7_test() ->
    FileName = test_file_path("test7.yml"),
    ?assertEqual(
       {ok,[#{<<"foo">> =>
                  #{<<"true">> => true,
                    <<"false">> => false,
                    <<"str">> => <<"123">>,
                    <<"str2">> => <<"123">>,
                    <<"int">> => 123,
                    <<"null">> => undefined
                   }},
            #{<<"inbox">> =>
                  #{<<"enabled">> => true,
                    <<"map">> => #{<<"value">> => 1},
                    <<"height">> => 100,
                    <<"filters">> =>
                        [#{<<"icon">> => <<"inbox">>,
                           <<"label">> => <<"Inbox">>
                          }
                        ]
                   }
             }]},
       decode_from_file(FileName, [sane_scalars, maps])).

decode_test8_test() ->
    FileName = test_file_path("test8.yml"),
    {ok, Contents} = file:read_file(FileName),
    ?assertMatch({ok,[#{}]}, decode(Contents, [sane_scalars, maps])).

encode_test1_test() ->
    ?assertEqual(
       list_to_binary(encode(<<"a">>)),
       <<"\"a\"">>).

encode_unicode_test1_test() ->
    ?assertEqual(
       unicode:characters_to_binary(encode(<<"☃"/utf8>>)),
       <<"\"☃\""/utf8>>).

encode_decode_backspace_test() ->
    FileName = temp_file_path(),
    Binary = <<"abc\bdef">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_escape_test() ->
    FileName = temp_file_path(),
    Binary = <<"\en">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_from_feed_test() ->
    FileName = temp_file_path(),
    Binary = <<"\f\"">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_new_line_test() ->
    FileName = temp_file_path(),
    Binary = <<"\n">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_carriage_return_test() ->
    FileName = temp_file_path(),
    Binary = <<"ref\r\n">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_space_test() ->
    FileName = temp_file_path(),
    Binary = <<" toto\stata \s ">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_tab_test() ->
    FileName = temp_file_path(),
    Binary = <<"\treturn True">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_vertical_tab_test() ->
    FileName = temp_file_path(),
    Binary = <<"\v  ok">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_simple_quote_test() ->
    FileName = temp_file_path(),
    Binary = <<"'\"'">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_double_quote_test() ->
    FileName = temp_file_path(),
    Binary = <<"\"\"">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_backslash_test() ->
    FileName = temp_file_path(),
    Binary = <<"\\\\\\\\">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

encode_decode_quote_and_backslash_test() ->
    FileName = temp_file_path(),
    Binary = <<"\"\\\"\"">>,
    Encoded = encode([[{'Source', Binary}]]),
    file:write_file(FileName, Encoded),
    Decoded = decode_from_file(FileName, [plain_as_atom]),
    file:delete(FileName),
    {ok, [[[{'Source', DecodedBinary}]]]} = Decoded,
    ?assertEqual(
       DecodedBinary,
       Binary
      ).

-endif.
