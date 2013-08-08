%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2013, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2013 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-module(p1_yaml).

%% API
-export([load_nif/0, load_nif/1, decode/1, decode/2, start/0, stop/0,
         decode_file/1, decode_file/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type option() :: {plain_as_atom, boolean()}.
-type options() :: [option()].

-define(PLAIN_AS_ATOM, 1).

-record(mark, {index = 0  :: non_neg_integer(),
               line = 0   :: non_neg_integer(),
               column = 0 :: non_neg_integer()}).

-type mark() :: #mark{}.

-record(document_start, {start_mark = #mark{} :: mark(),
                         end_mark = #mark{}   :: mark(),
                         version_directive    :: {integer(), integer()},
                         implicit = true      :: boolean()}).

-record(document_end, {start_mark = #mark{} :: mark(),
                       end_mark = #mark{}   :: mark(),
                       implicit = true      :: boolean()}).

-type scalar_style() :: plain | single_quoted |
                        double_quoted | literal |
                        folded.

-type encoding() :: utf8 | 'utf16-le' | 'utf16-be'.

-record(scalar, {start_mark = #mark{}    :: mark(),
                 end_mark = #mark{}      :: mark(),
                 style = plain           :: scalar_style(),
                 anchor = <<>>           :: binary(),
                 tag = <<>>              :: binary(),
                 value = <<>>            :: binary() | atom(),
                 plain_implicit = true   :: boolean(),
                 quoted_implicit = false :: boolean()}).

-record(alias, {start_mark = #mark{} :: mark(),
                end_mark = #mark{}   :: mark(),
                anchor = <<>>        :: binary()}).

-record(stream_start, {start_mark = #mark{} :: mark(),
                       end_mark = #mark{}   :: mark(),
                       encoding = utf8      :: encoding()}).

-record(stream_end, {start_mark = #mark{} :: mark(),
                     end_mark = #mark{}   :: mark()}).

-record(mapping_end, {start_mark = #mark{} :: mark(),
                      end_mark = #mark{}   :: mark()}).

-record(sequence_end, {start_mark = #mark{} :: mark(),
                       end_mark = #mark{}   :: mark()}).

-record(sequence_start, {start_mark = #mark{} :: mark(),
                         end_mark = #mark{}   :: mark(),
                         style = block        :: block | flow,
                         anchor = <<>>        :: binary(),
                         tag = <<>>           :: binary(),
                         implicit = true      :: boolean()}).

-record(mapping_start, {start_mark = #mark{} :: mark(),
                        end_mark = #mark{}   :: mark(),
                        style = block        :: block | flow,
                        anchor = <<>>        :: binary(),
                        tag = <<>>           :: binary(),
                        implicit = true      :: boolean()}).

-record(stream_error, {mark = #mark{} :: mark(),
                       reason = <<>>  :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(p1_yaml).

stop() ->
    application:stop(p1_yaml).

load_nif() ->
    load_nif(get_so_path()).

load_nif(LibDir) ->
    SOPath = filename:join(LibDir, "p1_yaml"),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load p1_yaml NIF: ~p~n", [Err]),
            Err
    end.

-spec decode(iodata()) -> {ok, term()} | {error, binary()}.

decode(Data) ->
    decode(Data, []).

-spec decode_file(string()) -> {ok, term()} | {error, binary()}.

decode_file(File) ->
    decode_file(File, []).

-spec decode_file(string(), options()) -> {ok, term()} | {error, binary()}.

decode_file(File, Opts) ->
    case file:read_file(File) of
        {ok, Data} ->
            decode(Data, Opts);
        Err ->
            Err
    end.

-spec decode(iodata(), options()) -> {ok, term()} | {error, binary()}.

decode(Data, Opts) ->
    case nif_decode(Data, make_flags(Opts)) of
        [#stream_error{reason = Reason}|_] ->
            {error, Reason};
        Events ->
            case process_events(Events, []) of
                [] -> {ok, []};
                [Root] -> {ok, Root}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_events([#stream_end{}|Events], Terms) ->
    process_events(Events, Terms);
process_events([#stream_start{}], Terms) ->
    Terms;
process_events([#document_end{}|Events], Terms) ->
    process_events(Events, Terms);
process_events([#document_start{}|Events], Terms) ->
    process_events(Events, Terms);

process_events([#mapping_end{}|Events], Terms) ->
    {Elems, NewEvents} = process_events(Events, []),
    process_events(NewEvents, [Elems|Terms]);
process_events([#mapping_start{}|Events], Elems) ->
    {zip(Elems), Events};

process_events([#sequence_end{}|Events], Terms) ->
    {Elems, NewEvents} = process_events(Events, []),
    process_events(NewEvents, [Elems|Terms]);
process_events([#sequence_start{}|Events], Elems) ->
    {Elems, Events};

process_events([#scalar{value = V}|Events], Terms) ->
    process_events(Events, [V|Terms]);
process_events([#alias{anchor = V}|Events], Terms) ->
    process_events(Events, [V|Terms]).

zip([E1,E2|Es]) ->
    [{E1,E2}|zip(Es)];
zip([]) ->
    [].

get_so_path() ->
    case os:getenv("EJABBERD_SO_PATH") of
        false ->
            case code:priv_dir(p1_yaml) of
                {error, _} ->
                    filename:join(["priv", "lib"]);
                Path ->
                    filename:join([Path, "lib"])
            end;
        Path ->
            Path
    end.

make_flags([{plain_as_atom, true}|Opts]) ->
    ?PLAIN_AS_ATOM bor make_flags(Opts);
make_flags([{plain_as_atom, false}|Opts]) ->
    make_flags(Opts);
make_flags([plain_as_atom|Opts]) ->
    ?PLAIN_AS_ATOM bor make_flags(Opts);
make_flags([Opt|Opts]) ->
    error_logger:warning_msg("p1_yaml: unknown option ~p", [Opt]),
    make_flags(Opts);
make_flags([]) ->
    0.

nif_decode(_Data, _Flags) ->
    error_logger:error_msg("p1_yaml NIF not loaded", []),
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).

load_nif_test() ->
    ?assertEqual(ok, load_nif(filename:join(["..", "priv", "lib"]))).

-endif.
