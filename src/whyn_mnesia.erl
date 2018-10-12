-module(whyn_mnesia).
-import(lists, [foreach/2]).

% -compile(export_all).
-export([do_this_once/0]).
-export([start/0]).
-export([start2/0]).
-export([stop/0]).
-export([do/1]).
-export([all/1]).
-export([ets_delete/1]).

% if we want to call qlc:q(...)
-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").
% define table keys
% -record(base, {key, sex, age, id, square, gps, status, date}).
% -record(changes, {id, square, type, gps}).

do_this_once() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(keys, [{type, set}, {attributes, record_info(fields, keys)}]),
	mnesia:create_table(positions, [{type, set}, {attributes, record_info(fields, positions)}]),
	mnesia:create_table(cache, [{type, set}, {attributes, record_info(fields, cache)}]),
	mnesia:create_table(snaps, [{type, set}, {attributes, record_info(fields, snaps)}]),
	mnesia:stop().

start() ->
	mnesia:start(),
	mnesia:wait_for_tables([keys, positions, cache], 20000).

start2() ->
	application:start(mnesia, permanent),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

stop() ->
	mnesia:stop().

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

% reset_tables() ->
% 	mnesia:clear_table(keys)
% 	F = fun() ->
% 		foreach(fun mnesia:write/1, example_tables())
% 	end,
% 	mnesia:transaction(F).


% all(kkeys) ->
% 	do(qlc:q([X || X <- mnesia:table(kkeys)])).

all(Tab) ->
	do(qlc:q([X || X <- mnesia:table(Tab)])).

% deltabs() ->
% 	mnesia:delete_table(keys),
% 	mnesia:delete_table(positions),
% 	mnesia:delete_table(cache).

ets_delete(Tab) ->
	case ets:info(Tab) of
		undefined -> ok;
		_ -> ets:delete(Tab)
	end.