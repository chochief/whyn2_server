-module(whyn_gps).
-behaviour(gen_server).

%% API.
-export([
	start_link/0, 
	write_position/1, 
	read_cache/1, 
	calculate_square/2,
	check_sock/1,
	check_square/1,
	check_zones/2
]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl"). %% for ets:fun2ms/1
-define(SERVER, ?MODULE).
-include("records.hrl").
-record(state, {snap_timer, lat_min, lat_max, lon_min, lon_max}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


write_position(Position) ->
	gen_server:cast(?SERVER, {write_position, Position}).

read_cache(Square) ->
	gen_server:call(?SERVER, {read_cache, Square}).

check_square(Square) ->
	gen_server:call(?SERVER, {check_square, Square}).



%% gen_server.

init([]) ->
	load_server_info(),
	load_socks_zones(),
	ServerZone = set_server_zone(),
	snap_init(),
	SnapTimer = erlang:send_after(1, self(), snapshot_timer),
	{ok, #state{
		snap_timer = SnapTimer,
		lat_min = ServerZone#sock.lat_min,
		lat_max = ServerZone#sock.lat_max,
		lon_min = ServerZone#sock.lon_min,
		lon_max = ServerZone#sock.lon_max
	}}.



handle_call({check_square, {SqLat,SqLon}}, _From, State) ->
	case SqLat >= State#state.lat_min andalso SqLat < State#state.lat_max andalso SqLon >= State#state.lon_min andalso SqLon < State#state.lon_max of
		true -> {reply, in, State};
		false -> {reply, out, State}
	end;

handle_call({read_cache, Square}, _From, State) ->
	case mnesia_read_cache(Square) of
		{atomic, {okey, Cache}} ->
			{reply, Cache, State};
		{atomic, _} ->
			{reply, nothing, State}
			% {reply, [], State}
	end;

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({write_position, Position}, State) ->
	mnesia_write_position(Position),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(snapshot_timer, State) ->
	erlang:cancel_timer(State#state.snap_timer),
	% do_task
	update_snap_write(),
	Positions = get_cache_positions(),
	CacheList = make_cache(Positions),
	cache_writeto_mnesia(CacheList),
	% 
	update_snap_read(),
	clear_positions(Positions),
	clear_cache(),
	% 
	SnapTimer = erlang:send_after(5000, self(), snapshot_timer),
	{noreply, State#state{
		snap_timer = SnapTimer
	}};

handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


% public gps functions

make_cache(Positions) ->
	make_cache_recursion(Positions, []).

make_cache_recursion([],Acc) -> Acc;
make_cache_recursion(Positions,Acc) ->
	[P|_TPositions] = Positions,
	SnSq = P#positions.snapshot_square,
	Sn = P#positions.snapshot,
	{LPositions, RPositions} = lists:partition(fun(Pos) ->
		SnSq =:= Pos#positions.snapshot_square
	end, Positions),
	JsonPrep = lists:foldl(fun(LP,JP) ->
		[LP#positions.json_prep | JP]
	end, [], LPositions),
	% Json = jsx:encode(JsonPrep),
	JsonPrepYa = [{<<"type">>,<<"FeatureCollection">>},{<<"features">>,JsonPrep}],
	Json = jsx:encode(JsonPrepYa),
	Cache = [ #cache{snapshot_square = SnSq, snapshot = Sn, json = Json} | Acc ],
	make_cache_recursion(RPositions, Cache).


% % Yaroslavl Square
% % [57.80, 39.66]
% % [57.50, 40.08]
% % LatMax = 57.80
% % LatMin = 57.50
% % LonMax = 40.08
% % LonMin = 39.66
% check_square({SqLat,SqLon}) ->
% 	SqLatMin = 5750, SqLatMax = 5780,
% 	SqLonMin = 3966, SqLonMax = 4008,
% 	case SqLat >= SqLatMin andalso SqLat < SqLatMax andalso SqLon >= SqLonMin andalso SqLon < SqLonMax of
% 		true -> in;
% 		false -> out
% 	end.

check_sock({SqLat,SqLon}) ->
	SocksMS = ets:fun2ms(fun({Sock, SName, SLatMin, SLatMax, SLonMin, SLonMax, SUrl})
						when SqLat >= SLatMin andalso SqLat < SLatMax andalso SqLon >= SLonMin andalso SqLon < SLonMax ->
							{SName, SUrl}
					end),
	[ServerName] = ets:lookup(server_info, sock_server_name),
	case ets:select(socks, SocksMS) of
		[{Name, Url}|_] ->
			case Name of
				Name when Name =:= ServerName#server_inf.value ->
					case check_zones(SqLat,SqLon) of
						[Zone|_] -> {ok, {Name,Zone}, Url}; % ie lv_federal + lv_regional, io limit zones
						_ -> {ok, {Name, ""}, Url}
					end;
				_ -> {new_sock, Url} % send url for new sock
			end;
		_ ->
			out % no sock for connect
	end.

check_zones(SqLat,SqLon) ->
	ZonesMS = ets:fun2ms(fun({Zones, ZName, ZSockName, ZLatMin, ZLatMax, ZLonMin, ZLonMax})
						when SqLat >= ZLatMin andalso SqLat < ZLatMax andalso SqLon >= ZLonMin andalso SqLon < ZLonMax ->
							ZName
					end),
	ets:select(zones, ZonesMS).

% Square - left bottom point
% Lat = 57.6198 -> 5761
% Lon = 39.8554 -> 3985 - >3984
% Square = [5761,3984]
% Lat = 157.6198 -> 15761
% Lon = 9.8554 -> 985 - >984
% Square = [15761,984]
calculate_square(Lat, Lon) ->
	% 1km = 0.01 => x100 -> floorr
	SqLat = floorr(Lat*100),
	% 1km lon = 0.02 => x100 -> floorr -> rem
	SqLon = floorr(Lon*100),
	case SqLon rem 2 == 0 of
		true -> {SqLat, SqLon};
		false -> {SqLat, SqLon - 1}
	end.
	% case SqLon1 rem 2 == 0 of
	% 	true -> SqLon = SqLon1;
	% 	false -> SqLon = SqLon1 - 1
	% end,
	% Square = {SqLat,SqLon}.

% missing methods floorr & ceil
floorr(X) when X < 0 ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
		false -> T - 1
	end;
floorr(X) -> 
	trunc(X).
 
% ceiling(X) when X < 0 ->
% 	trunc(X);
% ceiling(X) ->
% 	T = trunc(X),
% 	case X - T == 0 of
% 		true -> T;
% 		false -> T + 1
% 	end.

% private methods

snap_init() ->
	F = fun() ->
		{_, SnapInit} = calendar:local_time(), % init snapshot times
		mnesia:write(#snaps{
			key = snap_row, 
			snap_write = SnapInit,
			snap_cache = none,
			snap_read = none
		})
	end,
	mnesia:transaction(F).

update_snap_write() ->
	F = fun() ->
		Rows = mnesia:read({snaps, snap_row}),
		if
			length(Rows) =:= 1 -> 
				[Row|_T] = Rows,
				SnapCache = Row#snaps.snap_write, % remember snap_write
				{_, SnapWrite} = calendar:local_time(), % make new snap_write
				mnesia:write(Row#snaps{
					key = snap_row, 
					snap_write = SnapWrite, % apply new snap_write -> new data written there
					snap_cache = SnapCache
				});
			true -> false % error
		end
	end,
	mnesia:transaction(F).

update_snap_read() ->
	F = fun() ->
		Rows = mnesia:read({snaps, snap_row}),
		if
			length(Rows) =:= 1 -> 
				[Row|_T] = Rows,
				SnapRead = Row#snaps.snap_cache, % remember snap_write
				mnesia:write(Row#snaps{
					key = snap_row, 
					snap_read = SnapRead
				});
			true -> false % error
		end
	end,
	mnesia:transaction(F).

get_cache_positions() ->
	whyn_mnesia:do(qlc:q([X || X <- mnesia:table(positions), Y <- mnesia:table(snaps), Y#snaps.key =:= snap_row, X#positions.snapshot =:= Y#snaps.snap_cache])).

cache_writeto_mnesia(CacheList) ->
	F = fun() ->
		lists:foreach(fun mnesia:write/1, CacheList)
	end,
	mnesia:transaction(F).

clear_positions(Positions) -> 
	F = fun() ->
		lists:map(fun(Pos) -> 
			mnesia:delete({positions, Pos#positions.snapshot_id})
		end, Positions)
	end,
	mnesia:transaction(F).

clear_cache() ->
	CacheDel = whyn_mnesia:do(qlc:q([X || X <- mnesia:table(cache), Y <- mnesia:table(snaps), Y#snaps.key =:= snap_row, X#cache.snapshot =/= Y#snaps.snap_read])),
	F = fun() ->
		lists:map(fun(C) -> 
			mnesia:delete({cache, C#cache.snapshot_square})
		end, CacheDel)
	end,
	mnesia:transaction(F).

mnesia_read_cache(Square) ->
	F = fun() ->
		Snaps = mnesia:read({snaps, snap_row}),
		if
			length(Snaps) =:= 1 -> 
				[SnapsRow|_T] = Snaps,
				SnapRead = SnapsRow#snaps.snap_read,
				CacheKey = {SnapRead, Square},
				Rows = mnesia:read({cache, CacheKey}),
				if
					length(Rows) =:= 1 -> 
						[Row|_T] = Rows,
						{okey, Row#cache.json};
					true -> false
				end;
			true -> false
		end	
	end,
	mnesia:transaction(F).

mnesia_write_position(Position) ->
	JsonPrep = [
		{<<"type">>,<<"Feature">>},
		{<<"id">>,Position#positions.id},
		{<<"geometry">>, [
			{<<"type">>,<<"Point">>},
			{<<"coordinates">>,[Position#positions.lat, Position#positions.lon]}
		]},
		{<<"properties">>, [
			{<<"s">>,Position#positions.sex},
			{<<"a">>,Position#positions.age},
			{<<"f">>,Position#positions.filter},
			{<<"p">>,Position#positions.pic},
			{<<"m">>,Position#positions.msg}
		]}
	],
	F = fun() ->
		Snaps = mnesia:read({snaps, snap_row}),
		if
			length(Snaps) =:= 1 -> 
				[SnapsRow|_T] = Snaps,
				SnapWrite = SnapsRow#snaps.snap_write,
				NewRow = Position#positions{
					snapshot_id = {SnapWrite, Position#positions.id},
					snapshot_square = {SnapWrite, Position#positions.square},
					snapshot = SnapWrite,
					json_prep = JsonPrep
				},
				mnesia:write(NewRow);
			true -> false
		end		
	end,
	mnesia:transaction(F).

load_server_info () ->
	whyn_mnesia:ets_delete(server_info),
	ets:new(server_info, [set, protected, named_table, {keypos, #server_inf.name}]),
	ServerInfoFile = "/var/www/whyndata/server_info",
	case file:consult(ServerInfoFile) of
		{ok, ServerInfoData} ->
			lists:map(fun(Inf) ->
				ets:insert(server_info, #server_inf{
					name = Inf#server_inf.name,
					value = Inf#server_inf.value
				})
			end, ServerInfoData),
			check_server_info(); % TODO check all required fields
		_ ->
			% io:format("~p~n", [A]), 
			erlang:error("WHYN: file server_info reading error!")
	end.

check_server_info () ->
	ets:tab2list(server_info),
	ok.

load_socks_zones () ->
	whyn_mnesia:ets_delete(socks),
	whyn_mnesia:ets_delete(zones),
	ets:new(socks, [set, protected, named_table, {keypos, #sock.name}]),
	ets:new(zones, [set, protected, named_table, {keypos, #zone.name}]),
	case ets:lookup(server_info, sock_server_name) of
		[SockServerName] ->
			SocksZones = "/var/www/whyndata/socks_zones",
			case file:consult(SocksZones) of
				{ok, SocksAndZones} ->
					lists:map(fun(Row) ->
						case Row of
							Row when is_record(Row, sock) ->
								ets:insert(socks, #sock{
									name = Row#sock.name,
									lat_min = Row#sock.lat_min,
									lat_max = Row#sock.lat_max,
									lon_min = Row#sock.lon_min,
									lon_max = Row#sock.lon_max,
									url = Row#sock.url
								});
							Row when is_record(Row, zone) andalso Row#zone.sock_name =:= SockServerName#server_inf.value ->
								ets:insert(zones, #zone{
									name = Row#zone.name,
									sock_name = Row#zone.sock_name,
									lat_min = Row#zone.lat_min,
									lat_max = Row#zone.lat_max,
									lon_min = Row#zone.lon_min,
									lon_max = Row#zone.lon_max
								});
							_ -> ok
						end
					end, SocksAndZones);
				_ -> erlang:error("WHYN: file socks_zones reading error!")
			end;
		_ -> erlang:error("WHYN: prop sock_server_name is missing!")
	end.

set_server_zone () ->
	case ets:lookup(server_info, sock_server_name) of
		[ServerName] ->
			case ets:lookup(socks, ServerName#server_inf.value) of
				[Sock] -> Sock;
				[] -> erlang:error("WHYN: server SOCK not found in socks ets!")
			end;
		[] -> erlang:error("WHYN: prop sock_server_name is missing!")
	end.
