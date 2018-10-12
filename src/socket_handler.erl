-module(socket_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("records.hrl").
-record(state, {origin, id = none, sex = <<"n">>, age = <<"n">>, filter = <<"n">>, pic = <<"n">>, msg = <<"n">>}).

init(Req, _State) ->
	Origin = cowboy_req:header(<<"origin">>, Req),
	NewState = #state{origin=Origin},
	{cowboy_websocket, Req, NewState, #{idle_timeout => infinity}}.

websocket_init(State) ->
	[Origin] = ets:lookup(server_info, web_origin),
	OriginBin = list_to_binary(Origin#server_inf.value),
	if 
		State#state.origin =:= OriginBin ->
		% <<"https://whyn.map">> -> %% TODO server_info whyn_sup, args, makefile
			{ok, State};
		true ->
			io:format("broken origin ~p~n", [State#state.origin]),
			{ok, State}
			% {stop, "broken origin"}
	end.

websocket_handle({text, FrameJson}, State) ->
	case jsx:is_json(FrameJson) of
		true ->
			JsonMap = jsx:decode(FrameJson, [return_maps]),
			Route = maps:get(<<"route">>, JsonMap, null),
			case Route of
				<<"hello">> ->
					Sex = maps:get(<<"sex">>, JsonMap, <<"n">>),
					Age = maps:get(<<"age">>, JsonMap, <<"n">>),
					Filter = maps:get(<<"filter">>, JsonMap, <<"n">>),
					NewState = State#state{	sex = Sex, age = Age, filter = Filter },
					ContentId = maps:get(<<"content_id">>, JsonMap, null),
					Content = whyn_live:get_content(ContentId),
					Ans = jsx:encode([ 
						{<<"route_response">>,<<"hello_ok">>},
						{<<"content_id">>, Content#live.modified},
						{<<"content">>, Content#live.content}
					]),
					{reply, {text, Ans}, NewState};
				<<"re_sett">> ->
					Sex = maps:get(<<"sex">>, JsonMap, <<"n">>),
					Age = maps:get(<<"age">>, JsonMap, <<"n">>),
					Filter = maps:get(<<"filter">>, JsonMap, <<"n">>),
					NewState = State#state{	sex = Sex, age = Age, filter = Filter },
					Ans = jsx:encode([ 
						{<<"route_response">>,<<"sett_ok">>}
					]),
					{reply, {text, Ans}, NewState};
				<<"sims_init">> ->
					% key
					Key = maps:get(<<"key">>, JsonMap, null),
					KeyRow = whyn_auth:keycheck(Key),
					% live
					LiveNm = maps:get(<<"live_nm">>, JsonMap, null),
					LiveId = maps:get(<<"live_id">>, JsonMap, null),
					Live = whyn_live:upd_live(LiveNm, LiveId),
					% state
					NewState = State#state{
						id = KeyRow#keys.id
					},
					% ans
					Ans = jsx:encode([
						{<<"route_response">>, <<"sims_init_ok">>},
						{<<"key">>, KeyRow#keys.key},
						{<<"key_id">>, KeyRow#keys.id},
						{<<"live_nm">>, Live#live.name},
						{<<"live_id">>, Live#live.modified},
						{<<"live">>, Live#live.content}
					]),
					{reply, {text, Ans}, NewState};
				<<"gps_init">> ->
					Lat = maps:get(<<"lat">>, JsonMap, null),
					Lon = maps:get(<<"lon">>, JsonMap, null),
					Square = whyn_gps:calculate_square(Lat, Lon),
					case whyn_gps:check_sock(Square) of
						{ok, Zones, Url} ->
							% key
							Key = maps:get(<<"key">>, JsonMap, null),
							KeyRow = whyn_auth:keycheck(Key),
							% live
							LiveNm = maps:get(<<"live_nm">>, JsonMap, null),
							LiveId = maps:get(<<"live_id">>, JsonMap, null),
							Live = whyn_live:get_live(LiveNm, LiveId, Zones),
							% state
							NewState = State#state{
								id = KeyRow#keys.id
							},
							% ans
							Ans = jsx:encode([
								{<<"route_response">>, <<"gps_init_ok">>},
								{<<"sock_addr">>, list_to_binary(Url)},
								{<<"key">>, KeyRow#keys.key},
								{<<"key_id">>, KeyRow#keys.id},
								{<<"live_nm">>, Live#live.name},
								{<<"live_id">>, Live#live.modified},
								{<<"live">>, Live#live.content}
							]),
							{reply, {text, Ans}, NewState};
						{new_sock, Url} ->
							Ans = jsx:encode([{<<"route_response">>,<<"gps_sock">>}, {<<"sock_addr">>, list_to_binary(Url)}]),
							{reply, {text, Ans}, State};
						out -> 
							Ans = jsx:encode([{<<"route_response">>,<<"gps_out">>}]),
							{reply, {text, Ans}, State}
					end;
				<<"get_msg">> ->
					BotNames = maps:get(<<"bots">>, JsonMap, null),
					Msg = whyn_live:get_msg(BotNames),
					Ans = jsx:encode([
						{<<"route_response">>, <<"msg_ok">>},
						{<<"msg">>, Msg}
						% {<<"msg">>, Msg#live.content}
					]),
					{reply, {text, Ans}, State};
				<<"sims">> ->
					Cache = whyn_gps:read_cache(sims),
					Ans = jsx:encode([
						{<<"route_response">>,<<"sims_ok">>},
						{<<"sims_cache">>, Cache}
					]),
					{reply, {text, Ans}, State};
				<<"gps">> ->
					Lat = maps:get(<<"lat">>, JsonMap, null),
					Lon = maps:get(<<"lon">>, JsonMap, null),
					Square = whyn_gps:calculate_square(Lat, Lon),
					case whyn_gps:check_square(Square) of
						out ->
							Ans = jsx:encode([{<<"route_response">>,<<"gps_out">>}]),
							{reply, {text, Ans}, State};
						in ->
							Position = #positions{
								id = State#state.id,
								lat = Lat,
								lon = Lon,
								square = Square,
								sex = State#state.sex,
								age = State#state.age,
								filter = State#state.filter,
								pic = State#state.pic,
								msg = State#state.msg
							},
							whyn_gps:write_position(Position),
							Cache = whyn_gps:read_cache(Square),
							Ans = jsx:encode([
								{<<"route_response">>,<<"gps_ok">>},
								{<<"gps_sqcache">>, Cache}
							]),
							{reply, {text, Ans}, State}
					end; 
				_ ->
					io:format("No route: ~p ~n", [Route]),
					{ok, State}
			end;
		false ->
			io:format("Not json: ~p ~n", [FrameJson]),
			{ok, State}
	end;
websocket_handle(_Frame, State) ->
	{ok, State}.


websocket_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
