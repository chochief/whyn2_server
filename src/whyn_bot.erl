-module(whyn_bot).
-behaviour(gen_server).

%% API.
-export([
	start_link/0,
	reload_bots/0,
	stop_bots/0
]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-define(SERVER, ?MODULE).
-include("records.hrl").
-record(state, {bots_timer}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% reread files in the bots folder, reload ets tabs, remake bots
reload_bots() ->
	gen_server:cast(?SERVER, reload_bots).

% to stop bots timer
stop_bots() ->
	gen_server:cast(?SERVER, stop_bots).

%% gen_server.

init([]) ->
	reload_bots(),
	BotsTimer = erlang:send_after(1, self(), bots_timer_info),
	{ok, #state{
		bots_timer = BotsTimer
	}}.

% -------------------------------------------------------------------------------------------------
% call

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


% -------------------------------------------------------------------------------------------------
% cast

handle_cast(reload_bots, State) ->
	erlang:cancel_timer(State#state.bots_timer),
	re_bots(),
	BotsTimer = erlang:send_after(1, self(), bots_timer_info),
	{noreply, #state{
		bots_timer = BotsTimer
	}};

handle_cast(stop_bots, State) ->
	erlang:cancel_timer(State#state.bots_timer),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.


% -------------------------------------------------------------------------------------------------
% info

handle_info(bots_timer_info, State) ->
	erlang:cancel_timer(State#state.bots_timer),
	% do_task
	snap_move(bots),
	snap_move(sims),
	% restart timer 
	BotsTimer = erlang:send_after(5000, self(), bots_timer_info),
	{noreply, State#state{
		bots_timer = BotsTimer
	}};

handle_info(_Info, State) ->
	{noreply, State}.


% -------------------------------------------------------------------------------------------------
% others

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%% ------------------------------------------------------------------------------------------------
%% private methods
%% ------------------------------------------------------------------------------------------------

-record(track, {index, file, length, used}).
-record(track_pos, {id, square, lat, lon}). % id = {file,line}
-record(pos, {lat, lon}).
-record(bot_track, {file, pos, direction, length}).

-record(dbot, {name, sex, age, filter, pic, msg, track}).

re_bots() ->
	load_tracks(),
	load_bots(bots),
	load_bots(sims),
	ok.

snap_move(Type) ->
	Bots = ets:tab2list(Type),
	lists:map(fun(Bot) ->
		BotUpdated = move_bot(Bot#dbot.name, Type),
		Track = BotUpdated#dbot.track,
		TrackposId = {Track#bot_track.file,Track#bot_track.pos},
		case ets:lookup(trackpos, TrackposId) of
			[Trackpos|_] ->
				Square = case Type of
					bots ->
						Trackpos#track_pos.square;
					sims ->
						% Trackpos#track_pos.square; 
						sims;
					_ -> err
				end,
				Lat = Trackpos#track_pos.lat,
				Lon = Trackpos#track_pos.lon,
				% 
				Position = #positions{
					id = list_to_binary(BotUpdated#dbot.name),
					lat = Lat,
					lon = Lon,
					square = Square,
					sex = BotUpdated#dbot.sex,
					age = BotUpdated#dbot.age,
					filter = list_to_binary(BotUpdated#dbot.filter),
					pic = list_to_binary(BotUpdated#dbot.pic),
					msg = list_to_binary(BotUpdated#dbot.msg)
				},
				whyn_gps:write_position(Position);
			_ ->
				io:format("error: track ~p not found ~n", [TrackposId]), 
				{error, "track_not_found"}
		end
	end, Bots),
	ok.

load_bots(BType) ->
	whyn_mnesia:ets_delete(BType),
	ets:new(BType, [set, private, named_table, {keypos, #dbot.name}]),
	Dirs = "/var/www/whyndata/",
	FileName = string:join([Dirs, atom_to_list(BType)],""),
	case file:consult(FileName) of
		{ok, Bots} ->
			lists:map(fun(Bot) ->
				ets:insert(BType, #dbot{
					name = Bot#dbot.name,
					age = Bot#dbot.age,
					sex = Bot#dbot.sex,
					track = bind_track(Bot#dbot.track),
					filter = Bot#dbot.filter,
					pic = Bot#dbot.pic,
					msg = Bot#dbot.msg
				})
			end, Bots);
		_ -> io:format("Problems with BOTS file ~n", [])
	end,
	ok.

load_tracks() ->
	whyn_mnesia:ets_delete(tracks),
	whyn_mnesia:ets_delete(trackpos),
	ets:new(tracks, [set, private, named_table, {keypos, #track.file}]),
	ets:new(trackpos, [set, private, named_table, {keypos, #track_pos.id}]),
	Dirs = "/var/www/whyndata/",
	Dir = string:join([Dirs,"tracks"],""),
	% io:format("~p~n", [Dir]),
	{ok, DirFiles} = file:list_dir(Dir),
	lists:foldl(fun(FileName, TrackIndex) ->
		case file:consult(string:join([Dir,"/",FileName],"")) of
		 	{ok, Positions} ->
		 		TrackLength = lists:foldl(fun(Pos, Line) ->
		 			Length = Line + 1,
		 			ets:insert(trackpos, #track_pos{ 
		 				id = {FileName, Length},
		 				square = whyn_gps:calculate_square(Pos#pos.lat, Pos#pos.lon),
		 				lat = Pos#pos.lat,
		 				lon = Pos#pos.lon
		 			}),
		 			Length
		 		end, 0, Positions),
		 		if
		 			TrackLength > 0 ->
		 				NewTrackIndex = TrackIndex + 1,
		 				ets:insert(tracks, #track{
		 					index = NewTrackIndex,
		 					file = FileName,
		 					length = TrackLength,
		 					used = 0
		 				}),
		 				NewTrackIndex;
		 			true -> TrackIndex
		 		end;
		 	_ -> TrackIndex
		end 
	end, 0, DirFiles),
	ok.

bind_track(File) ->
	case ets:lookup(tracks, File) of
		[Track | _] ->
			% ets:insert(tracks, Track#track{ used = Track#track.used + 1 }),
			#bot_track {
				file = Track#track.file,
				pos = rand:uniform(Track#track.length),
				direction = rand:uniform(2)-1, % 0 or 1
				length = Track#track.length
			};
		[] -> 
			io:format("Problems with BOTS file: track ~p not found ~n", [File]),
			case ets:lookup(tracks, ets:first(tracks)) of
				[Tr|_] ->
					#bot_track {
						file = Tr#track.file,
						pos = rand:uniform(Tr#track.length),
						direction = rand:uniform(2)-1, % 0 or 1
						length = Tr#track.length
					};
				[] -> erlang:error("WHYN: tracks not found!")
			end
	end.

% bind_track_rand() ->
% 	TracksSize = ets:info(tracks, size),
% 	Index = rand:uniform(TracksSize),
% 	bind_track(Index).

move_bot(BotName, Type) ->
	case ets:lookup(Type, BotName) of
		[Bot|_] ->
			NewPos = walk_track(Bot#dbot.track),
			BotUpdated = Bot#dbot{track = NewPos},
			ets:insert(Type, BotUpdated),
			BotUpdated;
		[] ->
			{error, "bot_not_found"}
	end.

walk_track(BotTrack) ->
	% Pause = rand:uniform(2)-1,
	% Visible = rand:uniform(10),
	if 
		(BotTrack#bot_track.length =:= 0) or (BotTrack#bot_track.length =:= 1) ->
			Pos = BotTrack#bot_track.pos,
			Direction = BotTrack#bot_track.direction,
			BotTrack#bot_track{ pos = Pos, direction = Direction };
		BotTrack#bot_track.pos =:= BotTrack#bot_track.length ->
			Pos = BotTrack#bot_track.pos - 1,
			Direction = 0,
			BotTrack#bot_track{ pos = Pos, direction = Direction };
		BotTrack#bot_track.pos =:= 1 ->
			Pos = BotTrack#bot_track.pos + 1,
			Direction = 1,
			BotTrack#bot_track{ pos = Pos, direction = Direction };
		BotTrack#bot_track.direction =:= 0 ->
			Pos = BotTrack#bot_track.pos - 1,
			Direction = BotTrack#bot_track.direction,
			BotTrack#bot_track{ pos = Pos, direction = Direction };
		BotTrack#bot_track.direction =:= 1 ->
			Pos = BotTrack#bot_track.pos + 1,
			Direction = BotTrack#bot_track.direction,
			BotTrack#bot_track{ pos = Pos, direction = Direction };
		true ->
			Pos = BotTrack#bot_track.pos,
			Direction = BotTrack#bot_track.direction,
			BotTrack#bot_track{ pos = Pos, direction = Direction }
	end.

%% ------------------------------------------------------------------------------------------------