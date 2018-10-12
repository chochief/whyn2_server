-module(whyn_live).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_content/1]).
-export([get_live/3]).
-export([upd_live/2]).
-export([get_msg/1]).
-export([reload_content/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

 -include_lib("kernel/include/file.hrl").
-define(SERVER, ?MODULE).
-include("records.hrl").
-record(state, {fed_live}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_msg(BotNames) ->
	gen_server:call(?SERVER, {get_msg, BotNames}).

get_content(ContentId) ->
	gen_server:call(?SERVER, {get_content, ContentId}).

get_live(LiveNm, LiveId, Zones) ->
	gen_server:call(?SERVER, {get_live, LiveNm, LiveId, Zones}).

upd_live(LiveNm, LiveId) ->
	gen_server:call(?SERVER, {upd_live, LiveNm, LiveId}).

reload_content() ->
	gen_server:call(?SERVER, reload_content).


%% gen_server.

init([]) ->
	FunState = reload_content_start(),
	{ok, #state{ fed_live = FunState#state.fed_live }}.


handle_call({get_msg, BotNames}, _From, State) ->
	Msg =  case is_list(BotNames) of
		true ->
			lists:foldl(fun(BotName, Acc) ->
				case ets:lookup(msg, BotName) of
					[M|_] ->
						[M#live.content|Acc];
					_ -> Acc
				end
			end, [], BotNames);
		false -> []
	end,
	case Msg of
		[] -> {reply, not_found, State};
		_ -> {reply, Msg, State}
	end;

handle_call({get_content, ContentId}, _From, State) ->
	case ets:lookup(lives, content) of
		[Content|_] ->
			if
				Content#live.modified =/= ContentId ->
					{reply, Content, State};
				true -> {reply, #live{ modified = last, content = nothing }, State}
			end;
		_ -> {reply, #live{ modified = none, content = none }, State} % its error ! TODO
	end;

handle_call({upd_live, LiveNm, LiveId}, _From, State) ->
	% {Fed,Reg} = Zones,
	% RequiredName = list_to_binary(string:join([Fed,"|",Reg],"")),
	case ets:lookup(lives, LiveNm) of
		[Live|_] ->
			if
				Live#live.modified =:= LiveId ->
					{reply, #live{ name = LiveNm, modified = last, content = nothing }, State};
				true ->
					{reply, Live, State}
			end;
		_ ->
			{reply, State#state.fed_live, State}
	end;

handle_call({get_live, LiveNm, LiveId, Zones}, _From, State) ->
	{Fed,Reg} = Zones,
	RequiredName = list_to_binary(string:join([Fed,"|",Reg],"")),
	case ets:lookup(lives, RequiredName) of
		[Live|_] ->
			if
				RequiredName =:= LiveNm andalso Live#live.modified =:= LiveId ->
					{reply, #live{ name = RequiredName, modified = last, content = nothing }, State};
				true ->
					{reply, Live, State}
			end;
		_ ->
			{reply, State#state.fed_live, State}
	end;

handle_call(reload_content, _From, State) ->
	FunState = reload_content_start(),
	io:format("reload_content complited ~n"),
	{reply, ok, State#state{ fed_live = FunState#state.fed_live }};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reload_content_start () ->
	whyn_mnesia:ets_delete(lives),
	ets:new(lives, [set, private, named_table, {keypos, #live.name}]),
	Dirs = "/var/www/whyndata/",
	%msg (bots)
	whyn_mnesia:ets_delete(msg),
	ets:new(msg, [set, private, named_table, {keypos, #live.name}]),
	MsgFolder = string:join([Dirs,"msg"],""),
	% io:format("~p~n", [MsgFolder]),
	{ok, MsgFiles} = file:list_dir(MsgFolder),
	lists:map(fun(MFile) ->
		case file:read_file(string:join([MsgFolder,"/",MFile],"")) of
			{ok, MF} ->
				MObj = list_to_binary(filename:rootname(filename:basename(MFile))),
				Msg = {MObj, trim(MF)},
				ets:insert(msg, #live{
					name = MObj,
					modified = none,
					content = Msg
				});
			_ -> io:format("Problems with MSG file ~p~n", [MFile])
		end
	end, MsgFiles),
	% content
	ContFolder = string:join([Dirs,"content"],""),
	{ok, ContFiles} = file:list_dir(ContFolder),
	Content = lists:foldl(fun(CFile, Acc) ->
		case file:read_file(string:join([ContFolder,"/",CFile],"")) of
			{ok, CF} ->
				CObj = filename:rootname(filename:basename(CFile)),
				[ {list_to_binary(CObj), trim(CF)} | Acc];
			_ -> io:format("Problems with CONTENT file ~p~n", [CFile])
		end
	end, [], ContFiles),
	% CMod = filelib:last_modified(ContFolder),
	CMod = lists:foldl(fun(CFile, Acc) ->
		case file:read_file_info(string:join([ContFolder,"/",CFile],"")) of
			{ok, CFI} ->
				if
					CFI#file_info.mtime > Acc -> CFI#file_info.mtime;
					true -> Acc
				end;
			_ -> ok
		end
	end, {{0,0,0},{0,0,0}}, ContFiles),
	ets:insert(lives, #live{
		name = content,
		modified = list_to_binary(date_to_id(CMod)),
		content = Content
	}),
	% live
	LiveFolder = string:join([Dirs,"live"],""),
	[SockSrvName|_] = ets:lookup(server_info, sock_server_name),
	LvFedFileName = SockSrvName#server_inf.value,
	LvFedPath = string:join([LiveFolder,"/",LvFedFileName,".json"],""),
	FedContent = case file:read_file(LvFedPath) of
		{ok, FedF} ->
			{<<"lv_fed">>, trim(FedF)};
		_ ->
			io:format("Problems with FED! file ~p~n", [LvFedFileName]),
			{<<"lv_fed">>, <<"null">>}
	end,
	FedId = case file:read_file_info(LvFedPath) of
		{ok, FedInfo} ->
			list_to_binary(date_to_id(FedInfo#file_info.mtime));
		_ ->
			io:format("Problems with FED! file ~p~n", [LvFedFileName]),
			<<"null">>
	end,
	FedLive = #live{
		name = list_to_binary(string:join([LvFedFileName,"|"],"")),
		modified =  FedId,
		content = [FedContent, {<<"lv_reg">>, <<"null">>}]
	},
	ets:insert(lives, FedLive),
	{ok, LiveFiles} = file:list_dir(LiveFolder),
	lists:map(fun(LFile) ->
		LObj = filename:rootname(filename:basename(LFile)),
		if
			LObj =:= LvFedFileName -> ok;
			true ->
				case file:read_file(string:join([LiveFolder,"/",LFile],"")) of
					{ok, LF} ->
						NewLive = {<<"lv_reg">>, trim(LF)},
						case file:read_file_info(string:join([LiveFolder,"/",LFile],"")) of
							{ok, LFileInfo} ->
								LMod = LFileInfo#file_info.mtime,
								ets:insert(lives, #live{
									name = list_to_binary(string:join([LvFedFileName,"|",LObj],"")),
									modified =  list_to_binary(string:join([binary_to_list(FedId),"|",date_to_id(LMod)],"")),
									content = [FedContent, NewLive]
								});
							_ -> io:format("Problems with file ~p~n", [LFile])
						end;
					_ -> io:format("Problems with file ~p~n", [LFile])
				end
		end
	end, LiveFiles),
	#state{fed_live = FedLive}.

date_to_id(ErlDateTime) ->
	case ErlDateTime of
		{{Y,M,D},{H,Min,S}} ->
			string:join([integer_to_list(Y),".",integer_to_list(M),".",integer_to_list(D),"_",integer_to_list(H),":",integer_to_list(Min),":",integer_to_list(S)],"");
		_ -> ErlDateTime
	end.

trim(Bin) -> list_to_binary(trim_blanks(binary_to_list(Bin))).
trim_blanks(X) -> lists:reverse(skip(X)).
skip(X) ->
	lists:foldl(fun(G,Acc) ->
		case G of
			$\t -> Acc;
			$\n -> Acc;
			% [9|_T] -> Acc;
			% [10|_T] -> Acc;
			_ ->
				[G|Acc]
		end
	end, [], X).
