-module(wss_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, _State) ->
	Origin = cowboy_req:header(<<"origin">>, Req),
	{cowboy_websocket, Req, Origin, #{idle_timeout => infinity}}.
	% case cowboy_req:header(<<"origin">>, Req) of
	% 	<<"https://whyn.map">> ->
	% 		{cowboy_websocket, Req, State, #{idle_timeout => infinity}};
	% 	_ ->
	% 		{stop, Req, State}
	% end.

websocket_init(State) ->
	case State of
		<<"https://whyn.map">> ->
			{ok, State};
		_ ->
			{stop, "broken origin"}
			% {stop, State}
	end.
	% Key = whyn_auth:keygen(),
	% {ok, State}.

websocket_handle({text, InFrame}, State) ->
	InJson = jsx:decode(InFrame),
	Route = proplists:get_value(<<"route">>, InJson),
	case Route of
		<<"auth">> ->
			InKey = proplists:get_value(<<"key">>, InJson),
			io:format("Route auth | InboxKey ~p ~n", [InKey]),
			OutKey = whyn_auth:keycheck(InKey),
			io:format("Route auth | OutputKey ~p ~n", [OutKey]),
			Answer = jsx:encode([{<<"route_response">>,<<"auth">>},{<<"key">>,OutKey}]),
			{reply, {text, Answer}, State};
			% InKey = proplists:get_value(<<"key">>, InJson),
			% if
			% 	erlang:is_binary(InKey) -> 
			% 		% length(binary_to_list(InKey)) > 8 ->
			% 		io:format("key - yes (~p)~n", [InKey]),
			% 		{K, V} = whyn_auth:keycheck(InKey),
			% 		io:format("Auth. Key - ~p From - ~p~n", [K,V]),
			% 		AuthAnswer1 = jsx:encode([{<<"route_response">>,<<"auth">>},{<<"key">>,InKey}]),
			% 		{reply, {text, AuthAnswer1}, State};
			% 	true ->
			% 		Key = whyn_auth:keygen(),
			% 		io:format("New key - (~p)~n", [Key]),
			% 		AuthAnswer2 = jsx:encode([{<<"route_response">>,<<"auth">>},{<<"key">>,Key}]),
			% 		{reply, {text, AuthAnswer2}, State}
			% end;
		<<"gps">> ->
			io:format("Gps route~n"),
			{ok, State};
		_ ->
			io:format("No route~n"),
			{ok, State}
	end;
	% % io:format("From Websocket: {~p, ~p}~n", [FrameType, FrameContent]);
	% io:format("From Websocket: {~p, ~p}~n", [FrameType, FrameContent]),
	% Fc = jsx:decode(FrameContent),
	% io:format("jsx_decode: ~p ~n", [Fc]),
	% Path = proplists:get_value(<<"key">>, Fc),
	% io:format("Path: ~p ~n", [Path]),
	% {reply, {text, FrameContent}, State};
	% % {reply, {text, ["Erlang received: ", FrameContent, " of type ", atom_to_list(FrameType)]}, State};
websocket_handle(_InFrame, State) ->
	{ok, State}.
% % msg_handler({key, V}) ->
% % 	key;
% % msg_handler(_) ->
% % 	okk.

% % websocket_handle({text, {key, Key}}, State) ->
% % 	io:format("Init key: {~p}~n", [Key]),
% % 	{reply, {key_answer, ["U old key: ", Key]}, State};
% websocket_handle({text, Data}, State) ->
% 	% case erl_parse:parse_term(binary_to_term(Data)) of
% 	case binary_to_term(Data) of
% 		{ok, {key, Key}} ->
% 			io:format("Key: {~p}~n", [Key]),
% 			{reply, {text, ["Key: ", Key]}, State};
% 		{ok, Msg} ->
% 			io:format("Msg: {~p}~n", [Msg]),
% 			{reply, {text, ["Msg: ", Msg]}, State};
% 		{error, _ErrorInfo}	->
% 			io:format("Error parse Msg: {~p}~n", [Data]),
% 			{reply, {text, ["Error parse Msg: ", Data]}, State}
% 	end;

% 	% case {K, V} = Data of
% 	% 	{key, Key} ->
% 	% 		io:format("Init key: {~p}~n", [Key]),
% 	% 		{reply, {key_answer, ["U old key: ", Key]}, State};
% 	% 	true ->
% 	% 		io:format("From wss: {~p}~n", [State]),
% 	% 		{reply, {text, ["U ask: ", Data, ". Zdorovki! Key: ", State]}, State}
% 	% end;

% 	% case binary:bin_to_list(Data) of
% 	% 	{key, Key} ->
% 	% 		io:format("Init key: {~p}~n", [Key]),
% 	% 		{reply, {key_answer, ["U old key: ", Key]}, State};
% 	% 	true ->
% 	% 		io:format("From wss: {~p}~n", [State]),
% 	% 		{reply, {text, ["U ask: ", Data, ". Zdorovki! Key: ", State]}, State}
% 	% end;
% websocket_handle({binary, Data}, State) ->
% 	{reply, {binary, Data}, State};
% websocket_handle(_Frame, State) ->
% 	{ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, undefined, State) ->
	% self() ! {text, ["Closed: ", State]},
	io:format("Closed: {~p}~n", [State]),
	ok.
