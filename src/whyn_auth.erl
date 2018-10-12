-module(whyn_auth).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
% -export([create_account/2]).
% -export([authorize/2]).
% -export([keygen/0]).
-export([keycheck/1]).

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
-record(state, {auth_timer}).


start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

keycheck(Key) ->
	gen_server:call(?SERVER, {keycheck, Key}).

% create_account(Name, PIN) ->
% 	gen_server:cast(?SERVER, {create_account, Name, PIN}).
% authorize(Name, PIN) ->
% 	gen_server:call(?SERVER, {authorize, Name, PIN}).
% keygen() ->
% 	gen_server:call(?SERVER, {keygen}).


init([]) ->
	AuthTimer = erlang:send_after(1, self(), auth_timer_info),
	{ok, #state{ auth_timer = AuthTimer }}.

% handle_call({authorize, Name, PIN}, _From, State) ->
% 	case dict:find(Name, State) of
% 		{ok, {PIN, _Value}} ->
% 			{reply, ok, State};
% 		{ok, {_OtherPIN, _Value}} ->
% 			{reply, {error, invalid_pin}, State};
% 		error ->
% 			{reply, {error, account_does_not_exist}, State}
% 	end;
% handle_call({keygen}, _From, State) ->
% 	Val = crypto:strong_rand_bytes(62),
% 	B64 = base64:encode(Val),
% 	{reply, B64, State};

handle_call({keycheck, Key}, _From, State) ->
	case keycheck_is_valid(Key) of
		{atomic, {okey, BaseRow}} ->
			{reply, BaseRow, State};
		{atomic, _} ->
			NewKey = base64:encode(crypto:strong_rand_bytes(62)),
			{atomic, {okey, BaseRow}} = keycheck_add(NewKey),
			{reply, BaseRow, State}
	end;

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.	



% handle_cast({create_account, Name, PIN}, State) ->
% 	{noreply, dict:store(Name, {PIN, 0}, State)};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(auth_timer_info, State) ->
	erlang:cancel_timer(State#state.auth_timer),
	clear_keys(),
	AuthTimer = erlang:send_after(1000*60*60, self(), auth_timer_info),
	{noreply, State#state{ auth_timer = AuthTimer }};

handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% private methods

keycheck_is_valid(Key) ->
	F = fun() ->
		Rows = mnesia:read({keys, Key}),
		if
			length(Rows) =:= 1 -> 
				[Row|_T] = Rows,
				TouchRow = Row#keys{last_date = calendar:local_time()},
				mnesia:write(TouchRow),
				{okey, TouchRow};
			true -> false
		end
	end,
	mnesia:transaction(F).

keycheck_add(Key) ->
	F = fun() ->
		NewRow = #keys{key=Key, id=erlang:unique_integer([positive,monotonic]), last_date=calendar:local_time()},
		mnesia:write(NewRow),
		Rows = mnesia:read({keys, Key}),
		if
			length(Rows) =:= 1 -> 
				[Row|_T] = Rows,
				{okey, Row};
			true -> false
		end
	end,
	mnesia:transaction(F).

clear_keys() ->
	Now = calendar:local_time(),
	{D, {H,_,_}} = Now,
	UnderTime = {D, {H,0,0}},
	KeysForDel = whyn_mnesia:do(qlc:q([X || X <- mnesia:table(keys), X#keys.last_date < UnderTime])),
	F = fun() ->
		lists:map(fun(Row) -> 
			mnesia:delete({keys, Row#keys.key})
		end, KeysForDel)
	end,
	mnesia:transaction(F).
