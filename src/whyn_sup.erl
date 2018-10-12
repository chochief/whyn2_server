-module(whyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).
% start_link() ->
% 	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------

init([]) ->
	% whyn_mnesia:do_this_once(),
	Whyn_auth = {whyn_auth, {whyn_auth, start_link, []}, permanent, 4000, worker,[whyn_auth]},
	Whyn_gps = {whyn_gps, {whyn_gps, start_link, []}, permanent, 4000, worker,[whyn_gps]},
	Whyn_live = {whyn_live, {whyn_live, start_link, []}, permanent, 4000, worker,[whyn_live]},
	Whyn_bot = {whyn_bot, {whyn_bot, start_link, []}, permanent, 4000, worker,[whyn_bot]},
	{ok, {{one_for_one, 1, 5}, [Whyn_auth, Whyn_gps, Whyn_live, Whyn_bot]}}.

% init([]) ->
% 	Procs = [],
% 	{ok, {{one_for_one, 1, 5}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
