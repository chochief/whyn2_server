-module(whyn_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% start_clear просто tcp ~
% start(_Type, _Args) ->
%     Dispatch = cowboy_router:compile([
%         {'_', [
%             {"/wss", wss_handler, []}
%         ]}
%     ]),
%     {ok, _} = cowboy:start_clear(http, 100, [{port, 8443}], %% параметр 100 ~
%         #{env => #{dispatch => Dispatch}}
%     ),    
%     whyn_sup:start_link().

% start(_Type, _Args) ->
%     Dispatch = cowboy_router:compile([
%         {'_', [
%             % {"/css/[...]", cowboy_static, {priv_dir, whyn, "static/css"}},
%             % {"/js/[...]", cowboy_static, {priv_dir, whyn, "static/js"}},
%             % {"/img/[...]", cowboy_static, {priv_dir, whyn, "static/img"}},
%             {"/wss", socket_handler, {}}
%             % {"/", cowboy_static, {priv_file, whyn, "static/index.html"}}
%         ]}
%     ]),
%     % PrivDir = code:priv_dir(whyn),
%     {ok, _} = cowboy:start_tls(https, 100,
%         [
%             {port, 8443},
            
%             {certfile, "/etc/letsencrypt/live/whatsyourna.me/fullchain.pem"},
%             {keyfile, "/etc/letsencrypt/live/whatsyourna.me/privkey.pem"}

%             % {certfile, PrivDir ++ "/ssl/certs/erlsrv.crt.pem"},
%             % {keyfile, PrivDir ++ "/ssl/private/erlsrv.key.pem"}
%             % ----------------
%             % {certfile, PrivDir ++ "/ssl/certs/erlsrv.crt"},
%             % {keyfile, PrivDir ++ "/ssl/private/erlsrv.key"}


%             % {certfile, PrivDir ++ "/ssl/certs/server.crt"},
%             % {keyfile, PrivDir ++ "/ssl/private/server.key"}
%         ],
%         #{env => #{dispatch => Dispatch}}
%     ),    
% 	whyn_sup:start_link().

start(_Type, _Args) ->
    % whyn_mnesia:do_this_once(),
    % whyn_mnesia:start(),
    Dispatch = cowboy_router:compile([
        {'_', [{"/wss", socket_handler, []}]}
    ]),
    PrivDir = code:priv_dir(whyn),
    {ok, _} = cowboy:start_tls(my_http_listener,
        [
            {port, 8443},
            {certfile, PrivDir ++ "/ssl/certs/fullchain.pem"},
            {keyfile, PrivDir ++ "/ssl/private/privkey.pem"}
            %% {certfile, "/etc/letsencrypt/live/whatsyourna.me/fullchain.pem"},
            %% {certfile, "/etc/letsencrypt/live/whatsyourna.me/cert.pem"},
            %% {keyfile, "/etc/letsencrypt/live/whatsyourna.me/privkey.pem"}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    whyn_sup:start_link().


stop(_State) ->
	ok.
