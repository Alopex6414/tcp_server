%% @author alopex
%% @doc @todo Add description to tcp_server_app.


-module(tcp_server_app).

-author('alopex6414@outlook.com').
-behaviour(application).

-define(MAX_RESTART, 10).
-define(MAX_TIME, 60).
-define(DEF_PORT, 6000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_client/0]).
-export([start/2, stop/1, init/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% start client
start_client()	->
	supervisor:start_child(tcp_client_sup, []).

%% start
start(_Type, _Args)	->
	ListenPort = get_app_env(listen_port, ?DEF_PORT),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, tcp_echo_fsm]).

%% stop
stop(_S)	->
	ok.

%% init
init([Port, Module])	->
	{ok,
		{_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
			 	% TCP Listener
				{   tcp_server_sup,                          % Id       = internal id
					{tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
					permanent,                               % Restart  = permanent | transient | temporary
					2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
					worker,                                  % Type     = worker | supervisor
					[tcp_listener]                           % Modules  = [Module] | dynamic
				},
				% Client instance supervisor
                {   
                    tcp_client_sup,
                    {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module]]}, 
                    permanent,                               % Restart  = permanent | transient | temporary
                    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                    supervisor,                              % Type     = worker | supervisor
                    []                                       % Modules  = [Module] | dynamic
                }
              ]
		}
	 };

init([Module]) ->
      {ok,
          {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
              [
                % TCP Client
                {   undefined,                               % Id       = internal id
                    {Module,start_link,[]},                  % StartFun = {M, F, A}
                    temporary,                               % Restart  = permanent | transient | temporary
                    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    worker,                                  % Type     = worker | supervisor
                    []                                       % Modules  = [Module] | dynamic
                }
              ]
          }
      }.

%% get_app_env
get_app_env(Opt, Default) ->
	case application:get_env(application:get_application(), Opt) of
    	{ok, Val} -> Val;
      		_ ->
  		case init:get_argument(Opt) of
          	[[Val | _]] -> Val;
         	error       -> Default
     	end
	end.