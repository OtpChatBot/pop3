
-module(pop3_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_pop3_client/4]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start new pop3 client
%% @end
-spec start_pop3_client(User :: string(), Password :: string(), Server :: string(), Port :: integer()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_pop3_client(User, Password, Server, Port) ->
    Id = list_to_atom(User ++ "@" ++ Password),
    % pop3 client spec
    ChildSpec = {Id, 
   					{pop3_client, start_link, [Id, User, Password, Server, Port]},
    	 			 temporary, 2000, worker, []
    			},
    % Check pop3 client
    case whereis(Id) of
    	undefined ->
    		% run new pop3 client
    		supervisor:start_child(?MODULE, ChildSpec);
    	% Pid alread exist and maybe connected
        Pid ->
            % return pid
    		{ok, Pid}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Return supervisor restart strategy
    {ok, { {one_for_one, 5, 10}, []} }.

