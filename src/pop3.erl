-module (pop3).

-export([start/0]).

% pop3 api
-export([get_stat/1,quit/1, get_list/1, delete/1, get_message/1]).
-export([noop/1, rset/1]).

-spec start() -> ok.
start() ->
    application:start(pop3).

%% @doc Send `STAT` command to pop3 server
%% @end
-spec get_stat({User :: string(), Password :: string(), 
	            Server :: string(), Port :: integer()}) -> error | {ok, MessageCount :: integer(), Size :: integer()}.
get_stat({User, Password, Server, Port}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:call(Pid, send_stat).

%% @doc Send `quit`
%% @end
-spec quit({User :: string(), Password :: string(), 
	        Server :: string(), Port :: integer()}) -> ok.
quit({User, Password, Server, Port}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:cast(Pid, quit),
	ok.

%% @doc Get message list
%% @end
-spec get_list({User :: string(), Password :: string(), 
	            Server :: string(), Port :: integer()}) -> error | {ok, MessageCount :: integer(), Octets :: integer()}.
get_list({User, Password, Server, Port}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:call(Pid, get_list);

get_list({User, Password, Server, Port, MessageId}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:call(Pid, {get_list, MessageId}).

%% @doc Delete message
%% @end
-spec delete({User :: string(), Password :: string(), 
	          Server :: string(), Port :: integer(), MessageId :: integer()}) -> ok.
delete({User, Password, Server, Port, MessageId}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:cast(Pid, {delete, MessageId}),
	ok.

%% @doc Send `RSET`
%% @end
-spec rset({User :: string(), Password :: string(), 
	        Server :: string(), Port :: integer()}) -> ok.
rset({User, Password, Server, Port}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:cast(Pid, rset),
	ok.

%% @doc Send `NOOP`
%% @end
-spec noop({User :: string(), Password :: string(), 
	        Server :: string(), Port :: integer()}) -> ok.
noop({User, Password, Server, Port}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:cast(Pid, noop),
	ok.

%% @doc Get message with headers
%% @end
-spec get_message({User :: string(), Password :: string(), 
	               Server :: string(), Port :: integer(), MessageId :: integer()}) -> error | {ok, MessageWithHeaders :: string()}.
get_message({User, Password, Server, Port, MessageId}) ->
	% start new pop3 client
	{ok, Pid} = pop3_sup:start_pop3_client(User, Password, Server, Port),
	gen_server:call(Pid, {get_message, MessageId}, infinity).