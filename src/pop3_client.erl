-module(pop3_client).

-behaviour(gen_server).

-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% internal process state
-record(state, {
		% user email
		user_mail = "",
		% user password
		password = "",
		% pop3 server host
		server = "",
		% pop3 port
		port = "",
		% active socket
		socket
	}).

start_link(Id, User, Password, Server, Port) ->
    gen_server:start_link({local, Id}, ?MODULE, [User, Password, Server, Port], []).

init([User, Password, Server, Port]) ->
    % Init new process state
    {ok, #state{user_mail = User, 
                password = Password, 
                server = Server, 
                port = Port}}.

%% @doc Send `STAT` command
%% @end
handle_call(send_stat, _From, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `STAT`
	case Sock of
		error ->
			{reply, error, State};
		_ ->
			% Send `STAT command`
			gen_tcp:send(Sock, "STAT\r\n"),
			% Got server response
			{ok, ServerResponse} = gen_tcp:recv(Sock, 0),
			% Match server response
			case string:tokens(ServerResponse, " \r\n") of
				["+OK", MessageCount, Size] ->
					{reply, {ok, list_to_integer(MessageCount), list_to_integer(Size)}, State#state{socket = Sock}};
				_ ->
					{reply, error, State#state{socket = Sock}}
			end
	end;

%% @doc Get message list
%% @end
handle_call(get_list, _From, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST`
	case Sock of
		error ->
			{reply, error, State};
		_ ->
			% Send `LIST`
			gen_tcp:send(Sock, "LIST\r\n"),
			% Got server response
			{ok, ServerResponse} = gen_tcp:recv(Sock, 0),
			% Match server response
			case string:tokens(ServerResponse, " \r\n") of
				["+OK", MessageCount, _, [_Bracket | Octets], _] ->
					{reply, {ok, list_to_integer(MessageCount), 
					             list_to_integer(Octets)}, 
					             State#state{socket = Sock}};
				_ ->
					{reply, error, State#state{socket = Sock}}
			end
	end;

%% @doc Send `LIST` with message id
%% @end
handle_call({get_list, MessageId}, _From, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST` with message id
	case Sock of
		error ->
			{reply, error, State};
		_ ->
			% Send `LIST`
			gen_tcp:send(Sock, "LIST " ++ integer_to_list(MessageId) ++ "\r\n"),
			% Got server response
			{ok, ServerResponse} = gen_tcp:recv(Sock, 0),
			% Match server response
			case string:tokens(ServerResponse, " \r\n") of
				["+OK", MessageId, Octets] ->
					{reply, {ok, list_to_integer(MessageId),
								 list_to_integer(Octets)},
								 State#state{socket = Sock}};
				_ ->
					{reply, error, State#state{socket = Sock}}
			end
	end;

%% @doc Get message with Message Id
%% @end
handle_call({get_message, MessageId}, _From, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST` with message id
	case Sock of
		error ->
			{reply, error, State};
		_ ->
			% Send `RETR ...`
			ok = gen_tcp:send(Sock, "RETR " ++ integer_to_list(MessageId) ++ "\r\n"),
			% Get all message
			{ok, Message} = do_recv(Sock, []),
			% Update socket options
			inet:setopts(Sock, [{active,false}, {package, 0}]),
			% Return message with headers.
			{reply, {ok, Message}, State#state{socket = Sock}}
	end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Send `DELE MessageId` to server
%% @end
handle_cast({delete, MessageId}, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST` with message id
	case Sock of
		error ->
			{noreply, State};
		_ ->
			% Send `DELE`
			gen_tcp:send(Sock, "DELE " ++ integer_to_list(MessageId) ++ "\r\n"),
			% return
			{noreply, State#state{socket = Sock}}
	end;

%% @doc Send `RSET` to server
%% @end
handle_cast(rset, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST` with message id
	case Sock of
		error ->
			{noreply, State};
		_ ->
			% send `RSET`
			gen_tcp:send(Sock, "RSET\r\n"),
			% return
			{noreply, State#state{socket = Sock}}
	end;

%% @doc Send `NOOP` to server
%% @end
handle_cast(noop, State) ->
	% try to get socket
	Sock = get_socket(State),
	% Send `LIST` with message id
	case Sock of
		error ->
			{noreply, State};
		_ ->
			% send `NOOP`
			gen_tcp:send(Sock, "NOOP\r\n"),
			% return
			{noreply, State#state{socket = Sock}}
	end;

%% @doc Send `QUIT` to server
%% @end
handle_cast(quit, State) ->
	Socket = get_socket(State),
	% Send `QUIT\r\n`
	gen_tcp:send(Socket, "QUIT\r\n"),
	% stop process
	{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions

%% @doc Connect to pop3 server and simple USER/PASS authorization
%% @end
-spec auth(User :: string(), Pass :: string(), Server :: string(), Port :: integer()) 
           -> error | {success, Socket :: term()}.
auth(User, Pass, Server, Port) ->
	% Connect to pop3 server
	case gen_tcp:connect(Server, Port, [{active, false}, {packet, 0}]) of
		{ok, Socket} ->
			% check sucessful handshake
			{ok, Packet} = gen_tcp:recv(Socket, 0),
			% Parse pop3 server answer
			case Packet of
				"+OK\r\n" ->
					% try to send USER info
					gen_tcp:send(Socket, "USER " ++ User ++ "\r\n"),
					% got answer
					{ok, Answer} = gen_tcp:recv(Socket, 0),
					% Check answer
					case Answer of
						"+OK\r\n" ->
							% Send password
							gen_tcp:send(Socket, "PASS " ++ Pass ++ "\r\n"),
							% Get answer
							{ok, CheckAuth} = gen_tcp:recv(Socket, 0),
							case string:tokens(CheckAuth, " ") of
								["+OK" | _] ->
									{success, Socket};
								_ ->
									error
							end;
						_ ->
							error
					end;
				_ ->
					error
			end;
		{error, _Reason} ->
			error
	end.

%% @doc Check connected socket or not and return this socket or new
%% @end	
get_socket(State) ->
	% Check we are connected or not
	case State#state.socket of
		undefined ->
			% Try to auth
			TryAuth = auth(State#state.user_mail, State#state.password, State#state.server, State#state.port),
			case TryAuth of
				{success, Socket} ->
					Socket;
				_ ->
					error
			end;
		Socket ->
			Socket
	end.

%% @doc Buffer
%% @end
do_recv(Sock, Bs) ->
	inet:setopts(Sock,[{active,once}, {packet, line}]),
    receive
    	{tcp, _, Data} ->
            do_recv(Sock, [Bs, Data]);
        {tcp_closed, _} ->
        	{ok, lists:flatten(Bs)}
        after 500 ->
        	{ok, lists:flatten(Bs)}
    end.
