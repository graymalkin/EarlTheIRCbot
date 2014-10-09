-module(earlConnection).
-export([connect/3]).

% Opens a connectoin to the server
connect(Parent, {ok, Socket}) ->
	SendPid = spawn(earl, send, [Socket]),
	Parent ! SendPid,
	receive_data(undefined, Socket);
connect(_, {error, Reason}) ->
	io:format("ERROR - Could not connect: ~s~n", [Reason]).
connect(Hostname, Port, Parent) ->
	io:format("INFO - Connecting to ~s:~p ~n", [Hostname, Port]),
	connect(Parent, gen_tcp:connect(Hostname, Port, [], 1000)).


% Receives data from the server and passes it to buffer
receive_data(undefined, Socket) ->
	receive
		{bufferPid, BufferPid} ->
			io:format("Connection: got buffer pid~n"),
			receive_data(BufferPid, Socket)
	end;
receive_data(BufferPid, Socket) ->
	receive
		die ->
			io:format("connectPid :: EXIT~n"),
			exit(self(), normal);
		{tcp, Socket, Bin} ->
			BufferPid ! Bin;
		{tcp_closed, Socket} ->
			io:format("Connection closed.~n",[]),
			mainPid ! die
	end,
    receive_data(BufferPid, Socket).
