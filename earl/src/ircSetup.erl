-module(ircSetup).

-behaviour(gen_event).
-include("ircParser.hrl").
-include("earl.hrl").
-export([init/1, handle_event/2, terminate/2]).
-export([code_change/3, handle_info/2]).


init(Args) ->
	io:format("IRC Setup init ~n~p~n",[Args]),
	{ok, [{?NICKS, disconnected}, Args]}.


handle_event(_, [{Nicks, disconnected}, Args]) ->
	io:format("IRC Setup user ~n"),
	#server{connectionPid=SendPid} = Args,
	SendPid ! #user{user=?USER},
	{ok, [{Nicks, userSet}, Args]};

handle_event(_, [{[Nick|Nicks], userSet}, Args]) ->
	#server{connectionPid=SendPid} = Args,
	SendPid ! #nick{nick=Nick},
	{ok, [{Nicks, nickSent}, Args]};

handle_event(#raw{number_code="001"}, [_, Args]) ->
	#server{connectionPid=SendPid} = Args,
	JoinChan = fun(Chan) -> SendPid ! #join{channel=Chan} end,
	lists:foreach(JoinChan, ?AUTOJN),
	remove_handler;

handle_event(#raw{number_code="433"},  [{[Nick|Nicks], nickSent}, Args]) ->
	#server{connectionPid=SendPid} = Args,
	SendPid ! #nick{nick=Nick},
	{ok, [{Nicks, nickSent}, Args]};

handle_event(_, State) -> {ok, State}.


terminate(_Args, _State) ->
    ok.


handle_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
