%% Copyright (c) 2008-2013, Nanusorn Photpipat <nanusorn@photpipat.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.-module (ability_effect).
-module(smo_ranking_guardian).
-behaviour(gen_server).
-export([
						start_link/0,
						connected/2,
						disconnected/1
					]).
					
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

start_link() ->
	ServerName = {local, smo_ranking_guardian}, % name gets register()ed
	Module = smo_ranking_guardian,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).
	
connected(PlayerPid, Class) -> gen_server:cast(smo_ranking_guardian, {connected, PlayerPid, Class}).
disconnected(PlayerPid) -> gen_server:cast(smo_ranking_guardian, {disconnected, PlayerPid}).

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
	
handle_call(Request, From, State) ->
	PlayerList = State,
	{Pid, _Tag} = From,
	case Request of 
	Other ->
		{noreply,State}
	end.
	
handle_cast(Request, State) ->
	PlayerList = State,
	case Request of 
		{connected, PlayerPid, Class} ->
			NewState = State ++ [PlayerPid],
			InQueue = get_class_queue(Class) -- [PlayerPid],
			NewQueue = InQueue ++ [PlayerPid],
			smo_logger:fmsg("player wait in ~p are ~p", [Class, NewQueue]),
			put(Class, NewQueue),
			Select = get_player_play(Class, PlayerPid),
			check_player_play(Select),
			WaitingPlayer = length(NewQueue),
			if
				WaitingPlayer > 2 ->
					gen_server:cast(PlayerPid, {send, [16#01, 16#09, <<WaitingPlayer:16>>]});
				true ->
					gen_server:cast(PlayerPid, {send, [16#01, 16#09, <<1:16>>]})
			end,
		{noreply, NewState};
		{both_player_play, FirstQPid, RanQPid} ->
			NewState = State -- [FirstQPid, RanQPid],
			both_player_play(FirstQPid, RanQPid),
			{noreply, NewState};
		{disconnected, PlayerPid} ->
			PlayerList2 = PlayerList -- [PlayerPid],
			cancel_rank_play(PlayerPid),
			{noreply, PlayerList2}
	end.

handle_info(_, State) ->
	{noreply, State}.
	
init(Args) ->
	State = [],
	{ok,  State}.
	
% get_class_queue(Class, State) ->
	% {G, F, E, D, C, B, A, S} = State,
	% case Class of
		% "G" -> G;
		% "F" -> F;
		% "E" -> E;
		% "D" -> D;
		% "C" -> C;
		% "B" -> B;
		% "A" -> A;
		% "S" -> S
	% end.

check_player_play(Select) ->
	case Select of
		wait -> smo_logger:msg("can not find opponent");
		{FirstQPid, RanQPid} -> gen_server:cast(smo_ranking_guardian, {both_player_play, FirstQPid, RanQPid})
	end.
	
cancel_rank_play(PlayerPid) ->
	lists:foreach(fun(Class) -> clear_pid_from_rank(Class, PlayerPid) end, ["G", "F", "E", "D", "C", "B", "A", "S"]),
	gen_server:cast(PlayerPid,  {send, [16#81, 16#0b, 0]}).

clear_pid_from_rank(Class, Pid) ->
	AllInClass = 
	case get(Class) of
		undefined -> [];
		Queue -> Queue
	end,
	%io:format("all player in class:~p are ~p then remove pid ~p from Class ~p ~n", [Class, AllInClass, Pid, Class]),
	put(Class, AllInClass -- [Pid]).
	
get_class_queue(Class) ->
	case get(Class) of
		undefined -> [];
		Queue -> check_alive(Queue)
	end.
	
terminate(Reason, State) ->
	ok.
	
check_alive([]) -> [];
check_alive([PlayerPid|Tail]) ->
	case process_info(PlayerPid, status) of
		{status, waiting} ->  [PlayerPid]++check_alive(Tail);
		%{status, running} ->  [PlayerPid]++check_alive(Tail);
		Error -> io:format("PlayerPid is Not Alive case =>~p~n", [Error]),
			check_alive(Tail)
	end.

get_player_play(Class, SPlayer) ->
	ClassQ = get_class_queue(Class),
	case length(ClassQ) > 1 of
		true -> 
			% เลือกห้องจาก OnlyAlive เมื่อเลือกได้แล้วตัด Pid นั้นและ Pid ที่เข้ามาออกจาก Queue
			[FirstQPid|Tail] = ClassQ,
			[RanQPid] = function_utility:random_select(1, Tail),
			put(Class, (ClassQ -- [FirstQPid]) -- [RanQPid]),
			{FirstQPid, RanQPid};
		_ -> wait
	end.		
			
both_player_play(FirstQPid, RanQPid) ->
		%[LoginName1, PlayerName1] = lib_lobby_protocol:get_user_data_pid(FirstQPid, [user_id, player_name_utf]),
		smo_logger:msg("both player join room"),
		gen_server:cast(FirstQPid, {request_opprank_info, FirstQPid, RanQPid}),
		gen_server:cast(RanQPid, {request_opprank_info, FirstQPid, RanQPid}),
		Rank = 2,
		gen_server:cast(FirstQPid, {arena_to_create, RanQPid, Rank, undefined}).
		%smo_arena:start([FirstQPid, LoginName1, PlayerName1, 2, 0, "", "", 0, 30, 2, 1]),
		%[SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver]
		% smo_logger:fmsg("process whose I want its to create arena is = ~p",[FirstQPid]),
		% gen_server:cast(RanQPid, {arena_to_join, RanQPid, FirstQPid}).
