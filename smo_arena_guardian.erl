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
-module(smo_arena_guardian).
-behaviour(gen_server).
-created_by('Scroge McDuck at playpal.co.th').

-import(lists, [foreach/2, reverse/2, append/2, split/2]).

%% intermodule exports
-export([start_link/0]).  		% called by supervisor
%-export([create_arena/4]).	% called by smo_arena
-export([delete_arena/1]).
-export([update_arena/2]).
-export([get_total_arena/0]).
-export([find_arena_pid/1]).
-export([find_arena_by_player/1]).
-export([find_arena_by_pid/1]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).
-export([create_room/2]).
-export([request_room_info/1]).


% create_arena(RoomID, RoomName, PlayerPid, RoomPid) ->
	% gen_server:call(smo_arena_guardian, {create_arena, RoomID, RoomName, PlayerPid, RoomPid}).
delete_arena(RoomID) ->
	gen_server:cast(smo_arena_guardian, {delete_arena, RoomID}).
update_arena(RoomID, SPlayer) ->
	gen_server:call(smo_arena_guardian, {update_arena, RoomID, SPlayer}).
get_total_arena() ->
	gen_server:call(smo_arena_guardian, get_total_arena).
find_arena_pid(RoomID) ->
	gen_server:call(smo_arena_guardian, {find_arena_pid, RoomID}).
find_arena_by_player(SPlayer) ->
	gen_server:call(smo_arena_guardian, {find_arena_by_player, SPlayer}).
find_arena_by_pid(ArenaPid) ->
	gen_server:call(smo_arena_guardian, {find_arena_id, ArenaPid}).
create_room(SPlayer, Arg) -> 
	gen_server:call(smo_arena_guardian, {create_room, SPlayer, Arg}).
request_room_info(SPlayer) -> 
	gen_server:cast(smo_arena_guardian, {request_room_info, SPlayer}).
	
	%Arg = [ArenaPid, Rank, RoomName, Password, PlayTime, NumberOfPlayer, AllowObserver, SubTime].

%% used to start the gen_server
start_link() ->
	ServerName = {local, smo_arena_guardian}, % name gets register()ed
	Module = smo_arena_guardian,
	Options = [],
	Args = [],
	gen_server:start_link(ServerName, Module, Args, Options).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	ArenaList = State,
	{_Pid, _Tag} = From,
	case Request of
		{create_room, SPlayer, [RoomPid, Rank, RoomName, Password, PlayTime, NumberOfPlayer, AllowObserver, SubTime, IsReady, Mode]} ->
			Result =
			case lib_lobby_protocol:create_room(RoomPid, Rank, RoomName, Password, PlayTime, NumberOfPlayer, AllowObserver, SubTime, IsReady, Mode) of
				{ok, RoomID} -> 
					ArenaList2 = [{RoomID, RoomName, [SPlayer], RoomPid} | ArenaList],
					{{ok, RoomID}, ArenaList2};
				{error, Reason} -> {{error, Reason}, ArenaList}
			end,
			{Result2, ArenaList3} = Result,
			{reply, Result2, ArenaList3};
			
			% {create_arena, RoomID, RoomName, PlayerPid, RoomPid} ->
			% ArenaList2 = [{RoomID, RoomName, [PlayerPid], RoomPid} | ArenaList],
			% smo_logger:fmsg("ArenaList2 is now = ~w", [ArenaList2]),
			% {reply, ok, ArenaList2};

		{update_arena, RoomID, PlayerPid} ->
			update_room(RoomID, ArenaList, PlayerPid, []),
			{reply, ok, ArenaList};
		get_total_arena ->
			TotalArena = length(ArenaList),
			{reply, TotalArena, ArenaList};
		{find_arena_by_player, SPlayer} ->
			smo_logger:msg("Finding arena pid by plyaer pid"),
			case find_room_by_player(SPlayer, ArenaList) of
				{ok, RoomPid} ->
					smo_logger:msg("Found arena pid... "),
					{reply, RoomPid, State};
				error ->
					smo_logger:msg("Not found arena... "),
					{reply, error, State}
			end;
		{find_arena_pid, RoomID} ->
			smo_logger:msg("We're dig up here"),
			smo_logger:fmsg("ArenaList = ~w", [ArenaList]),
			case find_room(RoomID, ArenaList) of
				{ok, RoomPid, RoomName} ->
					smo_logger:fmsg("RoomPid = ~w RoomName = ~w", [RoomPid, RoomName]),
					{reply, RoomPid, State};
				error ->
					smo_logger:msg("Error not found room pid from roomid"),
					{reply, error, State}
			end;
		{find_arena_id, ArenaPid} ->
			case find_room_id_by_pid(ArenaPid, ArenaList) of
				{ok, RoomID} ->
					smo_logger:fmsg("RoomID = ~w", [RoomID]),
					{reply, RoomID, State};
				error ->
					smo_logger:msg("Error can not found room id from room pid"),
					{reply, error, State}
			end;
		{get_room_info, ArenaPid, Field} ->
			{ok, Info} = lib_lobby_protocol:get_room_infomation(ArenaPid, Field),
			{reply, Info, State};
		{get_room_rank, ArenaPid} ->
			Result = lib_lobby_protocol:get_room_rank(ArenaPid),
			{reply, Result, State};
		{get_room_id, ArenaPid} ->
			Result = lib_lobby_protocol:get_room_id(ArenaPid),
			{reply, Result, State};
		{debug_get_state} ->
			smo_logger:fmsg("State = ~w",[State]),
			{noreply,State};			
		_Other ->
			{noreply,State}
	end.

handle_cast(Request, State) ->
	ArenaList = State,
	case Request of
		% {create_arena, RoomID, RoomName, PlayerPid, RoomPid} ->
			% {noreply,State};
		{request_room_info, SPlayer} -> 
			lib_lobby_protocol:request_room_info(SPlayer),
			{noreply,State};
		{update_arena, RoomID, PlayerPid} ->
			update_room(RoomID, ArenaList, PlayerPid, []),
			{noreply, ArenaList};
		{delete_arena, RoomID} ->
			lib_lobby_protocol:remove_room(RoomID), 
			smo_player_guardian:update_remove_arena(RoomID),
			{ok, ListLeft} = delete(RoomID, ArenaList),
			{noreply, ListLeft};
		{set_room_infomation, RoomID, Field, Status} ->
			lib_lobby_protocol:set_room_infomation(RoomID, Field, Status),
			{noreply, ArenaList}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

init(_Args) ->
	State = [],
	{ok,  State}.

%% called when handle_cast returns stop.
terminate(_Reason, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% no channel pid required.
%%get_channel_pid (_, []) -> {error};
%%get_channel_pid (RoomPid, [{RoomID, _, _, RoomPid, ChannelPid} | _]) -> {ok, ChannelPid, RoomID};
%%get_channel_pid (RoomPid, [_ | T]) -> get_channel_pid (RoomPid, T).

% find_room_leave(RoomPid, ArenaLists) ->
	% case lists:keysearch(RoomPid, 4, ArenaLists) of
		% {value, {RoomID, _, _, _}} -> {ok, RoomID};
		% _ -> error
	% end.

% find_room_leave (RoomPid, [{RoomID, _, _, RoomPid}|_]) -> {ok, RoomID};
% find_room_leave (RoomPid, [_|T]) -> find_room_leave (RoomPid, T);
% find_room_leave (_, []) -> error.

find_room(RoomID, ArenaLists) ->
	case lists:keysearch(RoomID, 1, ArenaLists) of
		{value, {_, RoomName, _, RoomPid}} -> {ok, RoomPid, RoomName};
		_ -> error
	end.

% find_room(RoomID, [{RoomID, RoomName, _, RoomPid}|_]) -> {ok, RoomPid, RoomName};
% find_room(RoomID, [_|T])       -> find_room(RoomID, T);
% find_room(_,[])           -> error.

find_room_id_by_pid(RoomPid, ArenaLists) ->
	case lists:keysearch(RoomPid, 4, ArenaLists) of
		{value, {RoomID, _, _, _}} -> {ok, RoomID};
		_ -> error
	end.

% find_room_id_by_pid(RoomPid, [{RoomID, _, _, RoomPid}|_]) -> {ok, RoomID};
% find_room_id_by_pid(RoomPid, [_|T]) -> find_room_id_by_pid(RoomPid, T);
% find_room_id_by_pid(_, []) -> error.

find_room_by_player(SPlayer, [{_, _, [SPlayer], RoomPid}|_]) -> {ok, RoomPid};
find_room_by_player(SPlayer, [{_, _, [SPlayer,_], RoomPid}|_]) -> {ok, RoomPid};
find_room_by_player(SPlayer, [{_, _, [_,SPlayer], RoomPid}|_]) -> {ok, RoomPid};
find_room_by_player(SPlayer, [_|T])	-> find_room_by_player(SPlayer,T);
find_room_by_player(_,[]) -> error.

update_room (_, [], _, _) -> smo_logger:msg("Can not find room for update.");
update_room (RoomID, [{RoomID, RoomName, PlayerList, RoomPid}| T], PlayerPid, RoomData) ->
	RoomData ++ [{RoomID, RoomName, PlayerList ++ [PlayerPid], RoomPid}] ++ T;
update_room (RoomID, [H | T], PlayerPid, RoomData) -> update_room (RoomID, T, PlayerPid, RoomData ++ [H]).

% remove_player_from_channel(Pid, {PlayerList, RoomPid, ChannelPid}) ->
	% case Pid of
		% RoomPid ->
			% foreach(	fun (PlayerPid) ->
									% mnesia_play:remove_player_data (PlayerPid)
									% %%ChannelPid ! {chan_closed, PlayerPid},
									% %%controller(PlayerPid, ChannelPid)
								% end, PlayerList
							 % );
		% _ -> io:format("~n")
	% end.

delete(RoomID, ArenaList) -> 
	case lists:keysearch(RoomID, 1, ArenaList) of
		{value, {RoomID, A, B, C}} -> {ok, ArenaList -- [{RoomID, A, B, C}]};
		_ -> {ok, ArenaList}%error%ArenaList
	end.
		
% remove_player_from_channel (_, []) -> [];
% remove_player_from_channel (RoomPid, [{_, _, PlayerList, RoomPid, ChannelPid} | _]) ->
	% io:format ("Remove player from channel~n"),
	% foreach(	fun (PlayerPid) ->
							% mnesia_play:remove_player_data (PlayerPid),
							% ChannelPid ! {chan_closed, PlayerPid},
							% controller(PlayerPid, ChannelPid)
						% end, PlayerList
					 % );
% remove_player_from_channel (RoomPid, [_ | T]) -> remove_player_from_channel (RoomPid, T).

% remove_room(RoomPid, [{RoomID, _, _, RoomPid}|T]) ->
	% io:format ("All player leave room - Room ~p~p closed ~n",[RoomPid, RoomID]),
	% stack_pool:remove_stack (RoomPid),
	% mnesia_play:remove_game_data (RoomPid),
	% query_command:remove_room(RoomID),
	% %%ChannelPid ! {update_remove_room, RoomID},
	% T;
% remove_room(RoomPid, [H|T])       -> [H|remove_room(RoomPid, T)];
% remove_room(_, [])            -> [].

