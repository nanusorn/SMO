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
-module(smo_player_guardian).
-behaviour(gen_server).
-created_by('Scroge McDuck at playpal.co.th').

%% intermodule exports
-export([start_link/0]).  % called by supervisor
-export([connected/2]).  % called by smo_player for new logged on player
-export([disconnected/1]). % called by smo_player for termination of player (disconnect)
-export([request_player_list/1]). % called by msg controller for request list of player
-export([request_database_pid/1]).
-export([update_arena/2]).
-export([update_remove_arena/1]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

-import (lists, [foreach/2, split/2, append/2, reverse/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intermodule exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used to start the gen_server
start_link() ->
	ServerName = {local, smo_player_guardian}, % name gets register()ed
	Module = smo_player_guardian,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).
	
connected(Pid, PidDB) -> gen_server:cast(smo_player_guardian, {connected, Pid, PidDB}).
disconnected(PidDB) -> gen_server:call(smo_player_guardian, {disconnected, PidDB}).
request_player_list(SPlayer) -> gen_server:cast(smo_player_guardian, {request_player_list, SPlayer}).
request_database_pid(SPlayer) -> gen_server:call(smo_player_guardian, {request_database_pid, SPlayer}).

%% all regarding room 
update_arena(ArenaID, ArenaData) -> gen_server:cast(smo_player_guardian, {update_arena, ArenaID, ArenaData}).
update_remove_arena(ArenaID) -> gen_server:cast(smo_player_guardian, {update_remove_arena, ArenaID}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	PlayerList = State,
	{Pid, _Tag} = From,
	case Request of 
	{disconnected, PidDB} ->
		PlayerList2 = PlayerList -- [{Pid, PidDB}],
		{reply,ok,PlayerList2};
	{request_player_list, SPlayer} ->
		%%%smo_logger:msg("into list_all_users"),
		UserList = list_all_users(check_player_alive(PlayerList)),
		gen_server:cast(SPlayer, {send, [16#01, 16#05] ++ UserList}),
		{reply, ok, State};
	{request_database_pid, SPlayer} ->
		DbPid = get_database_pid(SPlayer,PlayerList),
		{reply, DbPid, State};
	{get_ccu} ->
		AlivePlayer = check_player_alive(PlayerList),
		NumOfUser = length(AlivePlayer)+1,%length(PlayerList)+1,
		%{links, AllLinks} = process_info(self(), links),
		%NumOfUser = length(AllLinks),
		{reply, NumOfUser, AlivePlayer};
	{check_player_alive} ->
		AlivePlayer = check_player_alive(PlayerList),
		{reply, AlivePlayer, AlivePlayer};
	{get_user_data_pid, PlayerPid, FieldReqire} ->
		 Result = lib_lobby_protocol:get_user_data_pid (PlayerPid, FieldReqire),
		{reply, Result, State};
	Other ->
		{noreply,State}
	end.

handle_cast({connected, Pid, PidDB}, State) ->
		smo_logger:fmsg("new pid for guardian is ~p~n",[Pid]),
		%%PlayerList2 = PlayerList ++ Pid,
		{noreply, [{Pid,PidDB}|State]};
handle_cast({update_arena, ArenaID, ArenaData}, State) ->
	foreach ( fun({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#81, 16#00, <<ArenaID:16>>, 2] ++ ArenaData})
			end, State),
	{noreply, State};
handle_cast({update_remove_arena, ArenaID}, State) ->
	foreach ( fun({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#81, 0, <<ArenaID:16>>, 1]})
			end, State),
	{noreply, State};
handle_cast({update_player_login, LoginName}, State) ->
	send_update_player (State, LoginName, 0),
	{noreply, State};
handle_cast({update_player_logout, LoginName}, State) ->
	send_update_player (State, LoginName, 1),
	{noreply, State};
handle_cast({update_player_status, LoginName}, State) ->
	send_update_player (State, LoginName, 2),
	{noreply, State};
handle_cast({request_player_list, SPlayer}, State) ->
	PlayerList = State,
	UserList = list_all_users(check_player_alive(PlayerList)),
	gen_server:cast(SPlayer, {send, [16#01, 16#05] ++ UserList}),
	{noreply, State};
handle_cast({change_player_data, SPlayer, LoginName, Field, PlayerStatus}, State) ->
	lib_lobby_protocol:change_player_data (SPlayer, LoginName, Field, PlayerStatus),
	{noreply, State};	
handle_cast(Request, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	{noreply, State}.

init(Args) ->
	State = [],
	{ok,  State}.

%% called when handle_cast returns stop.
terminate(Reason, State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%make_player_info(PlyerInfo1, PlayerInfo2) ->
%	PlayerInfo = PlayerInfo1 ++ PlayerInfo2,
%	{PlayerInfo}.
	
list_all_users (L) ->
	list_all_user (L, [], 0).

list_all_user ([], NL, NumList) -> [<<NumList:32>>] ++ NL;
list_all_user ([{PlayerPid,_} | T], NL, NumList) ->
	case lib_lobby_protocol:get_user_data_pid (PlayerPid, [player_name_utf, playing_status]) of
		[PlayerName, Status] ->
			NameSize = string:len(PlayerName),
			PlayerData = NL ++ [NameSize, PlayerName, Status],
			list_all_user (T, PlayerData, NumList + 1);
		_ ->	
			list_all_user (T, NL, NumList)
	end.
	
get_database_pid(_, []) -> error;
get_database_pid(SPlayer, [{SPlayer,Pid} | _]) -> Pid;
get_database_pid(SPlayer, [_|T]) -> get_database_pid(SPlayer, T).
	
send_update_player(PlayerLists, LoginName, UpdateType) ->
	foreach(	fun({PlayerPid, _}) ->
			case lib_lobby_protocol:get_user_data_login(LoginName, [player_name_utf, playing_status]) of
				[PlayerName, Status] ->
					PlayerNameSize = string:len(PlayerName),
					UpdateReply = [16#81, 16#05, PlayerNameSize, PlayerName, UpdateType, Status],
					gen_server:cast(PlayerPid, {send, UpdateReply});
				_ -> user_removed
			end
		end, PlayerLists).

check_player_alive([]) -> [];
check_player_alive([{Pid, PidDB}| PlayerList]) ->
	case process_info(Pid, status) of
		undefined -> check_player_alive(PlayerList);
		_ -> [{Pid, PidDB}] ++ check_player_alive(PlayerList)
	end.
	
