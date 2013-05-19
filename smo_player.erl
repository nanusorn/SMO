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
-module(smo_player).
-behaviour(gen_server).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule exports
-export([send_message/2]).% called by dispatcher to deliver a message
-export([start/1]).       % called by acceptor
-export([start_link/1]).  % called by supervisor

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).
-export([countdown_to_losser/2]).


%% internal exports
-export([reader_start/3]).

%% library import
-import(lib_msg_controller, [msg_group/5]).

-include_lib("kernel/include/inet.hrl"). 	% needed for hostent structure
-include("record.hrl").						% global record define

get_client_name(Socket) ->
	case inet:peername(Socket) of
	{error, _Reason} -> "?.?.?.?:?";
	{ok, {{A,B,C,D}, Port}} ->
		case inet:gethostbyaddr({A,B,C,D}) of
		{error, _Reason} ->
			io_lib:fwrite("~B.~B.~B.~B:~B", [A,B,C,D, Port]);
		{ok, Hostent} ->
			io_lib:fwrite("~s:~B", [Hostent#hostent.h_name, Port])
		end
	end.

%% returns a string representation of the number of seconds since Epoch
string_timestamp() ->
	{MegaSecs,Secs,_Microsecs} = now(),
	integer_to_list(MegaSecs) ++ integer_to_list(Secs).

%% makes a notification of joining, leaving, etc.
%format_notification(Name, String) ->
%	string_timestamp() ++ " " ++ Name ++" "
%		++ lists:filter(fun(X) -> (X /= $\r)and(X /= $\n)end, String).

%% formats a received message that the user typed
%format_message(Name, B) ->
%	string_timestamp() ++ " <" ++ Name ++ "> "
%		++ lists:filter(fun(X)->(X /= $\r)and(X /= $\n)end, binary_to_list(B)).

%% saves a log entry: <client address> <message>
log_message(Name, Format, Args) ->
	smo_logger:fmsg(Name ++ " " ++ Format, Args).

log_message(Name, String) ->
	smo_logger:msg(Name ++ " " ++ String).

reader_start(Socket, Name, PlayerPid) ->
	%%smo_logger:msg("Por: reader_start"),
	reader_loop(Socket, Name, PlayerPid).

reader_loop(Socket, Name, PlayerPid) ->
	case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
		%%smo_dispatcher:string(format_message(Name, B)),
		%%smo_logger:msg("Por: incomming data"),
		%%smo_player:action_request(PlayerPid, B),
		gen_server:call(PlayerPid, {action_request, B}, infinity),
		reader_loop(Socket, Name, PlayerPid);
	{error, einval} ->
		smo_logger:msg("Por: error einval?"),
		unlink(PlayerPid),
		gen_server:cast(PlayerPid, socket_closed);
	{error, closed} ->
		unlink(PlayerPid),
		gen_server:cast(PlayerPid, socket_closed);
	_Error ->
		unlink(PlayerPid),
		gen_server:cast(PlayerPid, socket_closed)
	end.
	
deliver_message(_Name, Socket, String) ->
	%%%log_message(Name, "==> \"~s\\r\\n\"", [String]),
	gen_tcp:send(Socket, String ++ "\r\n").

server_name() ->
	case application:get_env(name) of
	{ok, Name} -> Name;
	undefined -> 
		smo_logger:msg("socket: server Name unspecified in app environment. Using default."),
		"Summoner Master Online Server" % default name
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intermodule exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% called by dispatcher
send_message(String, Pid) ->
	%%%smo_logger:fmsg("pid=~w called socket:send_message(\"~s\", ~w)", [self(), String, Pid]),
	gen_server:cast(Pid, {message, String}). % ok
	
% send(Term, Pid) ->
	% gen_server:cast(Pid, {send, Term}).
	
%% called by reader_loop to manage group and function, player request.
% action_request(PlayerPid, B) ->
	% gen_server:call(PlayerPid, {action_request, B}).

% request_player_name(PlayerPid) ->
	% gen_server:call(PlayerPid, {request_player_name}).
	
% called by acceptor
start(Socket) ->
	%%smo_logger:msg("Por: smo_player:start"),
	ExtraArgs = [{socket, Socket}],
	supervisor:start_child(smo_player_sup, ExtraArgs). % simple_one_for_one

%% called by supervisor to start the socket gen_server
start_link(Args) ->
	Options = [],
	gen_server:start_link(smo_player, Args, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, _From, State) ->
	[_UserID, _PlayerName, PlayerNameUTF, PlayingStatus, _Point, Rank, _Rating, _Avatar, _DeckUsed, _Socket, _Name, ArenaPid, _ArenaID, PidDB] = State,
	case Request of
	{get_player_info} ->
		%%PlayerNameSize = lists:flatlength (State#new_player_data.player_name_utf),
		%%PlayerInfo = [PlayerNameSize,State#new_player_data.rank,State#new_player_data.playing_status],
		PlayerNameSize = lists:flatlength(PlayerNameUTF),
		PlayerInfo = [PlayerNameSize,Rank,PlayingStatus],
		{reply, PlayerInfo, State};
	{action_request, B} ->
		%%smo_logger:fmsg("=========================> ArenaPid = ~w", [ArenaPid]),
		List = binary_to_list(B),
		smo_logger:fmsg("List (action_request) = ~w",[List]),
		case List of
			[Group, Function | Data] ->
				msg_group(State, self(), Group, Function, Data);
			_ ->	
				smo_logger:msg("smo_dispatcher: Data message error")
		end,
		{reply, ok, State};
	{get_dbpid} ->
		smo_logger:fmsg("=*=*=*=*=*=*=*=* Request PidDB = ~w", [PidDB]),
		{reply, PidDB, State};
	{get_arena} ->
		smo_logger:msg("Get_Arena in handle_call"),
		%%{reply, State#new_player_data.arena_pid, State};
		{reply, ArenaPid, State};
	% {request_player_name} ->
		% smo_logger:fmsg("~p",[State]),
		% %%{reply, State#new_player_data.player_name, State};
		% {reply, PlayerName, State};
	% {arena_to_create} ->
		% [LoginName1, PlayerName1] = lib_lobby_protocol:get_user_data_pid(self(), [user_id, player_name_utf]),
		% %SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver, SubTime
		% smo_arena:start([self(), LoginName1, PlayerName1, 2, 0, "", "", 0, 30, 2, 1, 0]),
		% smo_logger:fmsg("process whose create arena is = ~p",[self()]),
		% {reply, ok, State};
	_Other ->
		%%log_message(Name, "unknown call, pid=~w, request=~w", [From, Other]),
		{noreply, State}
	end.

handle_cast(Request, State) ->
	[UserID, PlayerName, PlayerNameUTF, PlayingStatus, Point, Rank, Rating, Avatar, DeckUsed, Socket, Name, ArenaPid, ArenaID, PidDB] = State,
	%%{Socket, Name} = State,
	case Request of
	{message, String} ->
		%%deliver_message(State#new_player_data.name, State#new_player_data.socket, String),
		deliver_message(PlayerName, Socket, String),
		{noreply,State};
	{send, Term} ->
		%smo_logger:msg("smo_logger: send message to client"),
		%io:format("send ~p~n", [Term]),
		%%gen_tcp:send(State#new_player_data.socket, list_to_binary(Term)),
		gen_tcp:send(Socket, list_to_binary(Term)),
		{noreply,State};
	socket_closed ->
		%%log_message(State#new_player_data.name, "socket closed"),
		log_message(Name, "socket closed"),
		{stop,normal,State};
	{other_duplicate_login, SPlayer, Login} ->
		smo_logger:fmsg("send msg dupicate login ~p~n", [Login]),
		gen_server:cast(self(), {send, [16#00, 16#03]}),
		gen_server:cast(self(),{force_terminate_process, SPlayer, Login}),
		{noreply,State};
	{force_terminate_process, SPlayer, Login} ->
		gen_server:cast(SPlayer, {return_to_check_duplicate_login, Login}),
		smo_logger:fmsg("force last process logout {~p:~p}~n", [Login, SPlayer]),
		player_logout(),
		{noreply,State};
	{return_to_check_duplicate_login, Login} ->
		smo_logger:fmsg("reject login case duplicate at ~p ~n", [now()]),
		%reject_login(),
		return_to_check_duplicate_login(Login),
		{noreply,State};
	{update_state, Result, NewPidDB} ->
		%% do update player guardian for new logged on player
		smo_player_guardian:connected(self(), NewPidDB),
		smo_dispatcher:connected(self()),
		
		%smo_logger:msg("Do----> update_state of new_player_data"),
		%[{NewUID, NewPlayerID, NewPNUTF, NewAvatar, NewDeckUsed}] = Result,
		[{NewPlayerID, NewPNUTF, NewAvatarSet, NewAvatarID, NewDeckUsed}] = Result,
%		<<Point2Byte:16>> = <<Point:16>>,
%		<<Rank2Byte:16>> = <<Rank:16>>,
%		<<Rating2Byte:16>> = <<Rating:16>>,
%		<<Avatar2Byte:16>> = <<Avatar:16>>,
		%%PNASCII = lib_ascii_unicode:unicode_to_ascii(PNUTF),
		
		NewState = [NewPlayerID, PlayerName, NewPNUTF, PlayingStatus, Point, Rank, Rating, {list_to_integer(NewAvatarSet), list_to_integer(NewAvatarID)}, NewDeckUsed, Socket, Name, ArenaPid, ArenaID, NewPidDB],
		%{ok, PNASCII} = lib_utf8:from_binary(NewPNUTF),
		%NewState = [NewPlayerID, PNASCII, NewPNUTF, PlayingStatus, Point, Rank, Rating, NewAvatar, NewDeckUsed, Socket, Name, ArenaPid, NewPidDB],
		{noreply, NewState};
%		{noreply,  #new_player_data{user_id = PlayerID,
%										player_name = PNASCII,
%										player_name_utf = PNUTF,
%										playing_status = 0,
%										point = Point,
%										rank = Rank,
%										rating = Rating,
%										avatar = Gender,
%										deck_used = DeckUsed,
%										socket = State#new_player_data.socket,
%										name = State#new_player_data.name,
%										arena_pid = self()}};
	{set_arena, NewArenaPid, NewArenaID} ->
		%smo_logger:fmsg("Setting arena = ~w",[NewArenaPid]),
		%smo_logger:fmsg("State    = ~w",[State]),
		NewState = [UserID, PlayerName, PlayerNameUTF, PlayingStatus, Point, Rank, Rating, Avatar, DeckUsed, Socket, Name, NewArenaPid, NewArenaID, PidDB],
		%smo_logger:fmsg("NewState = ~w",[NewState]),
		{noreply, NewState};
	{arena_to_create, JoinerPid, SetRank, TID} ->
		[LoginName1, PlayerName1, Avatar1] = lib_lobby_protocol:get_user_data_pid(self(), [user_id, player_name_utf, avatar]),
		smo_arena:start([self(), LoginName1, PlayerName1, Avatar1, JoinerPid, SetRank, 0, "", "", 0, 30, 2, 1, 1, whereis(smo_arena_guardian), whereis(smo_player_guardian), whereis(smo_dispatcher), TID, [0]]),
		% Rank = 2 is Rank Play, = 3 is Tournament
		%smo_arena:start([self(), LoginName1, PlayerName1, JoinerPid, 2, 0, "", "", 0, 30, 2, 1, 1, whereis(smo_arena_guardian), whereis(smo_player_guardian), whereis(smo_dispatcher)]),
		%smo_logger:fmsg("process whose create arena is = ~p",[self()]),
		{noreply, State};
	{leave_arena} ->
		smo_logger:msg("----------------------------->>>>> smo_player leave_arena"),
		NewState = [UserID, PlayerName, PlayerNameUTF, PlayingStatus, Point, Rank, Rating, Avatar, DeckUsed, Socket, Name, 0, "", PidDB],
		{noreply, NewState};
	{login, _LoginName, Validate, Gender} ->
		[PlayerName] = lib_lobby_protocol:get_user_data_pid (self(), [player_name_utf]),
		PlayerNameSize = lists:flatlength (PlayerName),
		gen_server:cast(self(), {send, [16#0, 16#0, Validate, PlayerNameSize] ++ PlayerName ++ [Gender]}),
		{noreply, State};	
	{arena_to_join, RoomID, RoomPid, TID} ->
		[LoginName] = lib_lobby_protocol:get_user_data_pid(self(), [user_id]),
		JoinerAvatar = lib_lobby_protocol:get_user_data_pid(self(), avatar),
		gen_server:cast(RoomPid, {join_arena, self(), LoginName, JoinerAvatar, RoomID, 1, 2, TID, 0}),
		%lib_arena:join_rank_arena(self(), RoomID, RoomPid, LoginName),
		{noreply, State};
	{private_msg, Data} ->
		case get_login_from_msg(Data) of
			{ToPlayerName, MsgData} ->
				%ToPlayerNameAscii = ascii_unicode:unicode_to_ascii (ToPlayerName),
				case lib_lobby_protocol:get_user_pid_name_by_name(ToPlayerName) of
					[] ->	gen_server:cast(self(), {send, [16#02, 16#01, 16#00]});
					[{ReceiverPid, LoginName}] -> 
						SenderAvatar = lib_lobby_protocol:get_user_data_pid(self(), avatar),
						send_private_msg(self(), ReceiverPid, LoginName, ToPlayerName, MsgData, SenderAvatar)
				end;
			[] ->	io:format ("Player name not match~n"),
				gen_server:cast(self(), {send, [16#02, 16#01, 16#00]})
		end,
		{noreply, State};
	{request_opprank_info, FirstQPid, RanQPid} ->
		if
			FirstQPid =:= self() ->
			UserId = lib_lobby_protocol:get_user_data_pid(FirstQPid, user_id),
			SQLCommand = "CALL smno.sp_get_player_rank ('" ++ UserId ++ "');",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
			case  QueryResult of
				[{selected, _, [{OppRating, OppRank, _, _, _}]}, _] -> gen_server:cast(RanQPid, {send, [16#81, 16#09, <<OppRating:32>>, <<OppRank:32>>]});
				_ -> gen_server:cast(RanQPid, {send, [16#81, 16#09, <<0:32>>, <<0:32>>]})
			end;
			true ->
			UserId = lib_lobby_protocol:get_user_data_pid(RanQPid, user_id),
			SQLCommand = "CALL smno.sp_get_player_rank ('" ++ UserId ++ "');",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
			case  QueryResult of
				[{selected, _, [{OppRating, OppRank, _, _, _}]}, _] -> gen_server:cast(FirstQPid, {send, [16#81, 16#09, <<OppRating:32>>, <<OppRank:32>>]});
				_ -> gen_server:cast(FirstQPid, {send, [16#81, 16#09, <<0:32>>, <<0:32>>]})
			end
		end,
		{noreply, State};
	{countdown_to_be_losser} ->
		io:format("send check client alive~n"),
		gen_server:cast(self(), {send, [16#0b, 16#04]}), % Message ส่งไป Client เพื่อ ตรวจสอบว่า Client ยังทำงานอยู่หรือไม่
		Countdown = spawn_link(smo_player, countdown_to_losser, [self(), 20000]), % เริ่มนับเวลาถอยหลัง
		put(countdown_timer, Countdown),
		{noreply, State};
	{player_response} ->
		case get(countdown_timer) of
			undefined -> do_nothing;
			Countdown -> 
				unlink(Countdown),
				erase(countdown_timer),
				exit(Countdown, unuse_player_alive)
		end,
		{noreply, State};
	{stop_process} ->
		{stop,normal,State};
	Other ->
		%%log_message(State#new_player_data.name, "unknown cast, request=~w", [Other]),
		log_message(Name, "unknown cast, request=~w", [Other]),
		{noreply,State}
	end.
	
handle_info(Info, State) ->
	[_UserID, _PlayerName, _PlayerNameUTF, _PlayingStatus, _Point, _Rank, _Rating, _Avatar, _DeckUsed, _Socket, Name, _ArenaPid, _ArenaID, _PidDB] = State,
	%%{_Socket, Name} = State,
	%%log_message(State#new_player_data.name, "got unknown message, info=~w", [Info]),
	log_message(Name, "got unknown message, info=~w",[Info]),
	{noreply, State}.

init(Args) ->
	{socket, Socket} = Args,
	inet:setopts(Socket, [binary, {packet, 2}, {active, false}]),
	Name = get_client_name(Socket),
	log_message(Name, "connected, pid=~w", [self()]),
	%%smo_dispatcher:connected(),
	%deliver_message(Name, Socket, "Welcome to "++server_name()), % greet client
	%%smo_dispatcher:string(format_notification(Name,"connected.")), % inform
	spawn_link(smo_player, reader_start, [Socket, Name, self()]),
	%[UserID, PlayerName, PlayerNameUTF, PlayingStatus, Point, Rank, Rating, Avatar, DeckUsed, Socket, Name, ArenaPid, ArenaID, PidDB]
	{ok, [[],[],[],0,0,0,0,[],[],Socket,Name,0,[],0]}.
	%{ok,  #new_player_data{user_id = "",
	%%					player_name = "",
	%					player_name_utf = "",
	%					playing_status = 0,
	%					point = 0,
	%					rank = 0,
	%					rating = 0,
	%					avatar = "",
	%					deck_used = "",
	%					socket = Socket,
	%					name = Name,
	%					arena_pid = self()}}.
						
%% called when handle_cast returns stop.
%% when a shutdown occurs, all sockets are brutally killed by smo_player_sup
terminate(Reason, State) ->
	[UserID, _PlayerName, _PlayerNameUTF, _PlayingStatus, _Point, _Rank, _Rating, _Avatar, _DeckUsed, Socket, Name, ArenaPid, _ArenaID, PidDB] = State,
	%%{Socket, Name} = State,
	%%inet:close(State#new_player_data.socket),
	inet:close(Socket),
	%%log_message(State#new_player_data.name, "terminating, pid=~w, reason=~w", [self(), Reason]),
	log_message(Name, "terminating, pid=~w, reason=~w", [self(),Reason]),
	
	%% terminate itself to arena if registered.
	% SPlayer นี้อยู่ใน Arena หรือไม่
	case is_pid(ArenaPid) of
		true ->
							% case process_info(ArenaPid, status) of
								% undefined ->
									% smo_logger:msg("room is not process then boardcase remove arena"),
									% smo_logger:fmsg("found dead arena is ~p ~n", [ArenaPid]),
							
							%smo_arena_guardian:delete_arena(ArenaID),
								% _ -> ""
							% end,
			% ส่งไป บอก ArenaPid ว่า SPlayer นี้ Leave Room กรณี Disconnect คือการปิด Client ออกไป 
			gen_server:call(ArenaPid, {leave_room, disconnect});
		false -> 
			smo_logger:msg("ignore leave room message")
	end,
	case UserID of
		[] -> "";
		_ -> 
			gen_server:cast(smo_player_guardian, {update_player_logout, UserID}),
			lib_lobby_protocol:remove_player_from_mnesia(self(), UserID)
	end,	
	case is_pid(PidDB) of
		true ->
			lib_database:disconnect(),
			garbage_collect(PidDB),
			smo_player_guardian:disconnected(PidDB);
		_ -> ""
	end,
	smo_dispatcher:disconnected(self()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Local function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_login_from_msg([ToNameSize | Data]) ->
	DataSize1 = lists:flatlength (Data),
	if DataSize1 >= ToNameSize ->
		ToPlayerName = lists:sublist (Data, 1, ToNameSize),
		MsgData = lists:nthtail (ToNameSize, Data),
		{ToPlayerName, MsgData};
	   true -> {error, data_name_size_error}		
	end.
	
send_private_msg(PlayerPid, ReceiverPid, LoginName, _ToPlayerName, MsgData, Avatar) ->
	PlayerName = lib_lobby_protocol:get_user_data_pid(PlayerPid, player_name),
	SQLCommand = "CALL smno.sp_check_pm ('" ++ LoginName ++ "' ,'" ++ PlayerName ++ "');",
	case lib_database:get_query(SQLCommand) of
		[{selected, _, [{1}]} | _] ->
			gen_server:cast(PlayerPid, {send, [16#02, 16#01, 16#01]}),
			%PlayerSender = ascii_unicode:unicode_to_ascii (PlayerName),
			PlayerNameSize = lists:flatlength(PlayerName),
			{AvatarSet, AvatarID} = Avatar,
			gen_server:cast(ReceiverPid, {send, [16#82, 16#01, AvatarSet, AvatarID, PlayerNameSize] ++ PlayerName ++ MsgData});
		[{selected, _, [{0}]} | _] ->
			gen_server:cast(PlayerPid, {send, [16#02, 16#01, 16#02]});
		R -> io:format ("Query check pm ~p~n", [R])
	end.
	
player_logout() ->
	receive
	after 3000 ->
		terminate_process()
	end.
	
terminate_process() ->
	receive
	after 1000 ->
		smo_logger:fmsg("terminate_process ~p at ~p ~n", [self(), now()]),
		gen_server:cast(self(), {stop_process})
	end.
	
return_to_check_duplicate_login(Login) ->
	receive
	after 4500 ->
		smo_logger:fmsg("return to check duplcate login ~p~n", [Login]),
		lib_player_login:check_duplicate_login(self(), Login)
	end.

countdown_to_losser(BeCheckPid, TimeRemain) ->
	receive
	after TimeRemain ->
		gen_server:cast(BeCheckPid, {stop_process})
	end.
	
% reject_login() ->
	% receive
	% after 2500 ->
		% lib_player_login:reject_login(self(), duplicate_login)
	% end.
