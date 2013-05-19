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
-module(lib_player_profile).
-export([set_player_profile/2]).
-import(lists, [sublist/3, nthtail/2]).

% to_lower_case ([]) -> [];
% to_lower_case ([Chr | Text]) ->
	% if
		% Chr >= 65, Chr =< 90 ->
			% [Chr + 32] ++ to_lower_case (Text);
		% true ->
			% [Chr] ++ to_lower_case (Text)
	% end.

set_player_profile(SPlayer, [LoginSize | Data]) ->
	Login = lists:sublist (Data, 1, LoginSize),
	ProfileData = lists:nthtail (LoginSize, Data),
	create_player_profile(SPlayer, Login, ProfileData).

create_player_profile(PlayerPid, Login, [Gender, PlayerNameSize | Data]) ->
	smo_logger:fmsg("PlayerNameSize =:= ~p, Data ~p~n", [PlayerNameSize, Data]),
	%LowerLogin = to_lower_case (Login),
	PlayerName = sublist (Data, 1, PlayerNameSize),
	[PreconDeck] = nthtail(PlayerNameSize, Data),
	%SQLCommand = "CALL smno.sp_create_profile ('" ++ LowerLogin ++ "', " ++ integer_to_list (Gender) ++ ", '" ++ PlayerName ++ "', " ++ integer_to_list (PreconDeck) ++ ");",
	SQLCommand = "CALL smno.sp_create_profile ('" ++ Login ++ "', " ++ integer_to_list (Gender) ++ ", '" ++ PlayerName ++ "', " ++ integer_to_list (PreconDeck) ++ ");",
	smo_logger:fmsg("~p,~n", [SQLCommand]),
	
	case lib_database:get_query(SQLCommand) of
		[{selected, _, [{1}]}, _] ->
			lib_player_login:check_duplicate_login(PlayerPid, Login); % query_command_gender (PlayerPid, LowerLogin);
		[{selected, _, [{0}]}, _] ->
			gen_server:cast(PlayerPid, {send, [16#00, 16#00, 16#00, 16#04]});
		R ->	io:format ("Create player profile return ~p~n", [R]),
			gen_server:cast(PlayerPid, {send, [16#00, 16#00, 16#00, 16#04]})
	end.

% query_command_gender (PlayerPid, LoginLower) ->
	% NumOfUser = gen_server:call(smo_player_guardian, {get_ccu}),
	% SQLCommand = "CALL smno.sp_insertLogPlayer('"++ LoginLower ++"', 1);",
	% %SQLCommand = "CALL smno.sp_insertLogPlayer ('" ++ LoginLower ++ "', 1);",
	% smo_logger:fmsg("~p,~n", [SQLCommand]),
	% case lib_database:get_query(SQLCommand) of
		% [{selected, _, [{Gender}]}, _] ->
			% request_player_data (PlayerPid, LoginLower, Gender);
		% R ->	io:format ("Get gender error ~p ~n", [R]),
			% gen_server:cast(PlayerPid, {send, [16#00, 16#00, 16#00, 16#03]})
	% end.
% 
% request_player_data(PlayerPid, LoginLower, Gender) ->
	% %SQLCommand = "SELECT * From smno.data_player where player_id = '" ++ LoginLower ++ "';",
	% SQLCommand = "CALL smno.sp_get_data_player('" ++ LoginLower ++ "');",
	% QResult = lib_database:get_query(SQLCommand),
	% smo_logger:fmsg("~p = ~p~n", [SQLCommand, QResult]),
	% case QResult of
		% [{selected, _, []}, _] ->
			% io:format("Request player data fault ~p~n", [LoginLower]),
			% gen_server:cast(PlayerPid, {send, [16#00, 16#00, 16#00, 16#00]});
		% [{selected, _, Result}, _] ->
% %			io:format ("Result ~p~n", [Result]),
			% lib_player_login:update_ccu(),
			% lib_lobby_protocol:add_player_data_to_mnesia (PlayerPid, Result, Gender),
			% %%%ChannelPid ! {chan, PlayerPid, {login, LoginLower, 1, Gender}};
			% %%%gen_server:cast(PlayerPid, {login, LoginLower, 1, Gender});
			% [PlayerName] = lib_lobby_protocol:get_user_data_pid(self(), [player_name_utf]),
			% PlayerNameSize = lists:flatlength (PlayerName),
			% gen_server:cast(self(), {send, [16#0, 16#0, 1, PlayerNameSize] ++ PlayerName ++ [Gender]});
		% [{error, Reason}] ->
			% io:format("Query error : ~p~n", [Reason]),
			% gen_server:cast(PlayerPid, {send, [16#00, 16#00, 16#00, 16#00]})
	% end.
