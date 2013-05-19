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
-module(lib_avatar).

-compile (export_all).

request_all_avatar(SPlayer, Data) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id]) of
		[LoginName] ->
			SQLCommand = "CALL smno.sp_get_player_item ('" ++ LoginName ++ "', 1);",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <<==>> ~p~n", [SQLCommand, QueryResult]),
			case QueryResult of
				[{selected, _, List_A}, _] ->
					%Size_Avatar = lists:flatlength(List_A),
					List_Avatar = arrange(List_A),
					Size_Avatar = check_avatar_size(List_Avatar, 0),
					gen_server:cast(SPlayer, {send, [16#81, 16#0d, Size_Avatar] ++ List_Avatar});
				_ ->
					gen_server:cast(SPlayer, {send, [16#81, 16#0d, 0]})
			end
	end.

arrange([]) -> [];
arrange([{Set, Id, Date}|Tail]) ->  
	case Date of
		null -> [Set, Id, 0] ++ arrange(Tail);
		_ -> 
			case Date > 0 of
				true -> [Set, Id, Date] ++ arrange(Tail);
				false -> arrange(Tail)
			end
	end;
arrange([_|Tail]) -> arrange(Tail).

check_avatar_size([], Size) -> Size;
check_avatar_size([_, _, _| T], Size) -> check_avatar_size(T, Size+1).

player_select_avatar(SPlayer, [Set, Id]) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id]) of
		[LoginName] ->
			SQLCommand = "CALL smno.sp_get_player_item ('" ++ LoginName ++ "', 1);",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <<==>> ~p~n", [SQLCommand, QueryResult]),
			case QueryResult of
				[{selected, _, ListAvatar}, _] ->
					List_A = set_avatar(ListAvatar),
					case [{Set, Id}] -- List_A of
						[] ->
							SQLCommand1 = "CALL smno.sp_set_active_item ('" ++ LoginName ++ "', 1, "++ integer_to_list(Set) ++", "++ integer_to_list(Id) ++");",
							lib_database:get_query(SQLCommand1),
							lib_lobby_protocol:change_player_data(SPlayer, LoginName, avatar, {Set, Id});
						_ -> do_nothing
					end;
				_ -> do_nothing
			end;
		_ -> do_nothing
	end.

set_avatar([]) -> [];
set_avatar([{Set, Id, Date}|Tail]) ->  [{Set, Id}] ++ set_avatar(Tail);
set_avatar([_|Tail]) -> set_avatar(Tail).		
			
			
			
			
			
			
			
