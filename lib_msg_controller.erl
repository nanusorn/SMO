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
-module(lib_msg_controller).
-export([msg_group/5]).
-created_by('Scrooge McDuck at playpal.co.th').

-import(lib_player_login, [check_login/2]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO : Make sure we need to implement logout function over here?
%% or do we need to move to some where else.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
player_logout(SPlayer) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[_LoginName, _Status] ->
			gen_server:cast(SPlayer, {send, [16#00, 16#01]});
		_error ->
			smo_logger:fmsg("Found These ~p~n", [_error])
	end.
	
check_lock_msg ([], FunctionReceive, _, _) -> io:format ("Not allow this msg ~p~n", [FunctionReceive]);
check_lock_msg ([LockFunction|T], FunctionReceive, ProcessReceiver, ActiveMsg) ->
	%io:format("Check Lock Message of ~p~n", [[LockFunction|T]]),
	case LockFunction of
		0 -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
		FunctionReceive -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
		% fe -> 
			% case FunctionReceive of
				% 0 -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
				% _ -> check_lock_msg(T, FunctionReceive, ProcessReceiver, ActiveMsg)
			% end;
		_ -> check_lock_msg(T, FunctionReceive, ProcessReceiver, ActiveMsg)
	end;
check_lock_msg (0, _, ProcessReceiver, ActiveMsg) ->
	gen_server:call(ProcessReceiver, ActiveMsg, infinity).

correct_previous_msg (CheckReceive, [], _, _) ->
	gen_server:cast(self(), {send, [16#83, 16#fb]}),
	io:format ("---- Msg not match to process ~p ---~n", [CheckReceive]);
correct_previous_msg (_, [0], ProcessReceiver, ActiveMsg) -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
correct_previous_msg ([0], _, ProcessReceiver, ActiveMsg) -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
%correct_previous_msg ([fe], _, ProcessReceiver, ActiveMsg) -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
correct_previous_msg ([CheckReceive], [CheckReceive | _], ProcessReceiver, ActiveMsg) -> gen_server:call(ProcessReceiver, ActiveMsg, infinity);
correct_previous_msg (LockMsg, [_ | T], ProcessReceiver, ActiveMsg) ->
	correct_previous_msg (LockMsg, T, ProcessReceiver, ActiveMsg).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Incoming message function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
msg_group (State, SPlayer, Group, Function, Data) ->
	msg_group (State, SPlayer, Group, Function, Data, 0).
	
msg_group (State, SPlayer, Group, Function, Data, LockMsg) ->
	[_UserID, _PlayerName, _PlayerNameUTF, _PlayingStatus, _Point, _Rank, _Rating, _Avatar, _DeckUsed, _Socket, _Name, ArenaPid, _ArenaID, _PidDB] = State,
	case Group of
		16#00 -> ignore_is_not_pid(SPlayer, msg_general_controller, Function, Data);%msg_general_controller (SPlayer, Function, Data);
		16#01 -> ignore_is_not_pid(SPlayer, msg_lobby_controller, Function, Data);%msg_lobby_controller (SPlayer, Function, Data);
		16#02 -> ignore_is_not_pid(SPlayer, msg_chat_controller, Function, Data);%msg_chat_controller (SPlayer, Function, Data);
		
		16#03 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_waiting_step_controller, Function, Data, LockMsg);%msg_waiting_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#04 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_checkup_step_controller, Function, Data, LockMsg); %msg_checkup_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#05 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_draw_step_controller, Function, Data, LockMsg); %msg_draw_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#06 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_discard_step_controller, Function, Data, LockMsg); %msg_discard_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#07 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_interfere_step_controller, Function, Data, LockMsg); %msg_interfere_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#08 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_main_step_controller, Function, Data, LockMsg); %msg_main_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#09 -> ignore_is_not_pid(ArenaPid, SPlayer, msg_mp_clean_step_controller, Function, Data, LockMsg); %msg_mp_clean_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#0a -> ignore_is_not_pid(ArenaPid, SPlayer, msg_eos_step_controller, Function, Data, LockMsg); %msg_eos_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		16#0b -> ignore_is_not_pid(ArenaPid, SPlayer, msg_misc_controller, Function, Data, LockMsg); %msg_misc_controller (ArenaPid, SPlayer, Function, Data, LockMsg);
		
		16#0c -> ignore_is_not_pid(SPlayer, msg_deck_edit_controller, Function, Data, LockMsg);
%		16#e0 -> msg_authen_connection (Pid, S, Function, Data, LockMsg);
		_ -> smo_logger:fmsg("Msg group out of bound : ~p~n", [Group])
	end.
	
ignore_is_not_pid(SPlayer, FunctionCall, Function, Data) ->
	Module = lib_msg_controller,
	case is_pid(SPlayer) of
		true -> Module:FunctionCall(SPlayer, Function, Data);
		_ -> ignore		
	end.
	
ignore_is_not_pid(SPlayer, FunctionCall, Function, Data, LockMsg) ->
	Module = lib_msg_controller,
	case is_pid(SPlayer) of
		true -> Module:FunctionCall(SPlayer, Function, Data, LockMsg);
		_ -> ignore		
	end.
	
ignore_is_not_pid(ArenaPid, SPlayer, FunctionCall, Function, Data, LockMsg) ->
	Module = lib_msg_controller,
	case {is_pid(ArenaPid), is_pid(SPlayer)} of
		{true, true} -> Module:FunctionCall(ArenaPid, SPlayer, Function, Data, LockMsg);
		_ -> ignore		
	end.
	
msg_general_controller (SPlayer, Function, Data) ->
	case Function of
		16#00 ->	check_login(SPlayer, Data);
		16#01 ->	player_logout(SPlayer);
		16#02 ->	lib_player_profile:set_player_profile(SPlayer, Data);
		_ -> smo_logger:fmsg("Msg general controller out of bound : ~p~n", [Function])
	end.
	
msg_lobby_controller(SPlayer, Function, Data) ->
	case Function of 
		%% ร้องขอข้อมูลของห้องทั้งหมด
		16#00 -> smo_arena_guardian:request_room_info(SPlayer);%lib_lobby_protocol:request_room_info(SPlayer);
		16#01 -> lib_arena:create_arena(SPlayer, Data);
		%16#01 -> lib_arena:create_arena(SPlayer, [1, 0, 1, 2] ++ Data);
		16#02 -> 
			smo_logger:fmsg("recieve join msg with Data ~p~n", [Data]),
			lib_arena:join_arena(SPlayer, Data);
%		16#03 -> Pid ! {quick_join, S};
		16#05 -> smo_player_guardian:request_player_list(SPlayer);
		16#06 -> lib_friend:request_all_friend(SPlayer);
		16#07 -> lib_friend:request_friend(SPlayer, Data);
		16#08 -> lib_friend:response_friend(SPlayer, Data);
		16#09 -> lib_rank_play:request_rank_play(SPlayer);
		16#0a -> lib_rank_play:cancel_rank_play(SPlayer);
		16#0b -> lib_rank_play:request_rank_info(SPlayer);
		16#0c -> lib_arena:join_room_arena(SPlayer, Data);
		16#0d -> lib_avatar:request_all_avatar(SPlayer, Data);
		16#0e -> lib_avatar:player_select_avatar(SPlayer, Data);
		
		16#a1 -> lib_tournament:request_match_player_information(SPlayer, Data);
		16#a6 -> lib_tournament:request_valid_tournament(SPlayer); % Done
		16#a7 -> lib_tournament:request_registered_tournament(SPlayer); % Done
		16#a0 -> lib_tournament:request_register(SPlayer, Data); % Done
		16#a8 -> lib_tournament:cancel_tournament_register(SPlayer, Data); %Done
		%16#0e -> lib_tournament:request_next_match_info(SPlayer, Data);
		16#0f -> lib_tournament:request_player_scoreline(SPlayer);
		16#10 -> lib_tournament:request_tournament_info(SPlayer);
		16#a2 -> lib_tournament:player_ready(SPlayer, Data); % Test
		16#a3 -> lib_tournament:cancel_player_ready(SPlayer, Data); % Test
		
		16#e0 -> lib_announcement:request_announcement(SPlayer);
  		_ -> smo_logger:fmsg("Msg lobby controller out of bound : ~w", [Function])
	end.
	
msg_chat_controller (SPlayer, Function, Data) ->
	case Function of
		%% ส่งข้อความหาผู้เล่นทุกคน ( public chat )
		16#00 -> smo_dispatcher:string(Data, SPlayer);
		16#01 -> gen_server:cast(SPlayer, {private_msg, Data});
		_ -> smo_logger:fmsg("Msg chat controller out of bound : ~w", [Function])
	end.
	
msg_waiting_step_controller (ArenaPid, _SPlayer, Function, Data, _LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case Function of
		16#00 -> gen_server:cast(ArenaPid, {joiner_ready});
		16#01 -> correct_previous_msg (NewLockMsg, [m8300], ArenaPid, {owner_start});
		16#02 -> correct_previous_msg (NewLockMsg, [m8301], ArenaPid, {pao_ying_chub, Data});
		16#03 -> check_lock_msg (NewLockMsg, Function, ArenaPid, {play_order_select, Data});
		16#04 -> correct_previous_msg (NewLockMsg, [m8303], ArenaPid, {response_update_initial_data});
		16#10 -> gen_server:cast(ArenaPid, {player_request_surrender, _SPlayer});
		16#fa -> gen_server:cast(ArenaPid, {player_response_surrender, _SPlayer, Data});
		16#fc -> 
			gen_server:call(ArenaPid, {leave_rank_room}, infinity),
			%check_lock_msg ([0], Function, ArenaPid, {leave_rank_room}), 
			gen_server:cast(self(), {leave_arena});
		16#fe -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {request_next_step});
		16#ff -> 
			gen_server:call(ArenaPid, {leave_room, surrender}, infinity),
			%check_lock_msg ([0], Function, ArenaPid, {leave_room}), 
			gen_server:cast(self(), {leave_arena});

%		16#01 -> gen_server:call(ArenaPid, {owner_start});
%		16#02 -> gen_server:call(ArenaPid, {pao_ying_chub, Data});
%		16#03 -> gen_server:call(ArenaPid, {play_order_select, Data});
%		16#04 -> gen_server:call(ArenaPid, {response_update_initial_data});
%		16#fe -> gen_server:call(ArenaPid, {request_next_step});
%%%		16#ff -> lib_arena:leave_room(SPlayer);
%		16#ff -> gen_server:call(ArenaPid, {leave_room}),
%				 gen_server:cast(self(), {leave_arena});
		_ -> io:format("<waiting> Msg out of bound : ~p~n", [Function])
	end.

msg_checkup_step_controller (ArenaPid, _SPlayer, Function, _, _LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case Function of
		16#00 -> check_lock_msg(NewLockMsg, Function, ArenaPid, {req_into_checkup});
%		16#00 -> gen_server:call(ArenaPid, {req_into_checkup});
		_ -> io:format("<checkup> Msg out of bound : ~p~n", [Function])
	end.
	
msg_draw_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case gen_server:call(ArenaPid, {get_game_data, game_step}, infinity) of
		{ok, draw} ->
			case Function of
				16#00 -> correct_previous_msg (NewLockMsg, [m8500], ArenaPid, {req_into_draw});
%%%				16#01 -> check_lock_msg (NewLockMsg, Function, ArenaPid, {ack_abi_into_draw});
				16#02 -> correct_previous_msg (NewLockMsg, [m8502], ArenaPid, {request_draw, Data});
				16#03 -> correct_previous_msg (NewLockMsg, [m8503], ArenaPid, {rec_res_draw});
				16#0f -> correct_previous_msg (NewLockMsg, [m8502], ArenaPid, {skip_draw});
%				16#00 -> gen_server:call(ArenaPid, {req_into_draw});
%%%				16#01 -> gen_server:call(ArenaPid, {ack_abi_into_draw}); %% Never used in the project yet
%				16#02 -> gen_server:call(ArenaPid, {request_draw, Data});
%				16#03 -> gen_server:call(ArenaPid, {rec_res_draw});
%				16#0f -> gen_server:call(ArenaPid, {skip_draw});
				_ -> io:format("<draw> Msg out of bound : ~p~n", [Function])
			end;
		R ->	io:format("Receive draw msg on ~p step~n", [R])
	end.

msg_discard_step_controller (ArenaPid, _SPlayer, Function, Data, _LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case gen_server:call(ArenaPid, {get_game_data, game_step}, infinity) of
	%case mnesia_play:get_game_data(ArenaPid, game_step) of
		{ok, discard} ->
			case Function of
				16#00 -> check_lock_msg (NewLockMsg, Function, ArenaPid, {req_into_discard});
%%%				16#01 -> check_lock_msg (NewLockMsg, Function, ArenaPid, {ack_abi_into_discard, S, Data});
				16#02 -> check_lock_msg (NewLockMsg, Function, ArenaPid, {request_discard, Data});
				16#03 -> correct_previous_msg (NewLockMsg, [m8603], ArenaPid, {rec_res_discard});
%				16#00 -> gen_server:call(ArenaPid, {req_into_discard});
%%%				16#01 -> check_lock_msg (NewLockMsg, Function, Pid, {ack_abi_into_discard, S, Data}); %% never used in the ptoject yet
%				16#02 -> gen_server:call(ArenaPid, {request_discard, Data});
%				16#03 -> gen_server:call(ArenaPid, {rec_res_discard});
				_ -> io:format("<discard> Msg out of bound : ~p~n", [Function])
			end;
		{ok, main} ->
			case Function of
				16#03 -> correct_previous_msg (NewLockMsg, [m8603], ArenaPid, {rec_res_discard});
%				16#03 -> gen_server:call(ArenaPid, {rec_res_discard});
				_ -> io:format("<discard> Msg out of bound : ~p~n", [Function])
			end;
		_ ->
			io:format("Discard Msg out of Discard step~n")
	end.

msg_interfere_step_controller (ArenaPid, _SPlayer, Function, Data, _) ->
	%{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case Function of
		16#00 -> gen_server:call(ArenaPid, {req_use_card, Data}, infinity);
		16#01 -> gen_server:call(ArenaPid, {req_use_skill, Data}, infinity);
		16#09 -> gen_server:call(ArenaPid, {skip_interfere}, infinity);
		_ -> io:format("<interfere> Msg out of bound : ~p~n", [Function])
	end.
	
msg_main_step_controller (ArenaPid, SPlayer, Function, Data, LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case gen_server:call(ArenaPid, {get_game_data, game_step}, infinity) of
	%case mnesia_play:get_game_data(ArenaPid, game_step) of
		{ok, main} ->
			io:format("Main msg ~p ~p ~p ~p~n", [Function, Data, SPlayer, LockMsg]),
			case Function of
				16#00 -> check_lock_msg(NewLockMsg, Function, ArenaPid, {req_into_main});
				16#01 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {request_cast_card, Data});
				16#02 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {req_skill_use, Data});
				16#03 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {req_change_line, Data});
				16#04 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {request_assign_attack, Data});
				16#05 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {req_assign_combination, Data});
				16#06 -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {req_break_combination, Data});

				16#07 -> correct_previous_msg (NewLockMsg, [m8803], ArenaPid, {player_select_activation_ability, Data});
				16#08 -> correct_previous_msg (NewLockMsg, [m8804], ArenaPid, {response_update_select_activation});
%				16#00 -> gen_server:call(ArenaPid, {req_into_main});
%				16#01 -> gen_server:call(ArenaPid, {request_cast_card, Data});
%				16#02 -> gen_server:call(ArenaPid, {req_skill_use, Data});
%				16#03 -> gen_server:call(ArenaPid, {req_change_line, Data});
%				16#04 -> gen_server:call(ArenaPid, {request_assign_attack, Data});
%				16#05 -> gen_server:call(ArenaPid, {req_assign_combination, Data});
%				16#06 -> gen_server:call(ArenaPid, {req_break_combination, Data});

%				16#07 -> gen_server:call(ArenaPid, {player_select_activation_ability, Data});
%				16#08 -> gen_server:call(ArenaPid, {response_update_select_activation});

				% ======= Casting Zone ===========  Completed
				%16#09 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {response_card_caster});
				16#09 -> correct_previous_msg (NewLockMsg, [m8805], ArenaPid, {response_card_caster});
				16#0a -> correct_previous_msg (NewLockMsg, [m8806], ArenaPid, {player_select_growth, Data});
%				16#0b -> correct_previous_msg (NewLockMsg, [m8807], ArenaPid, {response_update_select_growth, Data});
				16#0c -> correct_previous_msg (NewLockMsg, [m8808], ArenaPid, {player_select_line, Data});
				16#0d -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {response_update_select_line});
				16#0f -> correct_previous_msg (NewLockMsg, [m880c], ArenaPid, {player_select_ability_target, Data});
				16#10 -> correct_previous_msg (NewLockMsg, [m880d], ArenaPid, {response_update_ability_target});
%				16#09 -> gen_server:call(ArenaPid, {response_card_caster});
%				16#0a -> gen_server:call(ArenaPid, {player_select_growth, Data}); 
%				16#0b -> gen_server:call(ArenaPid, {response_update_select_growth});
%				16#0c -> gen_server:call(ArenaPid, {player_select_line, Data});
%				16#0d -> gen_server:call(ArenaPid, {response_update_select_line});
%				16#0f -> gen_server:call(ArenaPid, {player_select_ability_target, Data});
%				16#10 -> gen_server:call(ArenaPid, {response_update_ability_target});

				% ======= Other Zone =============
				16#16 -> correct_previous_msg (NewLockMsg, [m880e], ArenaPid, {play_interfere_step});
				16#17 -> correct_previous_msg (NewLockMsg, [m880e, fe], ArenaPid, {skip_sub_interfere});
%				16#16 -> gen_server:call(ArenaPid, {play_interfere_step});
%				16#17 -> gen_server:call(ArenaPid, {skip_sub_interfere});

				% ======= Attack Zone =========  Completed
				16#18 -> correct_previous_msg (NewLockMsg, [m8810], ArenaPid, {response_attacker});
				16#19 -> correct_previous_msg (NewLockMsg, [8811], ArenaPid, {select_attacker_ability, Data});
				16#1a -> correct_previous_msg (NewLockMsg, [8812], ArenaPid, {response_update_select_ability});
				16#1b -> correct_previous_msg (NewLockMsg, [m8813], ArenaPid, {player_select_attack_target, Data});
				16#1c -> correct_previous_msg (NewLockMsg, [m8814], ArenaPid, {response_update_select_target});
				16#1d -> correct_previous_msg (NewLockMsg, [8815], ArenaPid, {select_attacked_ability, Data});
				16#1e -> correct_previous_msg (NewLockMsg, [8816], ArenaPid, {response_select_attacked_ability, Data});
				16#1f -> correct_previous_msg (NewLockMsg, [m881e], ArenaPid, {res_update_destroy});
				16#5d -> correct_previous_msg (NewLockMsg, [m8811], ArenaPid, {res_player_select_attack_zone, Data});
%				16#18 -> gen_server:call(ArenaPid, {response_attacker});
%				16#19 -> gen_server:call(ArenaPid, {select_attacker_ability, Data});
%				16#1a -> gen_server:call(ArenaPid, {response_update_select_ability});
%				16#1b -> gen_server:call(ArenaPid, {player_select_attack_target, Data});
%				16#1c -> gen_server:call(ArenaPid, {response_update_select_target});
%				16#1d -> gen_server:call(ArenaPid, {select_attacked_ability, Data});
%				16#1e -> gen_server:call(ArenaPid, {response_select_attacked_ability}); %% Warning : somthing may missing over here
%				16#1f -> gen_server:call(ArenaPid, {res_update_destroy});

				%% === Combination =====  Completed
				16#20 -> correct_previous_msg (NewLockMsg, [m885a], ArenaPid, {select_combine_option, Data});
				16#21 -> correct_previous_msg (NewLockMsg, [m885b], ArenaPid, {select_combine_set, Data});
				16#22 -> correct_previous_msg (NewLockMsg, [m885c], ArenaPid, {rep_update_support_seal});
				16#23 -> correct_previous_msg (NewLockMsg, [m885d], ArenaPid, {response_completed_combine});
%				16#20 -> gen_server:call(ArenaPid, {select_combine_option, Data});
%				16#21 -> gen_server:call(ArenaPid, {select_combine_set, Data});
%				16#22 -> gen_server:call(ArenaPid, {rep_update_support_seal});
%				16#23 -> gen_server:call(ArenaPid, {response_completed_combine});

				% ===== Move to Shrine =====
				16#24 -> correct_previous_msg (NewLockMsg, [m8830], ArenaPid, {response_update_shrine_level});
				%16#25 -> correct_previous_msg (NewLockMsg, [m8831], ArenaPid, {select_card_ability_affect, Data});
				16#26 -> correct_previous_msg (NewLockMsg, [m8832], ArenaPid, {response_card_ability_affect});
%				16#24 -> gen_server:call(ArenaPid, {response_update_shrine_level});
%				16#25 -> gen_server:call(ArenaPid, {select_card_ability_affect, Data});
%				16#26 -> gen_server:call(ArenaPid, {response_card_ability_affect});

				%% === Line Change ===== Completed
				16#2a -> correct_previous_msg (NewLockMsg, [fe], ArenaPid, {req_change_line, Data});  %%% [Review] Check this out, duplication to funcion 16#03
				16#2b -> check_lock_msg (NewLockMsg, Function, ArenaPid, {rec_res_changing_line});
				16#2c -> check_lock_msg (NewLockMsg, Function, ArenaPid, {rec_res_changed_line});
				16#2e -> check_lock_msg (NewLockMsg, Function, ArenaPid, {change_line_complete});
%				16#2a -> gen_server:call(ArenaPid, {req_change_line, Data}); %%% [Review] Check this out, duplication to funcion 16#03
%				16#2b -> gen_server:call(ArenaPid, {rec_res_changing_line});
%				16#2c -> gen_server:call(ArenaPid, {rec_res_changed_line});

				%% === Attack To Hand =====  Completed
				16#30 -> correct_previous_msg (NewLockMsg, [m8820], ArenaPid, {response_update_hand_attacker});
				16#31 -> correct_previous_msg (NewLockMsg, [m8821], ArenaPid, {player_select_select_hand_target, Data});
				16#32 -> correct_previous_msg (NewLockMsg, [m8822], ArenaPid, {response_update_hand_target});
%				16#30 -> gen_server:call(ArenaPid, {response_update_hand_attacker});
%				16#31 -> gen_server:call(ArenaPid, {player_select_select_hand_target, Data});
%				16#32 -> gen_server:call(ArenaPid, {response_update_hand_target});

				% -------- Player Select Attack Action ---------
				16#33 -> correct_previous_msg (NewLockMsg, [m8817], ArenaPid, {player_select_attack_action, Data});
				16#34 -> correct_previous_msg (NewLockMsg, [m8818], ArenaPid, {player_select_additional_attack, Data});
%				16#33 -> gen_server:call(ArenaPid, {player_select_attack_action, Data});
%				16#34 -> gen_server:call(ArenaPid, {player_select_additional_attack, Data});

				%% === Break Combine =====  Completed
				16#3a -> check_lock_msg (NewLockMsg, Function, ArenaPid, {res_update_break_combine});
				16#3b -> correct_previous_msg (NewLockMsg, [8861], ArenaPid, {response_update_card_to_arena});
%				16#3a -> gen_server:call(ArenaPid, {res_update_break_combine});
%				16#3b -> gen_server:call(ArenaPid, {response_update_card_to_arena});

				%% === Casting Mystic =====
				16#40 -> correct_previous_msg (NewLockMsg, [m8850], ArenaPid, {select_mystic_ability, Data});
				16#41 -> correct_previous_msg (NewLockMsg, [8851], ArenaPid, {response_update_casting_mystic});
				16#42 -> correct_previous_msg (NewLockMsg, [8852], ArenaPid, {player_select_mystic_target, Data});
				16#43 -> correct_previous_msg (NewLockMsg, [8853], ArenaPid, {response_update_select_mystic_target});
				16#44 -> correct_previous_msg (NewLockMsg, [8854], ArenaPid, {response_update_cast_mystic_success});
				16#45 -> correct_previous_msg (NewLockMsg, [m8855], ArenaPid, {player_select_mystic_effect, Data});
				16#46 -> correct_previous_msg (NewLockMsg, [m8856], ArenaPid, {response_update_select_mystic_fx});
%				16#40 -> gen_server:call(ArenaPid, {select_mystic_ability, Data});
%				16#41 -> gen_server:call(ArenaPid, {response_update_casting_mystic});
%				16#42 -> gen_server:call(ArenaPid, {player_select_mystic_target, Data});
%				16#43 -> gen_server:call(ArenaPid, {response_update_select_mystic_target});
%				16#44 -> gen_server:call(ArenaPid, {response_update_cast_mystic_success});
%				16#45 -> gen_server:call(ArenaPid, {player_select_mystic_effect, Data});
%				16#46 -> gen_server:call(ArenaPid, {response_update_select_mystic_fx});

				%% === Seal Skill =====
				16#48 -> correct_previous_msg(NewLockMsg, [m8866], ArenaPid, {response_update_card_use_skill});
				16#49 -> correct_previous_msg(NewLockMsg, [m8867], ArenaPid, {player_select_skill, Data});
				16#4a -> correct_previous_msg(NewLockMsg, [m8868], ArenaPid, {response_update_select_skill});
				16#4b -> correct_previous_msg (NewLockMsg, [m8869], ArenaPid, {response_update_player_mp});
				16#4c -> correct_previous_msg(NewLockMsg, [m886a], ArenaPid, {player_select_target_skill, Data});
				
				16#ef -> correct_previous_msg (NewLockMsg, [mxxxxx], ArenaPid, {server_select_target, Data});
				
				16#4d -> correct_previous_msg(NewLockMsg, [m886b], ArenaPid, {response_update_player_select_target_skill});
				16#7a-> correct_previous_msg (NewLockMsg, [m8890], ArenaPid, {response_finish_set_seal_use_skill});
				16#4e -> correct_previous_msg (NewLockMsg, [m886c], ArenaPid, {response_activate_select_curse, Data});
				16#6d -> correct_previous_msg (NewLockMsg, [m8862], ArenaPid, {response_activate_select_elem, Data});
				16#4f -> correct_previous_msg (NewLockMsg, [m886d], ArenaPid, {response_player_select_skill_condtion_target, Data});
				16#6e -> correct_previous_msg (NewLockMsg, [m886e], ArenaPid, {response_activate_skill_animation});
				16#6c -> correct_previous_msg(NewLockMsg, [m886d], ArenaPid, {response_select_deck, Data});
				16#68 -> correct_previous_msg(NewLockMsg, [m8841], ArenaPid, {response_player_select_skill_decision, Data});
%				16#48 -> gen_server:call(ArenaPid, {response_update_card_use_skill});
%				16#49 -> gen_server:call(ArenaPid, {player_select_skill, Data});
%				16#4a -> gen_server:call(ArenaPid, {response_update_select_skill});
%				16#4b -> gen_server:call(ArenaPid, {response_update_player_mp});
%				16#4c -> gen_server:call(ArenaPid, {player_select_target_skill, Data});
%				16#4d -> gen_server:call(ArenaPid, {response_update_player_select_target_skill});
%				16#7a -> gen_server:call(ArenaPid, {response_finish_set_seal_use_skill});
%				16#4e -> gen_server:call(ArenaPid, {response_activate_select_curse, Data});
				%16#yy -> gen_server:call(ArenaPid, {response_activate_select_elem, Data});
%				16#4f -> gen_server:call(ArenaPid, {response_player_select_skill_condtion_target, Data});
				%16#yy -> gen_server:call(ArenaPid, {response_player_select_decision, Data});
				
				%% ===== Move to Library Zone ======
				16#54 -> correct_previous_msg (NewLockMsg, [m8874], ArenaPid, {response_update_card_to_library});
				16#55 -> correct_previous_msg (NewLockMsg, [8875], ArenaPid, {response_update_deck_size});
%				16#54 -> gen_server:call(ArenaPid, {response_update_card_to_library});
%				16#55 -> gen_server:call(ArenaPid, {response_update_deck_size});

				% ===== Growth Zone =========
				16#58 -> correct_previous_msg (NewLockMsg, [m8838], ArenaPid, {response_update_select_growth, Data});
%				16#58 ->correct_previous_msg (NewLockMsg, [m8838], ArenaPid, {player_select_growth_option, Data});
				16#59 -> correct_previous_msg (NewLockMsg, [m8839], ArenaPid, {player_select_growth_set, Data});
				%16#5a -> correct_previous_msg (NewLockMsg, [m883a], ArenaPid, {response_update_growth_material});
%				16#58 -> gen_server:call(ArenaPid, {player_select_growth_option, Data});
%				16#59 -> gen_server:call(ArenaPid, {player_select_growth_set, Data});
%				16#5a -> gen_server:call(ArenaPid, {response_update_growth_material});

				% ===== Remove Zone ==========
				16#5c -> correct_previous_msg (NewLockMsg, [m883c], ArenaPid, {response_update_remove_cards});
%				16#5c -> gen_server:call(ArenaPid, {response_update_remove_cards});

				% ------------- Reveal Cards ---------------------
				16#64 -> correct_previous_msg (NewLockMsg, [m887b], ArenaPid, {response_reveal_cards});
%				16#64 -> gen_server:call(ArenaPid, {response_reveal_cards});

				%---------------Update Ability ----------------------
				16#85 -> gen_server:cast(ArenaPid, {update_ability, SPlayer, Data});
				16#86 -> gen_server:cast(ArenaPid, {update_seal_power, Data});
				
				16#a0 -> correct_previous_msg(NewLockMsg, [m88a0], ArenaPid, {response_player_select_deck, Data});
				16#a1 -> correct_previous_msg(NewLockMsg, [m88a1], ArenaPid, {response_update_player_collection});
				16#b0 -> correct_previous_msg(NewLockMsg, [m88b0], ArenaPid, {response_move_mystic_to_other_target});
				
				16#67 -> correct_previous_msg(NewLockMsg, [m8840], ArenaPid, {response_decide_change_zone, Data});
				
				16#ee -> gen_server:cast(ArenaPid, {request_card_deck_to_hand, SPlayer, Data});
				_ ->	
					gen_server:cast(SPlayer, {send, [16#83, 16#fb]}),
					io:format("<main> Msg out of bound : ~p~n", [Function])
			end;
		_ ->
			case Function of
				16#01 -> gen_server:call(ArenaPid, {reject_casting, 99, no_change_lock_msg}, infinity);

				% ------------- Activate Select Ability Target -----------------------
				16#07 -> correct_previous_msg (NewLockMsg, [m8803], ArenaPid, {player_select_activation_ability, Data});
				16#09 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {response_card_caster});
				16#0c -> correct_previous_msg (NewLockMsg, [m8808], ArenaPid, {player_select_line, Data});
				16#0d -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {response_update_select_line});
				16#0f -> correct_previous_msg (NewLockMsg, [m880c], ArenaPid, {player_select_ability_target, Data});
				16#10 -> correct_previous_msg (NewLockMsg, [m880d], ArenaPid, {response_update_ability_target});
%				16#0f -> gen_server:call(ArenaPid, {player_select_ability_target, Data});
%				16#10 -> gen_server:call(ArenaPid, {response_update_ability_target});

				% ===== Move to Library Zone ======
				16#1f -> correct_previous_msg (NewLockMsg, [m881e], ArenaPid, {res_update_destroy});
				16#24 -> correct_previous_msg (NewLockMsg, [m8830], ArenaPid, {response_update_shrine_level});
				%16#25 -> correct_previous_msg (NewLockMsg, [m8831], ArenaPid, {select_card_ability_affect, Data});
				16#26 -> correct_previous_msg (NewLockMsg, [m8832], ArenaPid, {response_card_ability_affect});
				16#54 -> correct_previous_msg (NewLockMsg, [m8874], ArenaPid, {response_update_card_to_library});
				16#55 -> correct_previous_msg (NewLockMsg, [8875], ArenaPid, {response_update_deck_size});
				16#64 -> correct_previous_msg (NewLockMsg, [m887b], ArenaPid, {response_reveal_cards});
				
				%16#5c -> correct_previous_msg (NewLockMsg, [m883c], ArenaPid, {response_update_remove_cards});
%				16#1f -> gen_server:call(ArenaPid, {res_update_destroy});
%				16#24 -> gen_server:call(ArenaPid, {response_update_shrine_level});
%				16#25 -> gen_server:call(ArenaPid, {select_card_ability_affect, Data});
%				16#26 -> gen_server:call(ArenaPid, {response_card_ability_affect});
%				16#54 -> gen_server:call(ArenaPid, {response_update_card_to_library});
%				16#55 -> gen_server:call(ArenaPid, {response_update_deck_size});
%				16#5c -> gen_server:call(ArenaPid, {response_update_remove_cards});
				%------------------------------------- จัดเรียงลำดับการเกิด Ability -----------------------------------
				16#85 -> gen_server:cast(ArenaPid, {update_ability, SPlayer, Data});
				16#86 -> gen_server:cast(ArenaPid, {update_seal_power, Data});
				16#5c -> correct_previous_msg(NewLockMsg, [m883c], ArenaPid, {response_update_remove_cards});
				16#a0 -> correct_previous_msg(NewLockMsg, [m88a0], ArenaPid, {response_player_select_deck, Data});
				16#a1 -> correct_previous_msg(NewLockMsg, [m88a1], ArenaPid, {response_update_player_collection});
				_ ->	
					gen_server:cast(SPlayer, {send, [16#83, 16#fb]}),
					io:format("Play main msg out of main step~n")
			end
	end.

msg_mp_clean_step_controller (ArenaPid, _SPlayer, Function, _, _) ->
	%{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	%smo_logger:fmsg("rec_update_mp_clean from ArenaPid ~p~n", [ArenaPid]),
	case Function of
		16#00 -> gen_server:call(ArenaPid, {req_into_mp_clean}, infinity);
		16#01 -> gen_server:call(ArenaPid, {rec_update_mp_clean}, infinity);
		_ -> io:format("<clean> Msg out of bound : ~p~n", [Function])
	end.

msg_eos_step_controller (ArenaPid, _SPlayer, Function, _, _) ->
	%{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case Function of
		16#00 -> gen_server:call(ArenaPid, {req_into_eos}, infinity);
		_ -> io:format("<eos> Msg out of bound : ~p~n", [Function])
	end.

msg_misc_controller (ArenaPid, SPlayer, Function, Data, _LockMsg) ->
	{ok, NewLockMsg} = gen_server:call(ArenaPid, {get_current_lockmsg}, infinity),
	case gen_server:call(ArenaPid, {get_game_data, game_step}, infinity) of
	%case mnesia_play:get_game_data(ArenaPid, game_step) of
		{ok, main} ->
			case Function of
				16#00 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {req_shrine_info, Data});
				16#01 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {req_hand_info});
				16#02 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {req_remove_info, Data});
				16#03 -> correct_previous_msg (NewLockMsg, [0], ArenaPid, {in_game_chat, {SPlayer, Data}});
				16#04 -> gen_server:cast(ArenaPid, {check_opponent_status, SPlayer});
				16#05 -> gen_server:cast(SPlayer, {player_response});
%				16#00 -> gen_server:call(ArenaPid, {req_shrine_info, Data});
%				16#01 -> gen_server:call(ArenaPid, {req_hand_info});
%				16#02 -> gen_server:call(ArenaPid, {req_remove_info, Data});
%				16#03 -> gen_server:call(ArenaPid, {in_game_chat, Data});
				_ -> io:format("<MISC> Msg out of bound : ~p~n", [Function])
			end;
		_ -> case Function of
				16#03 -> gen_server:call(ArenaPid, {in_game_chat, {SPlayer, Data}}, infinity);
				16#04 -> gen_server:cast(ArenaPid, {check_opponent_status, SPlayer});
				16#05 -> gen_server:cast(SPlayer, {player_response});
				_ -> io:format("Request data msg out of main step~n")
			end
	end.
		
msg_deck_edit_controller (SPlayer, Function, Data, _LockMsg) ->
	case Function of
		16#00 -> lib_deck_edit:request_all_decks(SPlayer);
		16#01 -> lib_deck_edit:request_create_deck(SPlayer, Data);
		16#02 -> lib_deck_edit:request_change_deck_name(SPlayer, Data);
		16#05 -> lib_deck_edit:request_save_deck(SPlayer, Data);
		16#06 -> lib_deck_edit:request_delete_deck(SPlayer, Data);
		16#07 -> lib_deck_edit:request_player_cards(SPlayer);
		16#08 -> lib_deck_edit:set_active_deck(SPlayer, Data);
		_ -> smo_logger:fmsg("<Deck Edit> Msg out of bound : ~w", [Function])
	end.
	
%msg_authen_connection (_, SPlayer, Function, Data, _) ->
%	case Function of
%		16#ff ->
%			case file:consult ("channel.conf") of
%				{ok, ConfigData} ->
%					[Port] = [ P || {port, P} <- ConfigData],
%					[ServerIp] = [ P || {server_ip, P} <- ConfigData];
%				{error, _} ->
%					ServerIp = "10.10.10.90",
%					Port = 9000
%			end,
%			AuthenString = play_utility:decode (ServerIp ++ integer_to_list (Port)),
%			case Data of
%				AuthenString ->
%					CCU = mnesia_table:count_data (user_data),
%					server_lib:send (SPlayer, [CCU]);
%				_ ->	io:format ("AS ~p~n", [AuthenString]),
%					io:format ("Data ~p~n", [Data]),
%					server_lib:close (SPlayer)
%			end;
%		_ ->	io:format("Authenticate Funtion ~p out of bound~n", [Function])
%	end.
