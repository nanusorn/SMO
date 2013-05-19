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
-module (interfere_step).

-import(server_lib, [send/2, controller/2]).
-import(lists, [foreach/2]).

-compile (export_all).

activate_interfere(PlayerPid, Header, InterfereTurn) ->
	case InterfereTurn of
		not_your_interfere -> 
			gen_server:cast(PlayerPid, {send, Header ++ [0]});
		your_interfere -> 
			gen_server:cast(PlayerPid, {send, Header ++ [1]})
	end.

interfere_player_def(PlayerPid, IsTurn) ->
	case IsTurn of
		1 -> activate_interfere(PlayerPid, [16#87, 16#00], not_your_interfere);
		0 -> activate_interfere(PlayerPid, [16#87, 16#00], your_interfere)
	end.

check_play_interfere(PlayerPid, OptionData, InterferePlay, Data) ->
	case OptionData of
		{PlayerPid, SkipInterfere} ->
			play_utility:out_of_turn(PlayerPid, req_use_card),
			{PlayerPid, SkipInterfere};
		{_, _} ->
			interfere_step:check_interfere_use(PlayerPid, InterferePlay, Data),
			{PlayerPid, 0}
	end.

check_skip_interfere(PlayerPid, PlayerList, OptionData) ->
	case OptionData of
		{PlayerPid, SkipInterfere} ->
			play_utility:out_of_turn (PlayerPid, skip_interfere),
			{PlayerPid, SkipInterfere};
		{_, SkipInterfere} ->
			interfere_step:player_skip_interfere (PlayerPid, PlayerList, SkipInterfere),
			{PlayerPid, SkipInterfere + 1};
		_ -> ""
	end.

switch_interfere_player (LastPlayerPid, PlayerLists, Header) ->
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					LastPlayerPid ->
						activate_interfere(LastPlayerPid, Header, not_your_interfere);
					OppoPid ->
						activate_interfere(OppoPid, Header, your_interfere)
				end
			end, PlayerLists).

check_interfere_use (_, InterferePlay, _) ->
	case InterferePlay of
		card_play -> to_do;
		skill_play -> to_do
	end.

player_skip_interfere (PlayerPid, PlayerLists, OptionData) ->
	case OptionData of
		0 ->	switch_interfere_player (PlayerPid, PlayerLists, [16#87, 16#00]);
		1 ->	play_utility:into_next_step ()
	end.

%into_sub_interfere () ->
%	{ok, PlayerPid} = mnesia_play:get_game_data(self(), player_turn),
%	{ok, {PlayId, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
%	{ok, Play} = stack_pool:get_last_stack (self(), play),
%	InterferePlayId = get_interfere_play_id (Play),
%	io:format("- ~p, card ~p, PlayID ~p~n", [Play, {PlayerPid, CardOrder, CardID}, InterferePlayId]),
%	stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, sub_interfere}, {card_play, {PlayId, CardOrder, CardID, InterferePlayId}}, {skip_player, 0}]),
%	case mnesia_play:get_game_data (self(), game_step) of
%		{ok, main} -> 
%			gen_server:cast(self(), {act_sub_interfere, sub_interfere});
%		{ok, inter1} -> 
%			gen_server:cast(self(), {act_sub_interfere, sub_interfere});
%		{ok, inter2} -> 
%			gen_server:cast(self(), {act_sub_interfere, sub_interfere});
%		_ -> return_play ()
%	end.

into_sub_interfere () ->
		{ok, PlayerPid} = mnesia_play:get_game_data(self(), player_turn),
		{ok, {PlayId, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
		{ok, Play} = stack_pool:get_last_stack (self(), play),
		InterferePlayId = get_interfere_play_id (Play),
		%io:format("- ~p, card ~p, PlayID ~p~n", [Play, {PlayerPid, CardOrder, CardID}, InterferePlayId]),
		case InterferePlayId of
			95 ->
				case card_utility:check_card_zone (PlayId, CardOrder, CardID) of
					arena_zone -> 
						stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, sub_interfere}, {card_play, {PlayId, CardOrder, CardID, InterferePlayId}}, {skip_player, 0}]),
							case mnesia_play:get_game_data (self(), game_step) of
								{ok, main} -> 
									gen_server:cast(self(), {act_sub_interfere, sub_interfere});
								{ok, inter1} -> 
									gen_server:cast(self(), {act_sub_interfere, sub_interfere});
								{ok, inter2} -> 
									gen_server:cast(self(), {act_sub_interfere, sub_interfere});
								_ -> return_play ()
							end;
					_ ->
					stack_pool:pop_stack_out (self()),
						case stack_pool:get_last_stack (self(), play) of
							{ok, StackPlay} ->
								interfere_step:return_play(StackPlay);
							{error, _} ->
								gen_server:cast(self(), {act_next_command})
						end
				end;
			_ ->	
			stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, sub_interfere}, {card_play, {PlayId, CardOrder, CardID, InterferePlayId}}, {skip_player, 0}]),
			case mnesia_play:get_game_data (self(), game_step) of
				{ok, main} -> 
					gen_server:cast(self(), {act_sub_interfere, sub_interfere});
				{ok, inter1} -> 
					gen_server:cast(self(), {act_sub_interfere, sub_interfere});
				{ok, inter2} -> 
					gen_server:cast(self(), {act_sub_interfere, sub_interfere});
					_ -> return_play()
			end
	end.

%play_interfere_step (PlayerList, NewPlayPid) ->
%	stack_pool:set_last_stack (self(), player_pid, NewPlayPid),
%	foreach (	fun ({PlayerPid, _}) ->
%				case PlayerPid of
%					NewPlayPid -> 
%						gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 1]});
%					_ -> 
%						gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 0]}) 
%				end
%			end, PlayerList)
	
	
play_interfere_step (PlayerList, NewPlayPid) ->
	stack_pool:set_last_stack (self(), player_pid, NewPlayPid),
	case stack_pool:get_last_stack (self(), option) of
		{ok, Option} ->
			case is_atk_type (Option) of
				{atk_all} ->
					put(a, {play_in}),
					foreach (	fun ({PlayerPid, _}) ->
						case PlayerPid of
							NewPlayPid -> 
								gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 1]});
							_ -> 
								gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 0]}) 
						end
					end, PlayerList);
				_ ->
					put(b, {play_in}),
				foreach (	fun ({PlayerPid, _}) ->
					case PlayerPid of
						NewPlayPid -> 
							gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 1]});
						_ -> 
							gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 0]}) 
					end
				end, PlayerList)
			end;
		_ ->
			put(b, {play_in}),
				foreach (	fun ({PlayerPid, _}) ->
					case PlayerPid of
						NewPlayPid -> 
							gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 1]});
						_ -> 
							gen_server:cast(PlayerPid, {send, [16#88, 16#1d, 0]}) 
					end
				end, PlayerList)
	end.

is_atk_type([{_, {_, _, _, 2}}|_]) -> {atk_all};
is_atk_type([_|T]) -> is_atk_type (T);
is_atk_type([]) -> {no_atk_all}.


check_skip_sub_interfere (PlayerPid, PlayerList, OptionData) ->
	case is_pid (PlayerPid) of
		true ->
			case OptionData of
				{ResumePlay, PlayerPid, SkipInterfere} ->
					play_utility:out_of_turn (PlayerPid, skip_sub_interfere),
					{error, {ResumePlay, PlayerPid, SkipInterfere}};
				{ResumePlay, _, SkipInterfere} ->
					interfere_step:player_skip_sub_interfere (PlayerPid, PlayerList, SkipInterfere, ResumePlay),
					{ok, {ResumePlay, PlayerPid, SkipInterfere + 1}}
			end;
		false -> {error, OptionData}
	end.

check_cancel_sub_interfere (PlayerPid, PlayerList, OptionData) ->
	put(a, {no_play_in}),
	put(b, {no_play_in}),
	case is_pid (PlayerPid) of
		true ->
			case stack_pool:get_last_stack (self(), play) of
				{ok, sub_interfere} ->
					cancel_sub_interfere (PlayerPid, PlayerList, OptionData);
				_ ->	OptionData
			end;
		false -> OptionData
	end.

cancel_sub_interfere (PlayerPid, PlayerList, OptionData) ->
	case OptionData of
		{ResumePlay, PlayerPid, SkipInterfere} ->
			interfere_step:player_skip_sub_interfere (PlayerPid, PlayerList, SkipInterfere, ResumePlay),
			{ok, {ResumePlay, PlayerPid, SkipInterfere + 1}};
		{_, _, _} ->
			play_utility:out_of_turn (PlayerPid, skip_sub_interfere),
			{error, OptionData}
	end.

switch_sub_interfere_player (LastPlayerPid, PlayerLists) ->
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					LastPlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 0]});
					_ ->	{ok, {PlayId, CardOrder, CardID, InterferePlayId}} = stack_pool:get_last_stack (self(), card_play),
						case PlayerPid of
							PlayId -> 
								gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 1, 1, CardOrder, <<CardID:16>>, InterferePlayId]});
							_ -> 
								gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 1, 0, CardOrder, <<CardID:16>>, InterferePlayId]})
						end
				end
			end, PlayerLists).

player_skip_sub_interfere (PlayerPid, PlayerLists, OptionData, LastPlayStep) ->
	case OptionData of
		0 ->	switch_sub_interfere_player (PlayerPid, PlayerLists);
		1 ->	io:format("<< Return from Interfere to ~p~n", [LastPlayStep]),
			return_play ()
	end.

sub_interfere_player_def (PlayerPid, IsPlayTurn) ->
	case IsPlayTurn of
		1 ->	
			gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 0]});
		0 ->	{ok, {PlayId, CardOrder, CardID, InterferePlayId}} = stack_pool:get_last_stack (self(), card_play),
			case PlayerPid of
				PlayId ->
					gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 1, 1, CardOrder, <<CardID:16>>, InterferePlayId]});
				_ ->	
					gen_server:cast(PlayerPid, {send, [16#88, 16#0e, 1, 0, CardOrder, <<CardID:16>>, InterferePlayId]})
			end
	end.

return_play () ->
	stack_pool:pop_stack_out(self()),
	case stack_pool:get_last_stack (self(), play) of
		{ok, PlayResume}  ->
			interfere_step:return_play (PlayResume);
		_ ->	
			no_stack_return
			%gen_server:cast(self(), {act_next_command})
	end.	

return_play (LastPlayStep) ->
	case LastPlayStep of
		sub_interfere ->
			stack_pool:pop_stack_out(self()),
			 case stack_pool:get_last_stack (self(), play) of %เช็ค message ตี all ให้โชว์ message
			 		{ok, check_interfere_atk_all} -> interfere_step:return_play (check_interfere_atk_all);
			 		_ -> smo_logger:msg("error case cannot operate"),
						into_sub_interfere()
			 end;
		check_play_step ->
			case stack_pool:get_last_stack (self(), play) of
				{ok, PlayResume}  -> interfere_step:return_play (PlayResume);
				_ -> %stack_empty
					gen_server:cast(self(), {act_next_command})
			end;
		_ ->	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
			return_play (LastPlayStep, PlayerList)
	end.

return_play (LastPlayStep, PlayerList) ->
	%smo_logger:fmsg("---- Return stack to ~p~n", [LastPlayStep]),
	%io:format("---- Return stack to ~p~n", [LastPlayStep]),
	case stack_pool:get_last_stack (self()) of
		{ok, {PlayerPid, CardOrder, CardID, _}} ->
			case LastPlayStep of
				player_discard -> discard_step:check_to_discard (PlayerPid);
				
				% --- Continuous Ability 
				%continuous_ability_to_card -> continuous_ability:check_effect_to_card();
				remove_continuous_effect_from_target -> continuous_ability:remove_continuous_ability();
				
				affect_card_ability_effect -> continuous_ability:add_current_effect();
				return_to_check_effect_to_card -> continuous_ability:check_effect_to_card();
				%to_add_effect_to_target -> continuous_ability:add_effect_to_target();

				% --- Casting cards
				%check_come_into_play -> casting_card:casting_seal_card(PlayerPid, CardOrder, CardID);
				casting_card_to_arena -> set_growth:check_growth_ability ();
				casting_seal_select_line -> casting_card:interfere_casting_step ();
				casting_seal -> casting_card:check_zone_post_interfere(PlayerPid, CardOrder, CardID);%cast_to_arena_10 (PlayerPid, CardOrder, CardID);
				
				check_on_arena_success -> casting_card:check_cast_success_ability();
				check_cast_success_ability -> casting_card:verify_into_arena_ability();
				verify_into_arena_ability -> casting_card:verify_on_arena_ability();
				verify_on_arena_ability -> casting_card:verify_cast_success_ability();
				verify_cast_success_ability -> casting_card:cast_to_arena_10_growth_activate(PlayerPid, CardOrder, CardID);
				send_update_growth -> casting_card:send_update_growth(PlayerPid, CardOrder, CardID);
				growth_activate -> casting_card:check_growth_success_ability();
				check_growth_success_ability -> casting_card:verify_growth_success_ability();
				verify_growth_success_ability -> casting_card:activate_growth_into_arena();
				activate_growth_into_arena -> casting_card:activate_into_arena_effect();  
				activate_into_arena_effect -> casting_card:activate_on_arena_effect();
				activate_on_arena_effect -> casting_card:activate_cast_success_effect();
				activate_cast_success_effect -> casting_card:activate_seal_on_arena_effect();
				activate_seal_on_arena_effect ->	casting_card:cast_to_arena_11(PlayerPid, CardOrder, CardID);
				cast_successful -> casting_card:cast_successful(PlayerPid, CardOrder, CardID);
				
				%play_cast_to_arena_10 -> casting_card:cast_to_arena_11 (PlayerPid, CardOrder, CardID);
				casting_activate_ability -> casting_card:casting_activated_ability (PlayerPid);
				update_ability_check -> casting_card:update_ability_check ();
				success_cast -> casting_card:cast_successful (PlayerPid, CardOrder, CardID);
				check_into_arena_ability -> casting_card:activate_select_line(PlayerPid);
				
				%check_growth_ability -> casting_card:check_ability_select_target (PlayerPid, CardOrder, CardID);
				check_growth_ability -> casting_card:check_into_arena_ability();
				%check_growth_ability -> casting_card:cast_seal_change_zone();
				%cast_seal_change_zone -> casting_card:check_into_arena_ability();
				check_growth_activate -> casting_card:check_ability_affect(PlayerPid, CardOrder, CardID);
								
				casting_mystic -> casting_card:check_mystic_casting_condition(PlayerPid, CardOrder, CardID);
				update_mystic_casting_data -> casting_card:select_mystic_target(PlayerPid, CardOrder, CardID);
				select_mystic_target -> casting_card:interfere_casting_mystic_card();
				select_then_mystic_target -> casting_card:verify_mystic_then_condition(PlayerPid, CardOrder, CardID);
				verify_cast_mystic_target -> casting_card:casting_mystic_resume2(PlayerPid, CardOrder, CardID);
				
				%casting_mystic_card_5 -> casting_card:activate_select_mystic_target (PlayerPid, CardOrder, CardID);
				%casting_mystic_card_6 -> mystic_effect:check_ability_select_target (PlayerPid);
				%play_mystic_target_done -> casting_card:interfere_casting_mystic_card ();
				interfere_casting_mystic -> casting_card:verify_mystic_casting_condition(PlayerPid, CardOrder, CardID);
				%play_casting_card_10 -> casting_card:casting_mystic_resume2 (PlayerPid, CardOrder, CardID);
				%casting_mystic_card_11 -> casting_card:activate_mystic_to_target (PlayerPid, CardOrder, CardID);
				
				%casting_mystic_card_11 -> casting_card:mystic_change_zone();
				casting_mystic_card_to_target -> casting_card:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				%play_activate_mystic_change_zone -> casting_card:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				play_activate_mystic_casting_effect -> mystic_effect:check_then_abiltity_id(PlayerPid, CardOrder, CardID);
				move_cancel_mystic_to_shrine -> casting_card:check_send_fx_and_duration_mystic(PlayerPid, CardOrder, CardID);
				activate_mystic_to_player -> mystic_effect:check_continuous_effect();
				activate_mystic_to_card -> mystic_effect:check_continuous_effect();
				mystic_card_check_continuous_effect -> mystic_effect:activate_mystic_to_card(PlayerPid, CardOrder, CardID);
				mystic_player_check_continuous_effect -> mystic_effect:activate_mystic_to_player(PlayerPid, CardOrder, CardID);
				
				
				cast_mystic_complete -> casting_card:check_send_fx_and_duration_mystic (PlayerPid, CardOrder, CardID);
				play_check_send_fx_and_duration_mystic -> casting_card:activate_check_next_command ();
				%check_next_command -> casting_card:check_next ();
				play_activate_check_next_command -> casting_card:next_command_to_client();

				play_activate_ability_mystic -> mystic_effect:check_mystic_ability_list (PlayerPid);
				play_activate_mystic_effect -> mystic_card:add_effect_to_target ();

				%play_activate_effect_to_player -> mystic_card:verify_effect_mystic ();
				
				% -----move mytic--------------
				active_remove_mystic_effect -> other_active_effect:move_mystic_to_other_target(PlayerPid, CardOrder, CardID);
				%Move Mystic to Other Target
				cannot_move_mystic_then_to_shrine -> other_active_effect:move_remain_to_arena();
				mystic_to_other_select_target -> other_active_effect:verify_move_mystic_to_arena(PlayerPid, CardOrder, CardID);
				verify_mystic_to_other_target -> other_active_effect:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				mystic_to_other_activate_mystic_effect -> other_active_effect:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				move_mystic_to_other_target -> other_active_effect:move_remain_to_arena();
				
				% -- Growth --
				assign_growth -> growth:check_growth_option (PlayerPid, CardOrder, CardID);
				activate_select_growth_option -> growth:growth_cost (PlayerPid, CardOrder, CardID);
				growth_ability_activate -> set_growth:check_ability_select_target (PlayerPid, CardOrder, CardID);
				activate_growth_ability_select_target -> growth:interfere_growth ();
				growth_interfere -> growth:check_growth_ability_target (PlayerPid, CardOrder, CardID);
				growth_ability_target_verify -> growth:check_growth_condition (PlayerPid, CardOrder, CardID);
				growth_condition_verify -> growth:remove_growth_condition (PlayerPid);
				growth_condition_removed -> set_growth:growth_completed (PlayerPid, CardOrder, CardID);

				% -------- Assign Attack ----------------
				%assign_attack_chk -> assign_atk:assign_seal_attack (PlayerPid, CardOrder, CardID);
				player_select_attack_action -> assign_atk:normal_check_attack_action (PlayerPid, CardOrder, CardID);
				
				new_player_select_attack_action -> new_assign_atk:check_attack_type(PlayerPid, CardOrder, CardID);
				new_assign_attack_ability_activate -> new_assign_atk:verify_ability_assign_attack();
				new_verify_assign_attack_ability_condition -> new_assign_atk:activate_ability_assign_attack();
				new_activate_assign_attack_ability_effect -> new_assign_atk:check_zone_attack(PlayerPid, CardOrder, CardID);
				
				new_assign_hand_attack_ability_activate -> hand_atk:verify_ability_assign_attack();
				new_verify_assign_hand_attack_ability_condition -> hand_atk:activate_ability_assign_attack();
				new_activate_assign_hand_attack_ability_effect -> hand_atk:ability_select_when_attacker();
				new_verify_ability_when_hand_attacker -> hand_atk:verify_ability_when_hand_attacker(PlayerPid, CardOrder, CardID);
				new_hand_attacker_ability_activate -> new_assign_atk:attacking_interfere_step();
				new_verify_seal_hand_attacker_condition -> hand_atk:activate_ability_when_hand_attacker();
				new_activate_seal_hand_attacker_effect -> new_assign_atk:collect_activate_ability(PlayerPid, CardOrder, CardID);
				new_activate_hand_target -> hand_atk:activate_hand_target(PlayerList);
				
				new_attacker_ability_activate -> new_assign_atk:ability_select_when_attack_target();
				new_attacked_ability_activate -> new_assign_atk:ability_select_when_attacker_fight();
				new_attacker_fight_ability_activate -> new_assign_atk:ability_select_when_target_fight();
				new_attacked_fight_ability_activate -> new_assign_atk:attacking_interfere_step();
				%recheck_attacker_condition -> new_assign_atk:verify_ability_when_attacker();
				recheck_attacker_condition -> new_assign_atk:pre_ability_active();
				new_verify_seal_attacker_condition -> new_assign_atk:activate_ability_when_attacker();
				new_activate_seal_attacker_effect -> new_assign_atk:activate_attacker_suspend_effect(PlayerPid, CardOrder, CardID);
				activate_suspend_attacker_effect -> new_assign_atk:verify_ability_when_target();
				
				activate_no_trigger_effect -> new_assign_atk:effect_activate();
				
				%new_activate_seal_hand_attacker_effect -> hand_atk:hand_attack_10(PlayerPid, CardOrder, CardID);
				
				
				new_verify_seal_attacked_condition -> new_assign_atk:activate_ability_when_target();
				new_activate_seal_attacked_effect -> new_assign_atk:activate_target_suspend_effect();
				activate_suspend_target_effect -> new_assign_atk:collect_activate_ability(PlayerPid, CardOrder, CardID);
				%recheck_attacker_condition_again -> new_assign_atk:verify_ability_when_attacker_fight();
				recheck_attacker_condition_again -> new_assign_atk:pre_fighting_ability_active(PlayerPid, CardOrder, CardID);
				
				new_verify_attacker_fight_condition -> new_assign_atk:activate_ability_when_attacker_fight();
				new_activate_attaker_fight_effect -> new_assign_atk:verify_ability_when_target_fight();
				new_verify_attacked_fight_condition -> new_assign_atk:activate_ability_when_target_fight();
				new_activate_attaked_fight_effect -> new_assign_atk:start_combat(PlayerPid, CardOrder, CardID);
				new_activate_attack_success -> new_assign_atk:ability_select_object_success();
				new_activate_been_attack_success -> new_assign_atk:ability_select_end_of_fighting();
				new_activate_end_of_fighting -> new_assign_atk:verify_ability_when_attack_success();
				new_verify_attack_success_ability -> new_assign_atk:activate_ability_when_attack_success();
				new_activate_attack_success_effect -> new_assign_atk:verify_ability_when_object_success();
				new_verify_been_attack_success_ability -> new_assign_atk:activate_ability_when_object_success();
				new_activate_been_attack_success_effect -> new_assign_atk:verify_ability_when_end_of_fighting();
				new_verify_end_of_fighting_ability -> new_assign_atk:activate_ability_when_end_of_fighting();
				new_activate_end_of_fighting_effect -> new_assign_atk:looser_seal_destroy(PlayerPid, CardOrder, CardID);
				new_normal_atk_destroyed -> new_assign_atk:delete_each_attack_round_data(PlayerPid, CardOrder, CardID);
				
				% ---- Attack to Support Seal -----
				attack_to_support_play_break_seal -> destroy_support_seal:each_support_destroy();
				support_seal_destroy -> destroy_support_seal:support_seal_destroyed();
				
				% --- Destroyed -----
				check_destroyed_ability -> destroy:verify_destroy_ability();
				verify_destroyed_ability -> destroy:activate_destroy_effect();
				activate_destroyed_effect -> destroy:destroy_to_shrine();
				
				% -- Attack normal zone --
				normal_attack_check_target -> assign_atk:activate_select_target ();
				
				select_target_ability_activate -> assign_atk:check_ability_attacked();%assign_atk:check_ability_affect(PlayerPid, CardOrder, CardID);
				attacker_ability_activate -> assign_atk:check_ability_attacked();
				
				attacked_ability_activate -> assign_atk:check_select_target_case1();
				attacker_fight_ability_activate -> assign_atk:check_ability_attacked_fight();
				
				attacked_fight_ability_activate -> assign_atk:attacked_ability_selected();
				
				ability_effect_affect -> mod_ability_effect:check_ability_effect_affect();
				%affect_ability_effect -> mod_ability_effect:affect_current_effect();
				
				check_attacker_all_ability_activate -> attack_all:check_attacked_all_ability_activate(PlayerPid);
				check_attacked_all_ability_activate -> attack_all:attack_all_interfere();
				
				check_interfere_atk_all -> attack_all:check_atk_all_interfere(PlayerPid, CardOrder, CardID);
				check_normal -> attack_all:normal_check(PlayerPid, CardOrder, CardID);
				
				%card_attack_all_interfere -> attack_all:verify_attacker_all_ability();
				card_attack_all_interfere -> attack_all:check_last(PlayerPid, CardOrder, CardID);
				verify_attacker_all_ability -> attack_all:verify_attacked_all_ability();
				verify_attacked_all_ability -> attack_all:activate_attacker_all_effect();
				activate_attacker_all_effect -> attack_all:activate_attacker_all_suspend_effect(PlayerPid, CardOrder, CardID);
				activate_suspend_attacker_all_effect -> attack_all:activate_attacked_all_effect();
				activate_attacked_all_effect -> attack_all:activate_target_suspend_effect();
				activate_suspend_target_all_effect -> attack_all:activate_battle(PlayerPid, CardOrder, CardID);%attack_all_9(PlayerPid, CardOrder, CardID);
				
				%attacked_ability_activate -> assign_atk:assign_attack_9();%assign_atk:attacked_ability_selected ();
				%normal_atk -> assign_atk:check_atk_condition(PlayerPid, CardOrder, CardID);
				
				%normal_atk -> assign_atk:check_last_assign(PlayerPid, CardOrder, CardID);
				normal_atk -> new_assign_atk:check_last_assign(PlayerPid, CardOrder, CardID);
				
				hand_atk_normal -> new_assign_atk:check_last_assign(PlayerPid, CardOrder, CardID);
				%normal_atk -> assign_atk:assign_attack_9();
				
				verify_seal_attacker_condition -> assign_atk:seal_attaker_effect();
				seal_attacker_effect -> assign_atk:assign_attack_9_1();
				verify_seal_attacked_condition -> assign_atk:seal_attaked_effect();
				%seal_attacked_effect -> assign_atk:check_atk_condition (PlayerPid, CardOrder, CardID);
				seal_attacked_effect -> assign_atk:assign_atk_11 ();
				play_check_attacker_condition -> assign_atk:assign_atk_10_1 (PlayerPid, CardOrder, CardID);
				%play_assign_atk_10_1 -> assign_atk:assign_atk_11 ();
				
				%play_assign_atk_10_1 -> assign_atk:a();
				%a -> assign_atk:assign_attack_9();
				
				%play_assign_atk_10_1 -> assign_atk:assign_attack_9();
				play_assign_atk_10_1 -> assign_atk:check_pre_ability_activate(PlayerPid, CardOrder, CardID);
				
				play_assign_atk_11 -> assign_atk:verify_attacker_fight_condition();
				attacker_fight -> assign_atk:check_ability_attacked_fight();
				
				verify_attacker_fight_condition -> assign_atk:activate_attaker_fight_effect();
				activate_attaker_fight_effect -> assign_atk:verify_attacked_fight_condition();
				verify_attacked_fight_condition -> assign_atk:activate_attaked_fight_effect();
				%activate_attaked_fight_effect  -> assign_atk:check_ability_attack_success(PlayerPid, CardOrder, CardID);
				activate_attaked_fight_effect  -> assign_atk:check_post_ability_activate(PlayerPid, CardOrder, CardID);
				
				%recheck_attacker_condition -> assign_atk:check_ability_attack_success(PlayerPid, CardOrder, CardID);
				
				check_attack_success_ability -> assign_atk:verify_attack_success_ability();
				verify_attack_success_ability -> assign_atk:activate_attack_success_effect();
				activate_attack_success_effect -> assign_atk:activated_attack_success (PlayerPid, CardOrder, CardID);
				normal_atk_destroyed -> assign_atk:check_end_assign_attack_condition (PlayerPid, CardOrder, CardID);

				% --- Attack All ---
				attack_all_check_target -> attack_all:update_target_status ();
				%attack_all_check_target -> attack_all:check_atk_target_1 ();
				%attack_all_check_target_last -> attack_all:update_target_status ();
				attacker_all_ability_activate -> attack_all:check_ability_attacked ();
				all_attacked_ability_activate -> attack_all:attacked_ability_selected();
				card_attack_all -> attack_all:attack_all_8 ();
				play_attack_all_8 -> attack_all:attack_all_9 (PlayerPid, CardOrder, CardID);
				play_attack_all_9 -> attack_all:activate_battle ();
				play_attack_all_10 -> attack_all:pre_attack_all_ability_active(PlayerPid, CardOrder, CardID);
				new_activate_attack_all_success -> attack_all:ability_select_object_success();
				new_activate_been_attack_all_success -> attack_all:verify_ability_when_attack_success();
				new_verify_attack_success_all_ability -> attack_all:activate_ability_when_attack_success();
				new_activate_attack_success_all_effect -> attack_all:verify_ability_when_object_success();
				new_verify_been_attack_success_all_ability -> attack_all:activate_ability_when_object_success();
				new_activate_been_attack_success_all_effect -> attack_all:destroy_looser_seal(PlayerPid, CardOrder, CardID);
				 
				play_attack_all_11 -> attack_all:attack_all_12 ();
				play_attack_all_12 -> attack_all:attack_all_13 (PlayerPid, CardOrder, CardID);
				play_attack_all_13 -> attack_all:attack_all_14 (PlayerPid, CardOrder, CardID);
				attack_all_destroyed -> attack_all:delete_each_attack_round_data(PlayerPid, CardOrder, CardID);

				% Move to library
				
				from_library_to_oher_zone -> change_zone:check_zone_to_move();
				
				move_cards -> move_to_library:check_card_move();
				cards_to_library -> move_to_library:update_card_to_library();
				update_cards_move_to_library -> move_to_library:check_into_library_ability(PlayerPid);
				check_into_library_ability -> move_to_library:check_remove_from_arena_ability(PlayerPid);
				check_remove_from_arena_to_lib_ability -> move_to_library:continue_move_to_library();
				move_library_interfere -> move_to_library:verify_into_library_ability();
				verify_into_library_ability -> move_to_library:verify_remove_from_arena_ability();
				verify_remove_from_arena_to_lib_ability -> move_to_library:activate_into_library_effect();
				activate_into_library_effect -> move_to_library:activate_remove_from_arena_effect();
				activate_remove_from_arena_to_lib_effect -> move_to_library:remove_from_zone_card_status();
				remove_from_zone_to_lib_card_status -> move_to_library:move_to_library_resume();
				move_library_interfere2 -> move_to_library:move_card_to_deck();
				
				

				destroyed -> shrine_zone:check_card_destroyed ();
				hand_atk -> hand_atk:hand_attack_9 (PlayerPid, CardOrder, CardID);
				play_hand_attack_9 -> hand_atk:hand_attack_10(PlayerPid, CardOrder, CardID);
				%play_hand_attack_10 -> hand_atk:activate_hand_target (PlayerList);
				% play_hand_attack_10 -> hand_atk:verify_attacker_fight_condition();			
				% verify_hand_attacker_fight_condition -> hand_atk:activate_attaker_fight_effect();
				% activate_hand_attaker_fight_effect -> hand_atk:activate_hand_target(PlayerList);
				play_hand_attack_10 -> hand_atk:activate_hand_target(PlayerList);
				attack_hand_to_shrine -> hand_atk:updated_hand_to_shrine(PlayerList);
				new_activate_hand_attack_success -> hand_atk:verify_ability_select_hand_attack_success();
				new_verify_hand_hand_attack_success -> hand_atk:activate_ability_select_hand_attack_success();
				new_activate_hand_attack_success_effect -> hand_atk:ability_select_hand_attack_success2(PlayerPid, CardOrder, CardID);
				new_activate_hand_attack_success2 -> hand_atk:verify_ability_select_hand_attack_success2();
				new_verify_hand_hand_attack_success2 -> hand_atk:activate_ability_select_hand_attack_success2(PlayerPid, CardOrder, CardID);
				new_activate_hand_attack_success_effect2 -> new_assign_atk:delete_each_attack_round_data(PlayerPid, CardOrder, CardID);
				% Change Line
				get_into_change_line_sub_interfere -> line_change:resume_changing_line (PlayerPid, CardOrder, CardID);
				%play_change_line_5 -> line_change:change_line_5_1 (PlayerPid);
				%play_change_line_5_1 -> line_change:change_line_6 (PlayerPid, CardOrder, CardID);
				
				play_change_line_5 -> line_change:changing_line_ability_verify();
				changing_line_ability_verify -> line_change:change_line_ability_verify();
				change_line_ability_verify -> line_change:change_line_6(PlayerPid, CardOrder, CardID);
				success_assign_line -> line_change:assign_line_9();
				play_assign_line_9 -> line_change:assign_line_9_1(PlayerPid, CardOrder, CardID);
				update_seal_changing_line -> line_change:check_changing_line_ability();
				check_changing_line_ability -> line_change:check_change_line_ability();
				check_change_line_ability -> line_change:into_sub_interfere();
				%check_change_line_ability -> line_change:verify_change_line_ability();
				
				
				verify_change_line_ability -> line_change:activate_changing_line_effect();
				update_changed_line -> line_change:activate_changing_line_effect();
				activate_changing_line_effect -> line_change:activate_change_line_effect();
				activate_change_line_effect -> line_change:activate_as_on_line_effect();
				activate_as_on_line_effect -> line_change:assign_line_9_1(PlayerPid, CardOrder, CardID);
				
				
				% Move to Arena
				move_each_card_to_arena -> move_to_arena:move_each_card_to_arena();
				% Move Mystic to Arena
				move_mystic_card_to_arena -> move_to_arena:check_mystic_move_condition(PlayerPid, CardOrder, CardID);
				%move_mystic_select_number -> move_to_arena:check_mystic_move_condition(PlayerPid, CardOrder, CardID);
				move_mystic_to_arena_update-> move_to_arena:check_move_mystic_to_arena_ability();
				check_move_mystic_into_arena_ability -> move_to_arena:check_move_mystic_on_arena_ability();
				check_move_mystic_on_arena_ability -> move_to_arena:select_mystic_target(PlayerPid, CardOrder, CardID);
				move_to_arena_select_mystic_target -> move_to_arena:verify_move_mystic_into_arena_ability();
				verify_move_mystic_into_arena_ability -> move_to_arena:verify_move_mystic_to_arena_ability();
				verify_move_mystic_on_arena_ability -> move_to_arena:activate_move_mystic_into_arena_effect();
				activate_move_mystic_into_arena_effect -> move_to_arena:activate_move_mystic_on_arena_effect();
				activate_move_mystic_on_arena_effect -> move_to_arena:verify_move_mystic_to_arena(PlayerPid, CardOrder, CardID);
				%verify_move_mustic_to_arena -> move_to_arena:update_move_mystic_arena(PlayerPid, CardOrder, CardID);
				verify_move_mytic_arena_target -> move_to_arena:update_move_mystic_arena(PlayerPid, CardOrder, CardID);
				update_move_mystic_to_arena -> move_to_arena:mystic_change_zone();
				play_activate_mystic_change_zone -> move_to_arena:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				move_mystic_arena_activate_mystic_effect -> move_to_arena:activate_mystic_to_target(PlayerPid, CardOrder, CardID);
				move_cancel_move_mystic_to_shrine -> move_to_arena:check_send_fx_and_duration_mystic(PlayerPid, CardOrder, CardID);
				move_mystic_arena_zero_turn -> move_to_arena:next_command_to_client();
				
				% Move Seal to Arena
				play_move_seal_to_arena -> move_to_arena:check_move_seal_to_arena_ability();
				check_move_seal_into_arena_ability -> move_to_arena:check_move_seal_on_arena_ability();
				check_move_seal_on_arena_ability -> move_to_arena:check_seal_select_line();
				play_move_seal_select_line -> move_to_arena:verify_move_seal_into_arena_ability();
				verify_move_seal_into_arena_ability -> move_to_arena:verify_move_seal_on_arena_ability();
				verify_move_seal_on_arena_ability -> move_to_arena:activate_move_into_arena_effect();
				activate_move_seal_into_arena_effect -> move_to_arena:activate_move_seal_on_arena_effect();
				activate_move_seal_on_arena_effect -> move_to_arena:move_seal_to_arena_success(PlayerPid, CardOrder, CardID);
				activate_move_seal_to_arena_continuous -> move_to_arena:move_seal_to_arena_complete();
				
				
				
				
				% check_move_into_arena_ability -> move_to_arena:check_move_to_arena_ability(PlayerPid);
				% check_move_to_arena_ability -> move_to_arena:move_card_to_arena_6(CardID);
				% verify_move_into_arena_ability -> move_to_arena:verify_move_to_arena_ability();
				% verify_move_to_arena_ability -> move_to_arena:move_card_to_arena_8(PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena -> move_to_arena:move_card_to_arena_3 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_3 -> move_to_arena:move_card_to_arena_4 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_4 -> move_to_arena:move_card_to_arena_5 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_5 -> move_to_arena:move_card_to_arena_6 (CardID);
				% play_move_card_to_arena_6 -> move_to_arena:move_card_to_arena_7 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_7 -> move_to_arena:move_card_to_arena_8 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_8 -> move_to_arena:activate_move_into_arena_effect();
				% activate_move_into_arena_effect -> move_to_arena:activate_move_to_arena_effect();
				% activate_move_to_arena_effect -> move_to_arena:move_card_to_arena_9 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_9 -> move_to_arena:move_card_to_arena_9_2_2 ();
				% %play_move_card_to_arena_9_2_1 -> move_to_arena:move_card_to_arena_9_2_2 ();
				% play_move_card_to_arena_9_2_2 -> move_to_arena:move_card_to_arena_9_3 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_9_3 -> move_to_arena:move_card_to_arena_9_3_1 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_9_3_1 -> move_to_arena:move_card_to_arena_10 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_10 -> move_to_arena:move_card_to_arena_10_1 (PlayerPid, CardOrder, CardID);
				% play_move_card_to_arena_10_1 -> move_to_arena:move_card_to_arena_10_2 ();

				% Combination
				%being_combine -> combination:check_combination_ability ();
				%check_ability_combine_activate -> combination:check_main_seal_condition (PlayerPid, CardOrder, CardID);
				
				%play_check_complete_combination -> combination:set_complete_combine ();
				%check_update_ability_combine_success -> combination:update_ability_check (PlayerPid, CardOrder, CardID);
				update_support_seal -> combination:check_being_combine_ability();
				check_being_combine_ability -> combination:check_combine_success_ability();
				check_combine_success_ability -> combination:into_sub_interfere();
				get_into_combination_sub_interfere -> combination:being_combine_ability_verify();
				being_combine_ability_verify -> combination:combine_success_ability_verify();
				combine_success_ability_verify -> combination:check_main_seal_condition(PlayerPid, CardOrder, CardID);
				check_combine_main -> combination:check_support_seal_condition (PlayerPid, CardOrder, CardID);
				check_combine_support -> combination:check_complete_combination (PlayerPid, CardOrder, CardID);
				
				play_check_complete_combination -> combination:activate_being_combine_effect();
				activate_being_combine_effect -> combination:activate_combine_success_effect();
				activate_combine_success_effect -> combination:update_ability_check(PlayerPid, CardOrder, CardID);
				
				play_update_ability_check -> combination:return_play_from_combination (PlayerPid, CardOrder, CardID);
				
				% Break Combine
				breaking_combine -> break_combine:check_break_combine_ability();
				check_break_combine_ability -> break_combine:check_break_combine_success_ability();
				%check_break_combine_success_ability -> break_combine:update_break_combine(PlayerPid, CardOrder, CardID);
				check_break_combine_success_ability -> break_combine:into_sub_interfere();
				%update_break_combine -> break_combine:into_sub_interfere();
				get_into_break_combina_sub_interfere -> break_combine:break_combine_ability_verify();
				break_combine_ability_verify -> break_combine:break_combine_success_ability_verify();
				break_combine_success_ability_verify -> break_combine:break_combine_resume(PlayerPid, CardOrder, CardID);
				check_break_combine_seal -> break_combine:separate_seal(PlayerPid, CardOrder, CardID);
				separate_seal -> break_combine:activate_break_combine_effect();
				activate_break_combine_effect -> break_combine:activate_break_combine_success_effect();
				activate_break_combine_success_effect -> break_combine:set_break_combine_complete();
				play_set_break_combine_complete -> break_combine:break_combine_success(PlayerPid, CardOrder, CardID);
				play_break_combine_success -> break_combine:return_play_from_break_combine ();
				
				force_breaking_combine -> force_break:check_break_combine_ability();
				check_break_force_combine_ability -> force_break:check_break_combine_success_ability();
				%check_force_break_combine_success_ability -> force_break:update_break_combine(PlayerPid, CardOrder, CardID);
				check_force_break_combine_success_ability -> force_break:separate_seal(PlayerPid, CardOrder, CardID);
				update_force_break_combine -> force_break:separate_seal(PlayerPid, CardOrder, CardID);
				force_separate_seal -> force_break:activate_break_combine_effect();
				activate_force_break_combine_effect -> force_break:activate_break_combine_success_effect();
				activate_force_break_combine_success_effect -> force_break:set_break_combine_complete();
				play_set_force_break_combine_complete -> force_break:break_combine_success(PlayerPid, CardOrder, CardID);
				play_force_break_combine_success -> force_break:return_play_from_break_combine();
				
				% Using Skill
				
				
				check_assign_use_skill_ability -> new_seal_skill:verify_assign_use_skill_ability();
				verify_assign_use_skill_ability -> new_seal_skill:activate_assign_use_skill_ability();
				activate_assign_use_skill_ability -> new_seal_skill:check_seal_skill(PlayerPid, CardOrder, CardID);
				player_select_target_skill -> new_seal_skill:set_use_skill();
				finish_set_card_use_skill -> new_seal_skill:check_ability_need_activate(PlayerPid, CardOrder, CardID);
				check_use_skill_ability -> 	new_seal_skill:seal_use_skill_into_interfere();
				play_card_use_skill_into_interfere -> new_seal_skill:verify_use_skill_ability();
				verify_use_new_skill_ability ->  new_seal_skill:activate_use_skill_effect();
				activate_use_new_skill_effect -> new_seal_skill:verify_card_controller(PlayerPid, CardOrder, CardID);
				send_seal_skill_animate -> new_seal_skill:activate_skill(PlayerPid, CardOrder, CardID);
				activate_skill_effect -> new_seal_skill:activate_skill(PlayerPid, CardOrder, CardID);
				activate_skill_to_player -> new_skill_effect:check_continuous_effect();
				skill_player_check_continuous_effect -> new_skill_effect:activate_skill_to_player(PlayerPid, CardOrder, CardID);
				activate_skill_to_card -> new_skill_effect:check_continuous_effect();
				skill_card_check_continuous_effect -> new_skill_effect:activate_skill_to_card(PlayerPid, CardOrder, CardID);
				check_skill_success_ability -> new_seal_skill:verify_use_skill_success_ability(PlayerPid, CardOrder, CardID);
				verify_skill_success_ability -> new_seal_skill:activate_use_skill_success_effect();
				activate_skill_success_effect -> new_seal_skill:end_of_use_skill2(PlayerPid, CardOrder, CardID);
				
				
				update_player_mp -> seal_skill:check_ability_activate();
				check_use_skill_ability_activate -> seal_skill:activate_select_target_skill ();
				return_to_set_use_skill -> seal_skill:set_use_skill (PlayerPid);
				%play_seal_use_skill_into_interfere -> seal_skill:check_ability_when_use_skill_success (PlayerPid, CardOrder, CardID);
				update_opponent_player_select_target_skill -> seal_skill:update_player_select_target_skill(PlayerPid);
				%play_seal_use_skill_into_interfere -> seal_skill:verify_use_skill_ability();
				play_seal_use_skill_into_interfere -> seal_skill:check_activate_ability(PlayerPid, CardOrder, CardID);
				
				verify_use_skill_ability -> seal_skill:activate_use_skill_effect();
				activate_using_skill_effect -> seal_skill:verify_using_skill(PlayerPid, CardOrder, CardID);
				%check_use_skill_success_ability_activate -> seal_skill:verify_using_skill_ability(PlayerPid, CardOrder, CardID);
				ability_when_use_skill -> seal_skill:verify_skill_condition (PlayerPid, CardOrder, CardID);
				skill_condition -> seal_skill:check_use_skill_success_ability(PlayerPid, CardOrder, CardID);
				check_use_skill_success_ability -> seal_skill:verify_use_skill_success_ability();
				verify_use_skill_success_ability -> seal_skill:activate_use_skill_success_effect();
				activate_use_skill_success_effect -> seal_skill:activate_skill(PlayerPid, CardOrder, CardID);
				finish_set_seal_use_skill -> seal_skill:seal_use_skill_into_interfere();
				activate_effect_skill -> skill_effect:skill_effect_activate (PlayerPid, CardOrder, CardID);
				all_skill_fx_activate -> seal_skill:seal_using_skill_12 (PlayerPid, CardOrder, CardID);
				skill_effect_affect -> skill_effect:check_skill_effect_affect ();
				effect_affect -> skill_effect:affect_current_effect();
				%return_to_check_skill_effect_affect -> skill_effect:check_skill_effect_affect();
				return_to_check_skill_effect_affect -> skill_effect:check_continuous_effect();
				check_skill_continuous_effect -> skill_effect:check_skill_effect_affect();
				
				go_to_aaa -> skill_utility:aaa();
				%ทำงานต่อหลังจากที่ Client รับรู้แล้วว่า มีการเลือก Curse
				%update_player_select_curse -> skill_effect:add_selected_curse_to_fx();
				%finish_set_seal_use_skill ->seal_skill:seal_use_skill_into_interfere();
				go_to_end_of_use_skill -> seal_skill:end_of_use_skill(PlayerPid, CardOrder, CardID);
				
				% Shrine zone
				
				from_shrine_to_oher_zone -> change_zone:check_zone_to_move();
				
				move_to_shrine -> shrine_zone:card_to_shrine_change_number();
				play_card_to_shrine_change_number -> shrine_zone:check_update_shrine_level ();
				play_check_update_shrine_level -> shrine_zone:check_into_shrine_ability(PlayerPid);
				check_into_shrine_ability -> shrine_zone:check_remove_from_arena_ability(PlayerPid);
				check_remove_from_arena_ability -> shrine_zone:interfere_card_to_shrine();
				
				interfere_card_to_shrine -> shrine_zone:verify_into_shrine_ability();
				verify_into_shrine_ability -> shrine_zone:verify_remove_from_arena_ability();
				verify_remove_from_arena_ability -> shrine_zone:activate_into_shrine_effect();
				
				activate_into_shrine_effect -> shrine_zone:activate_remove_from_arena_effect();
				activate_remove_from_arena_effect -> shrine_zone:remove_from_zone_card_status();
				remove_from_zone_card_status -> shrine_zone:activate_shrine_interfere_2();
				shrine_interfere_2 -> shrine_zone:shrine_level_update();
				check_count_shrine_level-> shrine_zone:check_in_shrine_ability();
				check_in_shrine_ability -> shrine_zone:verify_in_shrine_ability();
				verify_in_shrine_ability -> shrine_zone:activate_in_shrine_effect();
				activate_in_shrine_effect -> shrine_zone:activate_as_in_shrine_effect();
				
				activate_as_in_shrine_effect -> shrine_zone:move_to_shrine_complete();
				
				
				
				%activate_ability_select_target -> shrine_zone:activate_select_target_ability ();				
				
				%move_to_shrine_interfere -> shrine_zone:check_card_ability_target ();
				%activate_ability_target_verify -> shrine_zone:all_ability_affact (PlayerPid);
				%select_player_ability_activate -> ability_utility:activate_select_ability_affect (PlayerPid);
				%select_opponent_ability_activate -> ability_utility:activate_select_ability_affect (PlayerPid);
				%activate_all_affect -> shrine_zone:shrine_level_update ();

				% Draw Step
				play_draw_step -> draw_step:draw_step_4 (PlayerPid);
				play_draw_step_4 -> draw_step:draw_step_5 (PlayerPid);
				play_draw_step_5 -> draw_step:draw_step_5_1 (PlayerPid);
				play_draw_step_5_1 -> draw_step:verify_onhand_draw_ability();
				verify_onhand_draw_ability -> draw_step:activate_onhande_draw_effect();
				activate_onhand_draw_effect ->draw_step:draw_step_5_2 (PlayerPid);
				play_draw_step_5_2 -> draw_step:draw_step_5 (PlayerPid);

				% Discard
				player_discard_0 -> discard:player_discard_1 ();
				play_player_discard_1 -> discard:player_discard_2 ();
				play_player_discard_2 -> discard:player_discard_3 ();
				play_player_discard_3 -> discard:player_discard_4 ();
				play_player_discard_4 -> discard:player_discard_5 ();
				play_player_discard_5 -> discard:player_discard_6 (PlayerPid);
				play_player_discard_6 -> discard:discard_to_shrine(PlayerPid);

				% Remove Zone
				
				from_remove_to_oher_zone -> change_zone:check_zone_to_move();
				
				card_remove -> remove_zone:card_to_remove_zone(PlayerPid);
				mystic_to_shrine_from_remove_card -> remove_zone:remove_card_to_zone(PlayerPid); %change_zone_to_remove (PlayerPid);
				removing_cards -> remove_zone:activate_remove_ability();
				check_ability_into_remove -> remove_zone:check_ability_select_target ();
				%activate_ability_remove_select_target -> remove_zone:activate_select_target_ability ();
				activate_ability_remove_select_target -> remove_zone:check_into_remove_ability(PlayerPid);
				check_into_remove_ability -> remove_zone:remove_card_sub_interfere();
				remove_card_sub_interfere -> remove_zone:verify_into_remove_ability();
				verify_into_remove_ability -> remove_zone:activate_into_remove_effect();
				activate_into_remove_effect -> remove_zone:interfere_remove_2();
				interfere_remove_2 -> remove_zone:completed_remove_cards();
				move_to_remove_interfere -> remove_zone:check_card_ability_target ();
				activate_ability_remove_target_verify -> remove_zone:activate_all_ability_affact (PlayerPid);
				activate_all_remove_ability_affect -> remove_zone:interfere_remove_2 ();
				move_to_remove_interfere_2 -> remove_zone:completed_remove_cards ();

				set_effect_to_target -> ability_activate:resume_card_effect ();

				% Check up step
				play_checkup_step -> check_up_step:check_1st_subturn_onhand_ability(PlayerPid);
				check_1st_subturn_onhand_ability -> check_up_step:verify_1st_subturn_ability_condition();
				verify_1st_subturn_ability_condition -> check_up_step:activate_1st_subturn_onhand_effect();
				activate_1st_subturn_onhand_effect -> check_up_step:activate_1st_onhand_as_long_as_effect();
				activate_1st_onhand_as_long_as_effect ->	check_up_step:checkup_step_3(PlayerPid);
				
				
				%play_at_subturn_activate_ability -> check_up_step:check_ability_affect_into_at_subturn();
				%play_df_subturn_activate_ability -> check_up_step:check_ability_affect_into_df_subturn();
				%play_checkup_activate_ability -> check_up_step:activate_checkup_effect ();
				into_at_subturn_ability_activate -> check_up_step:checkup_step_3_1();
				verify_into_at_subturn_ability -> check_up_step:activate_into_at_subturn_effect();
				%activate_into_at_subturn_effect -> check_up_step:checkup_step_4(PlayerPid);
				activate_into_at_subturn_effect -> check_up_step:activate_check_up_suspend_effect();
				activate_suspend_checkup_effect -> check_up_step:checkup_step_4(PlayerPid);
				into_df_subturn_ability_activate -> check_up_step:checkup_step_4_1();
				verify_into_df_subturn_ability -> check_up_step:activate_into_df_subturn_effect();
				activate_into_df_subturn_effect -> check_up_step:checkup_step_5(PlayerPid);
				into_any_subturn_ability_activate -> check_up_step:checkup_step_5_1();
				verify_into_any_subturn_ability -> check_up_step:activate_into_any_subturn_effect();
				activate_into_any_subturn_effect -> check_up_step:checkup_step_6();
				checkup_activate_no_trigger_effect -> check_up_step:effect_activate();
				
				
				% End of subturn
				play_end_of_subturn_step -> end_of_subturn:end_at_subturn_ability_check(PlayerPid);
				end_at_subturn_ability_check -> end_of_subturn:end_at_subturn_ability_verify();
				end_at_subturn_ability_verify -> end_of_subturn:end_at_subturn_effect_activate();
				end_at_subturn_effect_activate -> end_of_subturn:end_df_subturn_ability_check(PlayerPid);
				end_df_subturn_ability_check -> end_of_subturn:end_df_subturn_ability_verify();
				end_df_subturn_ability_verify -> end_of_subturn:end_df_subturn_effect_activate();
				end_df_subturn_effect_activate -> end_of_subturn:end_any_at_subturn_ability_check(PlayerPid);
				end_any_at_subturn_ability_check -> end_of_subturn:end_any_at_subturn_ability_verify();
				end_any_at_subturn_ability_verify -> end_of_subturn:end_any_at_subturn_effect_activate();
				end_any_at_subturn_effect_activate -> end_of_subturn:end_any_df_subturn_ability_check(PlayerPid);
				end_any_df_subturn_ability_check -> end_of_subturn:end_any_df_subturn_ability_verify();
				end_any_df_subturn_ability_verify -> end_of_subturn:end_any_df_subturn_effect_activate();
				end_any_df_subturn_effect_activate -> end_of_subturn:end_of_subturn_step_5();
				
				end_of_sub_effect -> end_of_subturn:end_of_any_subturn_duration_off();
				
				%refresh_end_subturn -> end_of_subturn:end_of_subturn_step_6 ();
				refresh_end_subturn -> end_of_subturn:end_of_at_subturn_duration_off();
				play_end_of_subturn_step_6 -> end_of_subturn:end_of_subturn_step_6_1 ();
				play_end_of_subturn_step_6_1 -> end_of_subturn:end_of_subturn_step_6_2 ();
				play_end_of_subturn_step_6_2 -> end_of_subturn:end_of_subturn_step_6_3 ();
				play_end_of_subturn_step_6_3 -> end_of_subturn:end_of_subturn_step_7 ();
				play_end_of_subturn_step_7 -> end_of_subturn:end_of_subturn_step_8 ();
				
				play_check_end_of_subturn_activate -> end_of_subturn:activate_end_of_turn_effect ();
				play_check_effect_decrement -> end_of_subturn:check_effect_decrement ();
				play_check_mystic_duration_decrement -> end_of_subturn:check_mystic_duration_decrement ();

				play_card_activate_move -> end_of_subturn:activate_card_move_by_effect ();

				% effect action
				%activate_action -> ability_affect:activate_action_affect();

				% move to hand
				draw_card -> draw_card:return_to_draw(PlayerPid);
				check_another_draw -> draw_card:check_another_draw();
				from_hand_to_oher_zone -> change_zone:check_zone_to_move();
				card_to_hand -> move_to_hand:check_moving_to_hand_ability();
				check_moving_to_hand_ability -> move_to_hand:moving_to_hand_interfere();
				card_to_hand_interfere -> move_to_hand:verify_moving_to_hand_ability();
				verify_moving_to_hand_ability -> move_to_hand:activate_moving_to_hand_ability();
				activate_moving_to_hand_ability -> move_to_hand:moving_to_hand_interfere2();
				card_to_hand_interfere2 -> move_to_hand:check_on_hand_ability();
				check_on_hand_ability  -> move_to_hand:verify_on_hand_ability();
				verify_on_hand_ability -> move_to_hand:activate_on_hand_ability();
				activate_on_hand_ability -> move_to_hand:card_to_hand_continuous_activate();
				activate_on_hand_effect -> move_to_hand:card_to_hand_complete();

				% curse activation
				play_curse_activation_assign ->curse_activation:curse_activation_assign_1_1 ();
				play_curse_activation_assign_1_1 -> curse_activation:curse_activation_assign_2 ();
				play_curse_activation_assign_2 -> curse_activation:curse_activation_assign_2_1 ();
				play_curse_activation_assign_2_1 -> curse_activation:curse_activation_assign_3 ();
				play_curse_activation_assign_3 -> curse_activation:curse_activation_assign_4 ();
				curse_activation_assign_4 -> curse_activation:curse_activation_assign_5();
				
				%activate_each_curse -> curse_activation:activate_curse_effect();
				
				% Other
				checking_effect_to_target -> ability_activate:activate_ability_checking ();
				play_check_player_effect -> ability_activate:check_player_effect ();				
				play_check_opponent_effect -> ability_activate:check_opponent_effect ();
				play_check_self_card_effect -> ability_activate:check_self_card_effect ();
				play_check_other_card_effect -> ability_activate:check_other_card_effect ();

				play_activate_ability_effect -> ability_activate:activate_ability_effect_to_target ();
				play_set_effect_to_player -> ability_activate:set_effect_to_player ();
				play_set_effect_to_opponent -> ability_activate:set_effect_to_opponent ();
				play_set_self_card_effect -> ability_activate:set_self_card_effect ();
				play_set_other_card_effect -> ability_activate:set_other_card_effect ();


				attack_subturn_discard -> discard:check_cards_discard (PlayerPid);
				attack_subturn_discarded -> interfere_step:return_play();
				defend_subturn_discard -> discard:check_cards_discard (PlayerPid);
				defend_subturn_discarded -> interfere_step:return_play();

				attack_subturn_draw -> mystic_effect_activate_set1:check_cards_draw (PlayerPid);
				activate_attack_subturn_draw -> interfere_step:return_play();
				defend_subturn_draw -> mystic_effect_activate_set1:check_cards_draw (PlayerPid);
				activate_defend_subturn_draw -> interfere_step:return_play();

				play_ability_affect -> ability_effect:check_card_affect ();
				effect_activate_1 -> mod_ability_effect:effect_activate_1();
				return_to_check_ability_effect_affect -> mod_ability_effect:check_ability_effect_affect();
				check_ability_continuous_effect -> mod_ability_effect:check_next_effect();
				
				player_effect_affect -> mod_ability_effect:check_effect_to_player();
				affect_player_ability_effect -> mod_ability_effect:affect_current_player_effect();
				return_to_check_effect_to_player -> mod_ability_effect:check_effect_to_player();
				
				% special effect 
				activate_first_effect -> special_effect:activate_second_effect();
				activate_second_effect -> special_effect:effect_activated();
				
				_ -> io:format ("Return to play out of range ~p~n", [LastPlayStep])
			end;
		{error} -> io:format("Get last stack for interfere error~n")
	end.

get_interfere_play_id (Play) ->
	case Play of
		casting_seal -> 0; % ผู้เล่นร่ายการ์ด
		casting_mystic -> 0;
		interfere_casting_mystic -> 0;
		interfere_casting_mystic2 -> 0;
		normal_atk -> 1; % การ์ดทำการโจมตีปกติ -
		check_interfere_atk_all -> 2; % เช็คการโจมตีออล
		card_attack_all_interfere -> 2; % การ์ดทำการโจมตีออล -
		select_target_skill -> 3; % ผู้เล่นใช้สกิล
		play_card_use_skill_into_interfere -> 3; % ผู้เล่นใช้สกิล
		card_to_hand_interfere -> 89; % นำการ์ดขึ้นมือ
		card_to_hand_interfere2 -> 89;
		hand_atk -> 90; % โจมตีขึ้นมือ
		hand_atk_normal -> 90; % โจมตีขึ้นมือ
		play_curse_activation_assign_2 -> 91; % การ์ดได้รับเคิร์ส
		move_library_interfere -> 92; % กลับเข้ากอง
		move_library_interfere2 -> 92; % กลับเข้ากอง
		get_into_change_line_sub_interfere -> 93; % กำหนดไลน์ -
		get_into_break_combina_sub_interfere -> 94; % การ์ดกำลังแยกการรวมร่าง
		get_into_combination_sub_interfere -> 95; % การ์ดกำลังทำการรวมร่าง
		shrine_interfere_2 -> 96; % มีการ์ดกำลังตก Shrine
		interfere_card_to_shrine -> 96;
		remove_card_sub_interfere -> 97; % การ์ดกำลังเปลี่ยนโซนไปที่รีมูฟโซน
		move_to_remove_interfere_2 -> 97; % การ์ดกำลังเปลี่ยนโซนไปที่รีมูฟโซน
		growth_interfere -> 98; % การ์ดต้องการใช้ความสามารถเจริญเติบโต
		_What -> io:format("What ~p ~n", [_What]), 99
	end.
