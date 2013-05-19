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
-module (end_of_subturn).

-import (lists, [flatlength/1, reverse/1]).

-compile (export_all).

% 401.9. End of Subturn Step
% 401.9.1. ใน End of Subturn Step ผู้เล่นทั้งสองฝ่ายไม่สามารถร่ายหรือสั่งการ Seal Card หรือ Mystic
% Card ใดๆ ทั้งที่เป็นและไม่เป็น Interfere แม้ว่าจะมี Interfere Step แทรกขึ้นมาก็ตาม ยกเว้นจะมี Effect ระบุให้ร่ายหรือสั่งการได้ใน Step นี้ -
end_of_subturn_step () ->
	{ok, PlayerPid} = mnesia_play:get_game_data (self(), player_turn),
	stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, play_end_of_subturn_step}, {card_player, PlayerPid}]),
	interfere_step:return_play(check_play_step).

%401.9.2Ability เมือจบ Subturn โจมตีทำงาน
end_at_subturn_ability_check(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, end_at_subturn_ability_check),
	mod_ability_activate:check_any_ability_activate(end_at_subturn, PlayerPid).
	
end_at_subturn_ability_verify() ->
	stack_pool:set_stack_option (self(), play, end_at_subturn_ability_verify),
	mod_ability_activate:verify_ability_condition(end_at_subturn).
	
end_at_subturn_effect_activate() ->
	stack_pool:set_stack_option (self(), play, end_at_subturn_effect_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(end_at_subturn, PlayerPid).
% จบการทำงาน Ability เมื่อจบ Subturn โจมตี

%401.9.3.  Ability เมือจบ Subturn ป?องกัน ของผู้เล?น Subturn ป?องกันทํางาน 
end_df_subturn_ability_check(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, end_df_subturn_ability_check),
	OppPid = mnesia_play:get_opponent_pid (PlayerPid),
	mod_ability_activate:check_any_ability_activate(end_df_subturn, OppPid).
	
end_df_subturn_ability_verify() ->
	stack_pool:set_stack_option (self(), play, end_df_subturn_ability_verify),
	mod_ability_activate:verify_ability_condition(end_df_subturn).
	
end_df_subturn_effect_activate() ->
	stack_pool:set_stack_option (self(), play, end_df_subturn_effect_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	OppPid = mnesia_play:get_opponent_pid(PlayerPid),
	mod_ability_effect:check_any_ability_activate(end_df_subturn, OppPid).
% จบการทำงาน Ability เมื่อจบ Subturn ป?องกัน ของผู้เล?น Subturn ป้องกัน

%401.9.4.  Ability เมือเข?าสู? End of Subturn Step ทํางาน ของผู้เล่นใน Sunturn โจมตีทำงาน
end_any_at_subturn_ability_check(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, end_any_at_subturn_ability_check),
	mod_ability_activate:check_any_ability_activate(end_any_at_subturn, PlayerPid).
	
end_any_at_subturn_ability_verify() ->
	stack_pool:set_stack_option (self(), play, end_any_at_subturn_ability_verify),
	mod_ability_activate:verify_ability_condition(end_any_at_subturn).
	
end_any_at_subturn_effect_activate() ->
	stack_pool:set_stack_option (self(), play, end_any_at_subturn_effect_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(end_any_at_subturn, PlayerPid).
% จบการทำงาน Ability เมื่อจบ Subturn

%401.9.4.  Ability เมือเข?าสู? End of Subturn Step ทํางาน ของผู้เล่นใน Sunturn ป้องกันทำงาน
end_any_df_subturn_ability_check(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, end_any_df_subturn_ability_check),
	OppPid = mnesia_play:get_opponent_pid (PlayerPid),
	mod_ability_activate:check_any_ability_activate(end_any_df_subturn, OppPid).
	
end_any_df_subturn_ability_verify() ->
	stack_pool:set_stack_option (self(), play, end_any_df_subturn_ability_verify),
	mod_ability_activate:verify_ability_condition(end_any_df_subturn).
	
end_any_df_subturn_effect_activate() ->
	stack_pool:set_stack_option (self(), play, end_any_df_subturn_effect_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	OppPid = mnesia_play:get_opponent_pid (PlayerPid),
	mod_ability_effect:check_any_ability_activate(end_any_df_subturn, OppPid).
% จบการทำงาน Ability เมื่อจบ Subturn

% 401.9.5. Effect ที่ส่งผลจนจบ Subturn จะหยุดทำงานลงภายใน Step นี้ -
end_of_subturn_step_5() ->
	stack_pool:set_stack_option(self(), play, refresh_end_subturn),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	OppPid = mnesia_play:get_opponent_pid (PlayerPid),
	update_ability:update(refresh_end_subturn, PlayerPid, OppPid),
	interfere_step:return_play(check_play_step).
	
end_of_at_subturn_duration_off() ->
	%io:format("end_of_at_subturn_duration_off ~p~n", [stack_pool:get_last_stack (self(), end_of_at_subturn)]),
	case stack_pool:get_last_stack (self(), end_of_at_subturn) of
		{ok, []} ->
			end_of_df_subturn_duration_off();
		{error, _} ->
			end_of_df_subturn_duration_off();
		{ok, [{CardOwner, CardOrder, CardID, _Fx, _FxField} | Cards]} ->
			stack_pool:set_stack_option (self(), end_of_at_subturn, Cards),
			%{ok, CardFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, FxField),
			%RemainFx = remain_effect(CardFx, Fx),
			%card_utility:update_card_option_field (CardOwner, CardOrder, CardID, FxField, RemainFx),
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
			end_of_at_subturn_duration_off()
	end.
	
end_of_df_subturn_duration_off() ->
	%io:format("end_of_df_subturn_duration_off ~p~n", [stack_pool:get_last_stack (self(), end_of_df_subturn)]),
	case stack_pool:get_last_stack (self(), end_of_df_subturn) of
		{ok, []} ->
			end_of_any_subturn_duration_off();
		{error, _} ->
			end_of_any_subturn_duration_off();
		{ok, [{CardOwner, CardOrder, CardID, _Fx, _FxField} | Cards]} ->
			stack_pool:set_stack_option (self(), end_of_df_subturn, Cards),
			%{ok, CardFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, FxField),
			%RemainFx = remain_effect(CardFx, Fx),
			%card_utility:update_card_option_field(CardOwner, CardOrder, CardID, FxField, RemainFx),
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
			end_of_df_subturn_duration_off()
	end.

end_of_any_subturn_duration_off() ->
	%io:format("end_of_subturn_duration_off ~p~n", [stack_pool:get_last_stack (self(), end_of_subturn)]),
	case stack_pool:get_last_stack (self(), end_of_subturn) of
		{ok, []} ->
			end_of_subturn_step_6();
		{error, _} ->
			end_of_subturn_step_6();
		{ok,[{CardOwner, CardOrder, CardID, FxField}| Cards]} ->
			stack_pool:set_stack_option(self(), play, end_of_sub_effect),
			stack_pool:set_stack_option(self(), end_of_subturn, Cards),
			check_special_effect({CardOwner, CardOrder, CardID}, FxField);
		{ok, [{CardOwner, CardOrder, CardID, _Fx, _FxField} | Cards]} ->
			stack_pool:set_stack_option (self(), end_of_subturn, Cards),
			%{ok, CardFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, FxField),
			%RemainFx = remain_effect(CardFx, Fx),
			%card_utility:update_card_option_field (CardOwner, CardOrder, CardID, FxField, RemainFx),
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
			end_of_any_subturn_duration_off()
	end.
	
check_special_effect({CardOwner, CardOrder, CardID}, FxField) ->
	case FxField of
		{move, to_remove_zone} -> remove_zone:move_to_remove_zone(CardOwner, [{CardOwner, CardOrder, CardID}]);
		{move, to_deck} -> move_to_library:move_to_library (CardOwner, [{CardOwner, CardOrder, CardID}]);
		{move, destroy} -> destroy:check_card_destroyed(CardOwner, [{CardOwner, CardOrder, CardID}], duration_destroy);
		_ -> interfere_step:return_play(check_play_step)
	end.

% 401.9.6. Effect ทุกอย่างที่ระบุจำนวน Turn ที่ยังคงอยู่ในสนาม ให้นับ Turn ลดลงใน Step นี้ 1 Subturn
end_of_subturn_step_6() ->
	stack_pool:set_stack_option(self(), play, play_check_effect_decrement),
	check_effect_decrement ().

end_of_subturn_step_6_1 () ->
	stack_pool:set_stack_option(self(), play, play_check_mystic_duration_decrement),
	check_mystic_duration_decrement ().

end_of_subturn_step_6_2 () ->
	stack_pool:set_stack_option(self(), play, play_check_end_of_subturn_activate),
	stack_pool:set_stack_option(self(), stack_option_field_check, card_activate_zero_turn),
	activate_end_of_turn_effect().

end_of_subturn_step_6_3 () ->
	stack_pool:set_stack_option (self(), play, play_end_of_subturn_step_6_3),
	case stack_pool:get_last_stack(self(), activate_destroy_card) of
		{ok, []} -> interfere_step:return_play(check_play_step);
		{error, _} -> interfere_step:return_play(check_play_step);
		{ok, Destroy} ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			destroy:check_card_destroyed(PlayerPid, Destroy, duration_destroy)
	end.
	
% 401.9.7. Seal ทุกใบในสนามที่มีสภาพ Inactive Seal จะกลับมามีสภาพเป็น Acitve Seal ใน Step นี้ -
end_of_subturn_step_7() ->
	stack_pool:set_stack_option (self(), play, play_end_of_subturn_step_7),
	case stack_pool:get_last_stack (self(), on_arena_cards) of
		{ok, []} ->
			interfere_step:return_play (check_play_step);
		{error, _} ->
			interfere_step:return_play (check_play_step);
		{ok, Cards} ->
			set_cards_on_arena_active (Cards)
	end.

set_cards_on_arena_active ([]) ->
	{ok, PlayerPid} = mnesia_play:get_game_data (self(), player_turn),
	mnesia_play:set_player_data (PlayerPid, first_turn, not_firse),
	interfere_step:return_play (check_play_step);
set_cards_on_arena_active ([{CardOwner, CardOrder, CardID} | Cards]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			card_utility:set_card_option_field (CardOwner, CardOrder, CardID, active, active, arena_zone),			
			set_cards_on_arena_active (Cards);
		is_not_seal ->
			set_cards_on_arena_active (Cards)
	end.

% 401.9.8. หากในสนามของเจ้าของ Subturn โจมตี ไม่มี Seal ใน Check up Step และยังคงไม่นำ Seal เข้ามาในสนาม เมื่อเข้าสู่ Step นี้ จะเป็นฝ่ายแพ้เกมนั้นใน Step นี้ -
end_of_subturn_step_8 () ->
	{ok, PlayerPid} = mnesia_play:get_game_data (self(), player_turn),
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	mnesia_play:remove_player_status (PlayerPid, disallow_first_attack),
	mnesia_play:remove_player_status (PlayerPid, already_attack_hand),
	mnesia_play:remove_player_status (OpponentPid, disallow_first_attack),
	mnesia_play:remove_player_status (OpponentPid, already_attack_hand),
	case mnesia_play:get_game_data(self(), seal_checkup) of
		% ไม่มีซีลในสนาม แพ้
		{ok, have_not_seal} ->
			stack_pool:remove_stack (self()),
			%smo_logger:fmsg("Room' s Process end game is ~p~n", [self()]),
			gen_server:cast(self(), {update_game_end, PlayerPid, player_loss});
		{ok, have_seal} ->
			stack_pool:pop_stack_out (self()),
			mnesia_play:set_game_data (self(), player_turn, OpponentPid),
			mnesia_play:set_player_data (PlayerPid, first_turn, not_first),
			play_utility:into_next_step()
	end.

% ------------------- External Call Function -------------------------------------------
activate_end_of_turn_effect () ->
	{ok, SOFCheck} = stack_pool:get_last_stack (self(), stack_option_field_check),
	EOSCheck = stack_pool:get_last_stack (self(), SOFCheck),
	case EOSCheck of
		{ok, []} ->
			check_stack_return_play (SOFCheck);
		{error, _} ->
			check_stack_return_play (SOFCheck);
		{ok, [{CardOwner, CardOrder, CardID, Fx} | Cards]} ->
			stack_pool:set_stack_option (self(), SOFCheck, Cards),
			activate_ability (CardOwner, CardOrder, CardID, Fx);
		{ok, [{CardOwner, CardOrder, CardID, _, Fx} | Cards]} ->
			stack_pool:set_stack_option (self(), SOFCheck, Cards),
			activate_ability (CardOwner, CardOrder, CardID, Fx)
	end.

check_stack_return_play (SOFCheck) ->
	stack_pool:remove_stack_option (self(), SOFCheck),
	case SOFCheck of
		card_activate_zero_turn -> interfere_step:return_play(play_end_of_subturn_step_6_2);
		_ ->	io:format ("Check stack return play ~p out of range~n", [SOFCheck])
	end.

activate_ability (CardOwner, CardOrder, CardID, Fx) ->
	case Fx of
		{move, to_df} ->
			line_change:move_to_line ([{CardOwner, CardOrder, CardID}], 0);
		{move, to_at} ->
			line_change:move_to_line ([{CardOwner, CardOrder, CardID}], 1);
		{move, to_deck} ->
			move_to_library:move_to_library (CardOwner, [{CardOwner, CardOrder, CardID}]);
		{move, to_remove_zone} ->
			remove_zone:move_to_remove_zone (CardOwner, [{CardOwner, CardOrder, CardID}]);
		_ ->	io:format ("End of subturn ~p effect update~n", [Fx]),
			effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [], update),
			interfere_step:return_play (check_play_step)
	end.

check_effect_decrement () ->
	case stack_pool:get_last_stack (self(), integer_duration_effect) of
		{ok, []} ->
			interfere_step:return_play (play_end_of_subturn_step_6);
		{error, _} ->
			interfere_step:return_play (play_end_of_subturn_step_6);
		{ok, [{CardOwner, CardOrder, CardID, CardZone} | Cards]} ->
			stack_pool:set_stack_option (self(), integer_duration_effect, Cards),
			set_decrese_effect_duration (CardOwner, CardOrder, CardID, CardZone)
	end.

set_decrese_effect_duration (CardOwner, CardOrder, CardID, CardZone) ->
	ACFx = decrease_effect_duratioin (CardOwner, CardOrder, CardID, receive_effect, CardZone),
	SCFx = decrease_effect_duratioin (CardOwner, CardOrder, CardID, skill_effect, CardZone),
	case CardZone of
		arena_zone ->
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, ACFx ++ SCFx, add);
		_ ->	not_send_effect_data
	end,
	interfere_step:return_play (check_play_step).

decrease_effect_duratioin (CardOwner, CardOrder, CardID, FxField, CardZone) ->
	case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, FxField, CardZone) of
		{ok, []} -> [];
		{ok, CardFx} ->
			{FxUpdate, CurseFx} = decrease_card_effect_duratioin (CardOwner, CardOrder, CardID, CardFx, CardZone),
			card_utility:update_card_option_field (CardOwner, CardOrder, CardID, FxField, FxUpdate, CardZone),
			CurseFx
	end.

decrease_card_effect_duratioin (_, _, _, [], _) -> {[], []};
decrease_card_effect_duratioin (CardOwner, CardOrder, CardID, [{GFx, Fx, Duration} | CardFx], CardZone) when is_integer (Duration) ->
	{CFxUpdate, CurseFxUpdate} = decrease_card_effect_duratioin (CardOwner, CardOrder, CardID, CardFx, CardZone),
	case Duration =:= 198 of
		true -> CurseFx = get_curse_effect (Fx),
			CFx = [{GFx, Fx, Duration}];
		false ->
			DurationUpdate = Duration - 1,
			if 
				DurationUpdate > 0 ->
					CurseFx = get_curse_effect (Fx),
					CFx = [{GFx, Fx, DurationUpdate}];
			   true ->
					check_zero_subturn_effect (CardOwner, CardOrder, CardID, Fx, CardZone),
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [{GFx, Fx}], remove),
					CurseFx = [],
					CFx = []
			end
	end,
	{CFx ++ CFxUpdate, CurseFx ++ CurseFxUpdate};
decrease_card_effect_duratioin (CardOwner, CardOrder, CardID, [{GFx, Fx, Duration} | CardFx], CardZone) ->
	{CFxUpdate, CurseFxUpdate} = decrease_card_effect_duratioin (CardOwner, CardOrder, CardID, CardFx, CardZone),
	{[{GFx, Fx, Duration}] ++ CFxUpdate, [] ++ CurseFxUpdate}.

get_curse_effect ([]) -> [];
get_curse_effect ([{curse, Curse} | Fx]) ->
	[{curse, Curse}] ++ get_curse_effect (Fx);
get_curse_effect ([_ | Fx]) -> get_curse_effect (Fx).

check_zero_subturn_effect (_, _, _, [], _) -> [];
check_zero_subturn_effect (CardOwner, CardOrder, CardID, [FxCheck | Fx], CardZone) ->
	case FxCheck of
		{curse, poison_curse} ->
			stack_pool:add_stack_option_field (self(), activate_destroy_card, [{CardOwner, CardOrder, CardID}]);
		{curse, {last_dance_curse, _}} ->
			stack_pool:add_stack_option_field (self(), activate_destroy_card, [{CardOwner, CardOrder, CardID}]);
		{move, to_df} ->
			stack_pool:add_stack_option_field (self(), card_activate_zero_turn, [{CardOwner, CardOrder, CardID, CardZone, {move, to_df}}]);
		{move, to_at} ->
			stack_pool:add_stack_option_field (self(), card_activate_zero_turn, [{CardOwner, CardOrder, CardID, CardZone, {move, to_at}}]);
		{move, to_deck} ->
			stack_pool:add_stack_option_field (self(), card_activate_zero_turn, [{CardOwner, CardOrder, CardID, CardZone, {move, to_deck}}]);
		{move, to_remove_zone} ->
			stack_pool:add_stack_option_field (self(), card_activate_zero_turn, [{CardOwner, CardOrder, CardID, CardZone, {move, to_remove_zone}}]);
		{at, _} -> not_process;
		{df, _} -> not_process;
		{sp, _} -> not_process;
		{ma, _} -> not_process;
		{mc, _} -> not_process;
		{ms, _} -> not_process;
		{ability_id, _} -> not_process;
		_ ->	io:format ("Fx zero turn check ~p not process ~n", [FxCheck])
	end,
	check_zero_subturn_effect (CardOwner, CardOrder, CardID, Fx, CardZone).

check_mystic_duration_decrement () ->
	case stack_pool:get_last_stack (self(), mystic_card_on_arena) of
		{ok, []} ->
			interfere_step:return_play (play_end_of_subturn_step_6_1);
		{error, _} ->
			interfere_step:return_play (play_end_of_subturn_step_6_1);
		{ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
			stack_pool:set_stack_option (self(), mystic_card_on_arena, Cards),
			set_decrese_mystic_duration (CardOwner, CardOrder, CardID)
	end.

set_decrese_mystic_duration(CardOwner, CardOrder, CardID) ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, duration, arena_zone) of
		{ok, MysticDuration} when is_integer(MysticDuration) ->
			case MysticDuration =< 1 of
				true ->	Duration = mystic_duration_decrement (CardOwner, CardOrder, CardID, MysticDuration);
				_ when MysticDuration < 100 ->
					Duration = mystic_duration_decrement (CardOwner, CardOrder, CardID, MysticDuration);
				_ ->	Duration = MysticDuration
			end,
			card_utility:update_card_option_field (CardOwner, CardOrder, CardID, duration, Duration, arena_zone),
			interfere_step:return_play (check_play_step);
		_ ->	interfere_step:return_play (check_play_step)
	end.

mystic_duration_decrement(CardOwner, CardOrder, CardID, MysticDuration) when MysticDuration =< 1 ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to, arena_zone) of 
		{ok, [{SPid, SOrder, Sid}]} ->
			io:format("get card option field paste_to"),
			stack_pool:add_stack_option_field (self(), activate_destroy_card, [{CardOwner, CardOrder, CardID}]),
			remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid);
		{ok, _} -> 
			stack_pool:add_stack_option_field(self(), activate_destroy_card, [{CardOwner, CardOrder, CardID}])
	end;	
mystic_duration_decrement (_, _, _, MysticDuration) when MysticDuration < 100 ->
	MysticDuration - 1;
mystic_duration_decrement (_, _, _, MysticDuration) -> MysticDuration.

remove_player_mystic_effect(CardOwner, CardOrder, CardID, PlayerPid) ->
	{ok, PlayerFx} = mnesia_play:get_player_data(PlayerPid, player_effect),
	RFxRemain = remove_mystic_effect(PlayerPid, PlayerFx, CardOwner, CardOrder, CardID),
	mnesia_play:set_player_data(PlayerPid, player_effect, RFxRemain).

% SPid, SOrder, Sid คือ การ์ด ที่ได้รับ Effect จาก Mystic CardOwner, CardOrder, CardID
remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid) ->
	  case card_utility:get_card_option_field(SPid, SOrder, Sid, receive_effect, arena_zone) of
		  {ok, RFx} ->
			  RFxUpdate = remove_mystic_effect(SPid, SOrder, Sid, RFx, CardOwner, CardOrder, CardID),
			  card_utility:update_card_option_field(SPid, SOrder, Sid, receive_effect, RFxUpdate, arena_zone);
		 _ -> card_not_on_zone
	 end,
	 case card_utility:get_card_option_field(SPid, SOrder, Sid, mystic, arena_zone) of
		 {ok, Mystic} ->
			 PastedUpdate = remove_mystic_from_seal(Mystic, {CardOwner, CardOrder, CardID}),
			 card_utility:update_card_option_field(SPid, SOrder, Sid, mystic, PastedUpdate, arena_zone);
		 _ -> card_not_on_zone
	 end,
	 effect_activate:send_update_activate_effect(SPid, SOrder, Sid, [], update).

remove_mystic_effect(_, [], _, _, _) -> [];
remove_mystic_effect(PlayerPid, [PlayerFx|PFx], CardOwner, CardOrder, CardID) ->
	case PlayerFx of
		{{CardOwner, CardOrder, CardID, _}, _Fx, depend_on_s} ->
			remove_mystic_effect(PlayerPid, PFx, CardOwner, CardOrder, CardID);
		_ ->	[PlayerFx] ++ remove_mystic_effect(PlayerPid, PFx, CardOwner, CardOrder, CardID)
	end.
	
remove_mystic_effect(_, _, _, [], _, _, _) -> [];
remove_mystic_effect(SPid, SOrder, Sid, [CardFx | CardFxRest], CardOwner, CardOrder, CardID) ->
	%io:format("remove mystic {~p, ~p, ~p} effect ~p~n", [CardOwner, CardOrder, CardID, CardFx]),
	case CardFx of
		{{CardOwner, CardOrder, CardID, MAbilityID}, Fx, depend_on_s} ->
			effect_activate:send_update_activate_effect(SPid, SOrder, Sid, [{{CardOwner, CardOrder, CardID, MAbilityID}, Fx}], remove),
			remove_mystic_effect(SPid, SOrder, Sid, CardFxRest, CardOwner, CardOrder, CardID);
		_ ->	[CardFx] ++ remove_mystic_effect(SPid, SOrder, Sid, CardFxRest, CardOwner, CardOrder, CardID)
	end.

remove_mystic_from_seal ([], _) -> [];
remove_mystic_from_seal ([RemovePasted | T], RemovePasted) -> T;
remove_mystic_from_seal ([H | T], RemovePasted) -> [H] ++ remove_mystic_from_seal (T, RemovePasted).

% ------------------------- General Function ------------------------------------------------------------------
check_end_of_subturn_effect (CardOwner, CardOrder, CardID, CardZone, RefleshStatus) ->
%	LastNow = now(),
	case RefleshStatus of
		[] ->	no_status_remove;
		_ ->	remove_status (CardOwner, CardOrder, CardID, CardZone, RefleshStatus)
	end,
	case {mnesia_odbc:is_seal_card (CardID), CardZone} of
		{is_not_seal, arena_zone} ->
			stack_pool:add_stack_option_field (self(), mystic_card_on_arena, [{CardOwner, CardOrder, CardID}]);
		_ ->	not_process
	end,
	check_eos_effect (CardOwner, CardOrder, CardID, CardZone).

remove_status (_, _, _, _, []) -> all_status_remove;
remove_status (CardOwner, CardOrder, CardID, CardZone, [Status | Reflesh]) ->
	card_utility:remove_card_status (CardOwner, CardOrder, CardID, Status, CardZone),
	remove_status (CardOwner, CardOrder, CardID, CardZone, Reflesh).

check_eos_effect (CardOwner, CardOrder, CardID, CardZone) ->
	check_and_delete_eos_effect (CardOwner, CardOrder, CardID, receive_effect, CardZone),
	check_and_delete_eos_effect (CardOwner, CardOrder, CardID, skill_effect, CardZone).

check_and_delete_eos_effect (CardOwner, CardOrder, CardID, FxType, CardZone) ->
	case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, FxType, CardZone) of
		{ok, []} -> ok;
		{ok, CardFx} ->
			FxUpdate = get_and_delete_eos_effect (CardOwner, CardOrder, CardID, CardZone, CardFx, FxType),
			card_utility:update_card_option_field (CardOwner, CardOrder, CardID, FxType, FxUpdate, CardZone)
	end.

get_and_delete_eos_effect(_, _, _, _, [], _) -> [];

get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [{GFx, Fx, end_of_at_subturn} | CardFx], FxType) ->
	get_eos_effect(CardOwner, CardOrder, CardID, end_of_at_subturn, {GFx, Fx, end_of_at_subturn}, FxType),
	get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType);
	
get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [{GFx, Fx, end_of_df_subturn} | CardFx], FxType) ->
	get_eos_effect(CardOwner, CardOrder, CardID, end_of_df_subturn, {GFx, Fx, end_of_at_subturn}, FxType),
	get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType);
	
get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [{GFx, Fx, end_of_subturn} | CardFx], FxType) ->
	get_eos_effect(CardOwner, CardOrder, CardID, end_of_subturn, {GFx, Fx, end_of_subturn}, FxType),
	get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType);
	
get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [{GFx, Fx, end_of_subturn_if_not_use} | CardFx], FxType) ->
	get_eos_effect(CardOwner, CardOrder, CardID, end_of_subturn, {GFx, Fx, end_of_subturn}, FxType),
	get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType);
	
get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [{GFx, Fx, Duration} | CardFx], FxType) when is_integer (Duration) ->
	stack_pool:add_stack_option_field (self(), integer_duration_effect, [{CardOwner, CardOrder, CardID, CardZone}]),
	[{GFx, Fx, Duration}] ++ get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType);
	
get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, [Fx | CardFx], FxType)  ->
	[Fx] ++ get_and_delete_eos_effect(CardOwner, CardOrder, CardID, CardZone, CardFx, FxType).

get_eos_effect(_, _, _, _, {_, [], _}, _) -> set_all_remove;
get_eos_effect(CardOwner, CardOrder, CardID, StackOptionField, {GFx, [FxCheck | Fx], Duration}, FxType) ->
	case FxCheck of
		{move, to_df} ->
			stack_pool:add_stack_option_field (self(), StackOptionField, [{CardOwner, CardOrder, CardID, FxCheck}]);
		{move, to_at} ->
			stack_pool:add_stack_option_field (self(), StackOptionField, [{CardOwner, CardOrder, CardID, FxCheck}]);
		{move, to_deck} ->
			stack_pool:add_stack_option_field (self(), StackOptionField, [{CardOwner, CardOrder, CardID, FxCheck}]);
		{move, to_remove_zone} ->
			stack_pool:add_stack_option_field (self(), StackOptionField, [{CardOwner, CardOrder, CardID, FxCheck}]);
		_ ->	stack_pool:add_stack_option_field (self(), StackOptionField, [{CardOwner, CardOrder, CardID, {GFx, FxCheck, Duration}, FxType}])
	end,
	get_eos_effect(CardOwner, CardOrder, CardID, StackOptionField, {GFx, Fx, Duration}, FxType).