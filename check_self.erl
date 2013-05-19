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
-module(check_self).
-compile(export_all).

check_each_self_require_attribute(AbilityOwnCardData, PlayerOppID, {Attribute, Value}, AbilityId, PlayStatus) ->
	%io:format('check ~p~n', [Attribute]),
	case Attribute of
		pre_zone ->
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:pre_zone_check(CardZone, Value);
		post_zone ->
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:post_zone_check(CardZone, Value);
		zone -> 
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:present_check(CardZone, Value);
		active_zone -> 
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:active_zone_check(CardZone, Value); 
		line -> attribute_check:line_check(game_info:card_line(AbilityOwnCardData), Value, game_info:card_curse(AbilityOwnCardData));
		action -> attribute_check:action_check(game_info:card_action(AbilityOwnCardData), Value, PlayStatus);
		any_action -> attribute_check:any_action_check(game_info:card_action(AbilityOwnCardData), Value);
		stack_check -> attribute_check:stack_check(AbilityOwnCardData, Value);
		naming -> attribute_check:naming_check(game_info:card_naming(AbilityOwnCardData), Value);
		elem -> attribute_check:element_check(game_info:card_element(AbilityOwnCardData), Value);
		type -> attribute_check:type_check(game_info:card_type(AbilityOwnCardData), Value);
		active -> attribute_check:active_status_check(game_info:card_active_status(AbilityOwnCardData), Value);
		curse -> attribute_check:curse_check(game_info:card_curse(AbilityOwnCardData), Value);
		protect_curse -> attribute_check:protect_curse_check(game_info:card_protect_curse(AbilityOwnCardData), Value);
		combine -> attribute_check:combine_check(AbilityOwnCardData, Value, PlayStatus);
		skill -> attribute_check:skill_effect_check(game_info:card_skill_effect(AbilityOwnCardData), Value);
		ability -> attribute_check:ability_effect_check(game_info:card_ability_effect(AbilityOwnCardData), Value);
		mystic_paste -> 
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			attribute_check:mystic_paste_check(game_info:card_mystic_paste(AbilityOwnCardData), Value, OwnerPid, PlayerOppID);
		
		paste -> attribute_check:get_mystic_paste(AbilityOwnCardData, AbilityOwnCardData, Value);
		paste_on -> attribute_check:pasted_target(AbilityOwnCardData, Value);
		mystic_recent_paste ->
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			attribute_check:mystic_paste_check(function_utility:last(game_info:card_mystic_paste(AbilityOwnCardData)), Value, OwnerPid, PlayerOppID);
		growth -> attribute_check:growth_check(game_info:card_growth(AbilityOwnCardData), Value);
		counter -> attribute_check:counter_check(game_info:card_counter(AbilityOwnCardData), Value);
		{Whose, Zone, Card, Self} -> attribute_check:check_card_condition({Whose, Zone, Card, Self}, AbilityOwnCardData, PlayerOppID, Value);
		
		
		%seal_owner_arena_include_self -> attribute_check:s_owner_arena_include_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_owner_arena -> attribute_check:s_owner_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_owner_hand -> attribute_check:s_owner_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_controller_hand -> attribute_check:s_controller_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_owner_hand -> attribute_check:m_owner_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_owner_shrine -> attribute_check:m_owner_shrine_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_owner_deck -> attribute_check:m_owner_deck_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_controller_hand -> attribute_check:m_controller_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%seal_opponent_arena -> attribute_check:s_opp_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_opponent_hand -> attribute_check:s_opp_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_uncontrol_arena -> attribute_check:s_uncon_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_uncontrol_hand -> attribute_check:s_uncon_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%card_uncontrol_hand ->  attribute_check:c_uncon_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_opponent_hand -> attribute_check:m_opp_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_opponent_shrine -> attribute_check:m_opp_shrine_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_opponent_deck -> attribute_check:m_opp_deck_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%seal_arena -> attribute_check:s_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_arena -> attribute_check:m_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%other_seal_arena -> attribute_check:other_s_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		
		have_ability -> attribute_check:have_ability(AbilityOwnCardData, Value);
		have_skill ->
			{_, {_, _, CardID}} = AbilityOwnCardData,
			attribute_check:have_skill(CardID, Value);
		have_one ->
			{_, {_, _, CardID}} = AbilityOwnCardData,
			attribute_check:have_skill_ability(CardID, Value);
		protect_skill ->
			attribute_check:check_protect_skill(AbilityOwnCardData, AbilityOwnCardData, Value);
		activate_ability -> attribute_check:check_activate_ability(AbilityOwnCardData, AbilityId, Value);
		can_move_to ->
			attribute_check:move_to_line_check(AbilityOwnCardData, AbilityOwnCardData, Value);
		arena_count ->
			attribute_check:arena_count(AbilityOwnCardData, PlayerOppID, Value);
		hand_count ->
			attribute_check:hand_count(AbilityOwnCardData, PlayerOppID, Value);
		check_flag ->
			attribute_check:check_flag(game_info:card_action(AbilityOwnCardData), Value);
		_ -> 
			io:format('mismatch case ~p~n', [Attribute]),
			false
	end.
%===============================================================================
check_each_self_require_attribute(AbilityOwnCardData, PlayerOppID, {Attribute, Value}, AbilityId) ->
	%io:format('check ~p~n', [Attribute]),
	case Attribute of
		zone -> 
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:present_check(CardZone, Value);
		active_zone -> 
			{CardZone,_ } = AbilityOwnCardData,
			attribute_check:active_zone_check(CardZone, Value); 
		line -> attribute_check:line_check(game_info:card_line(AbilityOwnCardData), Value, game_info:card_curse(AbilityOwnCardData));
		action -> attribute_check:action_check(game_info:card_action(AbilityOwnCardData), Value);
		any_action -> attribute_check:any_action_check(game_info:card_action(AbilityOwnCardData), Value);
		stack_check -> attribute_check:stack_check(AbilityOwnCardData, Value);
		naming -> attribute_check:naming_check(game_info:card_naming(AbilityOwnCardData), Value);
		elem -> attribute_check:element_check(game_info:card_element(AbilityOwnCardData), Value);
		type -> attribute_check:type_check(game_info:card_type(AbilityOwnCardData), Value);
		active -> attribute_check:active_status_check(game_info:card_active_status(AbilityOwnCardData), Value);
		curse -> attribute_check:curse_check(game_info:card_curse(AbilityOwnCardData), Value);
		protect_curse -> attribute_check:protect_curse_check(game_info:card_protect_curse(AbilityOwnCardData), Value);
		combine -> attribute_check:combine_check(AbilityOwnCardData, Value);
		skill -> attribute_check:skill_effect_check(game_info:card_skill_effect(AbilityOwnCardData), Value);
		ability -> attribute_check:ability_effect_check(game_info:card_skill_effect(AbilityOwnCardData), Value);
		mystic_paste -> 
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			attribute_check:mystic_paste_check(game_info:card_mystic_paste(AbilityOwnCardData), Value, OwnerPid, PlayerOppID);
			
		paste ->
			attribute_check:get_mystic_paste(AbilityOwnCardData, AbilityOwnCardData, Value);
		paste_on -> attribute_check:pasted_target(AbilityOwnCardData, Value);
			
		mystic_recent_paste ->
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			attribute_check:mystic_paste_check(function_utility:last(game_info:card_mystic_paste(AbilityOwnCardData)), Value, OwnerPid, PlayerOppID);
		growth -> attribute_check:growth_check(game_info:card_growth(AbilityOwnCardData), Value);
		counter -> attribute_check:counter_check(game_info:card_counter(AbilityOwnCardData), Value);

		{Whose, Zone, Card, Self} -> attribute_check:check_card_condition({Whose, Zone, Card, Self}, AbilityOwnCardData, PlayerOppID, Value);
		
		%seal_owner_arena_include_self -> attribute_check:s_owner_arena_include_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_owner_arena -> attribute_check:s_owner_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_owner_hand -> attribute_check:s_owner_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_controller_hand -> attribute_check:s_controller_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_owner_hand -> attribute_check:m_owner_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_owner_shrine -> attribute_check:m_owner_shrine_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_owner_deck -> attribute_check:m_owner_deck_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_controller_hand -> attribute_check:m_controller_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%seal_opponent_arena -> attribute_check:s_opp_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_opponent_hand -> attribute_check:s_opp_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_uncontrol_arena -> attribute_check:s_uncon_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%seal_uncontrol_hand -> attribute_check:s_uncon_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%card_uncontrol_hand ->  attribute_check:c_uncon_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%mystic_opponent_hand -> attribute_check:m_opp_hand_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_opponent_shrine -> attribute_check:m_opp_shrine_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_opponent_deck -> attribute_check:m_opp_deck_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%seal_arena -> attribute_check:s_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		%mystic_arena -> attribute_check:m_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		
		%other_seal_arena -> attribute_check:other_s_arena_check(AbilityOwnCardData, PlayerOppID, Value);
		
		have_ability -> 	attribute_check:have_ability(AbilityOwnCardData, Value);
		have_skill ->
			{_, {_, _, CardID}} = AbilityOwnCardData,
			attribute_check:have_skill(CardID, Value);
		have_one ->
			{_, {_, _, CardID}} = AbilityOwnCardData,
			attribute_check:have_skill_ability(CardID, Value);
		protect_skill ->
			attribute_check:check_protect_skill(AbilityOwnCardData, AbilityOwnCardData, Value);
		activate_ability -> attribute_check:check_activate_ability(AbilityOwnCardData, AbilityId, Value);
		can_move_to ->
			attribute_check:move_to_line_check(AbilityOwnCardData, AbilityOwnCardData, Value);
		arena_count ->
			attribute_check:arena_count(AbilityOwnCardData, PlayerOppID, Value);
		hand_count ->
			attribute_check:hand_count(AbilityOwnCardData, PlayerOppID, Value);
		check_flag ->
			attribute_check:check_flag(game_info:card_action(AbilityOwnCardData), Value);
		_ -> 
			io:format('mismatch case ~p~n', [Attribute]),
			false
	end.
%===============================================================================