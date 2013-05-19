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
-module(check_other).
-compile(export_all).

check_all_other_require_attribute(AbilOwnCardData, PlayerZoneListHead, [ConditionHead|Tail], PlayStatus) ->
	Return = check_each_other_require_attribute(AbilOwnCardData, PlayerZoneListHead, ConditionHead, PlayStatus),
	if 
		Return =:= true -> check_all_other_require_attribute(AbilOwnCardData, PlayerZoneListHead, Tail, PlayStatus);
		true -> []
	end;
check_all_other_require_attribute(_, PlayerZoneListHead, [], _) -> 
	{_, OtherCardData} = PlayerZoneListHead,
	[OtherCardData].
%===============================================================================
check_each_other_require_attribute(AbilityOwnCardData, OtherCardDataZone, {Attribute, Value}, PlayStatus) ->
	%io:format('Attribute ~p~n',[Attribute]),
	%io:format('OtherCardDataZone ~p~n',[OtherCardDataZone]),
	case Attribute of
		card_id -> attribute_check:id_check(OtherCardDataZone, Value);	
		card_type -> 
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:card_type_check(CardID, Value);
		paste ->
			attribute_check:get_mystic_paste(AbilityOwnCardData, OtherCardDataZone, Value);
		paste_type ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:paste_type_check(CardData, Value);
		mystic_type ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:mystic_type_check(CardData, Value);
		mystic_subtype ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:mystic_subtype_check(CardData, Value);
		mystic_paste -> 
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			PlayerOppID = mnesia_play:get_opponent_pid(OwnerPid),
			attribute_check:mystic_paste_check(game_info:card_mystic_paste(OtherCardDataZone), Value, OwnerPid, PlayerOppID);
		mystic_recent_paste ->
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			PlayerOppID = mnesia_play:get_opponent_pid(OwnerPid),
			attribute_check:mystic_paste_check(function_utility:last(game_info:card_mystic_paste(OtherCardDataZone)), Value, OwnerPid, PlayerOppID);
		name -> attribute_check:name_check(game_info:card_name(OtherCardDataZone), Value);
		naming -> attribute_check:naming_check(game_info:card_naming(OtherCardDataZone), Value);
		naming_or -> attribute_check:naming_or_check(game_info:card_naming(OtherCardDataZone), Value);
		active -> attribute_check:active_status_check(game_info:card_active_status(OtherCardDataZone), Value);
		line -> attribute_check:line_check(game_info:card_line(OtherCardDataZone), Value, game_info:card_curse(OtherCardDataZone));
		action -> attribute_check:action_check(game_info:card_action(OtherCardDataZone), Value);
		any_action -> attribute_check:any_action_check(game_info:card_action(OtherCardDataZone), Value);
		elem -> attribute_check:element_check(game_info:card_element(OtherCardDataZone), Value);
		type -> attribute_check:type_check(game_info:card_type(OtherCardDataZone), Value);
		type_or -> attribute_check:type_or_check(game_info:card_type(OtherCardDataZone), Value);
		elem_or_type ->	attribute_check:elem_or_type_check(game_info:card_element(OtherCardDataZone), game_info:card_type(OtherCardDataZone), Value);
		level -> attribute_check:level_check(AbilityOwnCardData, game_info:card_level(OtherCardDataZone), Value);
		mpcast -> attribute_check:mp_cast_check(AbilityOwnCardData, game_info:card_mpcast(OtherCardDataZone), Value);
		mpat -> attribute_check:mpat_check(AbilityOwnCardData, game_info:card_mpat(OtherCardDataZone), Value);
		attack -> attribute_check:at_check(AbilityOwnCardData, game_info:card_at(OtherCardDataZone), Value);
		defend -> attribute_check:df_check(AbilityOwnCardData, game_info:card_df(OtherCardDataZone), Value);
		speed -> attribute_check:sp_check(other_check, AbilityOwnCardData, game_info:card_sp(OtherCardDataZone), Value, PlayStatus);
		curse -> attribute_check:curse_check(game_info:card_curse(OtherCardDataZone), Value);
		protect_curse -> attribute_check:protect_curse_check(game_info:card_protect_curse(OtherCardDataZone), Value);
		combine -> attribute_check:combine_check(OtherCardDataZone, Value);
		skill -> attribute_check:skill_effect_check(game_info:mystic_ability_effect(OtherCardDataZone), Value);
		ability -> attribute_check:ability_effect_check(game_info:mystic_ability_effect(OtherCardDataZone), Value);
		mystic -> attribute_check:mystic_paste_check(game_info:card_mystic_paste(OtherCardDataZone), Value);
		growth -> attribute_check:growth_check(game_info:card_growth(OtherCardDataZone), Value);
		counter -> attribute_check:counter_check(game_info:card_counter(OtherCardDataZone), Value);
		have_ability -> attribute_check:have_ability(OtherCardDataZone, Value);
		have_skill -> 
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:have_skill(CardID, Value);
		have_one ->
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:have_skill_ability(CardID, Value);
		protect_skill -> attribute_check:check_protect_skill(AbilityOwnCardData, OtherCardDataZone, Value);
		can_move_to -> attribute_check:move_to_line_check(AbilityOwnCardData, OtherCardDataZone, Value);
		can_paste_to_seal -> attribute_check:paste_to_seal_check(AbilityOwnCardData, OtherCardDataZone, Value);
		stack_check -> attribute_check:stack_check(OtherCardDataZone, Value);
		check_flag -> attribute_check:check_flag(game_info:card_action(AbilityOwnCardData), Value);
		support_to -> attribute_check:support_to(AbilityOwnCardData, OtherCardDataZone, Value);
		protect_attack -> attribute_check:check_protect_attack(AbilityOwnCardData, OtherCardDataZone, Value);
		true -> 
			io:format('mismatch case ~p~n', [Attribute]),
			false
	end.
%===============================================================================
check_all_other_require_attribute(AbilOwnCardData, PlayerZoneListHeadl, [ConditionHead|Tail]) ->
	%io:format('check_all_other_require_attribute ~n'),
	%io:format('PlayerZoneListHeadl ~p~n',[PlayerZoneListHeadl]),
	%io:format('ConditionHead ~p~n',[ConditionHead]),
	Return = check_each_other_require_attribute(AbilOwnCardData, PlayerZoneListHeadl, ConditionHead),
	%io:format('Return ~p~n',[Return]),
	case	Return of
		true -> check_all_other_require_attribute(AbilOwnCardData, PlayerZoneListHeadl, Tail);
		_Mismatch -> []%smo_logger:fmsg("Ability of ~p is mismatch to ~p {~p case ~p}~n", [AbilOwnCardData, PlayerZoneListHeadl, ConditionHead, Mismatch]), []
	end;
check_all_other_require_attribute(_, PlayerZoneListHeadl, []) -> 
	{_, OtherCardData} = PlayerZoneListHeadl,
	[OtherCardData].
%===============================================================================
check_each_other_require_attribute(AbilityOwnCardData, OtherCardDataZone, {Attribute, Value}) ->
	%io:format('Attribute ~p~n',[Attribute]),
	%io:format('OtherCardDataZone ~p~n',[OtherCardDataZone]),
	case Attribute of
		card_id -> attribute_check:id_check(OtherCardDataZone, Value);	
		card_type -> 
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:card_type_check(CardID, Value);
			
		paste ->
			attribute_check:get_mystic_paste(AbilityOwnCardData, OtherCardDataZone, Value);
		paste_type ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:paste_type_check(CardData, Value);
		mystic_type ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:mystic_type_check(CardData, Value);
		mystic_subtype ->	
			{_, CardData} = OtherCardDataZone,
			attribute_check:mystic_subtype_check(CardData, Value);
		mystic_paste -> 
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			PlayerOppID = mnesia_play:get_opponent_pid(OwnerPid),
			attribute_check:mystic_paste_check(game_info:card_mystic_paste(OtherCardDataZone), Value, OwnerPid, PlayerOppID);
	
		mystic_recent_paste ->
			{_,{OwnerPid, _, _}} = AbilityOwnCardData,
			PlayerOppID = mnesia_play:get_opponent_pid(OwnerPid),
			attribute_check:mystic_paste_check(function_utility:last(game_info:card_mystic_paste(OtherCardDataZone)), Value, OwnerPid, PlayerOppID);
		name -> attribute_check:name_check(game_info:card_name(OtherCardDataZone), Value);
		naming -> attribute_check:naming_check(game_info:card_naming(OtherCardDataZone), Value);
		naming_or -> attribute_check:naming_or_check(game_info:card_naming(OtherCardDataZone), Value);
		active -> attribute_check:active_status_check(game_info:card_active_status(OtherCardDataZone), Value);
		can_change_line -> attribute_check:can_change_line(OtherCardDataZone, Value);
		line -> attribute_check:line_check(game_info:card_line(OtherCardDataZone), Value, game_info:card_curse(OtherCardDataZone));
		action -> attribute_check:action_check(game_info:card_action(OtherCardDataZone), Value);
		any_action -> attribute_check:any_action_check(game_info:card_action(OtherCardDataZone), Value);
		elem -> attribute_check:element_check(game_info:card_element(OtherCardDataZone), Value);
		type -> attribute_check:type_check(game_info:card_type(OtherCardDataZone), Value);
		type_or -> attribute_check:type_or_check(game_info:card_type(OtherCardDataZone), Value);
		elem_or_type ->	attribute_check:elem_or_type_check(game_info:card_element(OtherCardDataZone), game_info:card_type(OtherCardDataZone), Value);
		level -> attribute_check:level_check(AbilityOwnCardData, game_info:card_level(OtherCardDataZone), Value);
		mpcast -> attribute_check:mp_cast_check(AbilityOwnCardData, game_info:card_mpcast(OtherCardDataZone), Value);
		mpat -> attribute_check:mpat_check(AbilityOwnCardData, game_info:card_mpat(OtherCardDataZone), Value);
		attack -> attribute_check:at_check(AbilityOwnCardData, game_info:card_at(OtherCardDataZone), Value);
		defend -> attribute_check:df_check(AbilityOwnCardData, game_info:card_df(OtherCardDataZone), Value);
		speed -> attribute_check:sp_check(AbilityOwnCardData, game_info:card_sp(OtherCardDataZone), Value);
		skill_target_speed -> attribute_check:skill_taret_sp_check(AbilityOwnCardData, game_info:card_sp(OtherCardDataZone), Value);
		curse -> attribute_check:curse_check(game_info:card_curse(OtherCardDataZone), Value);
		protect_curse -> attribute_check:protect_curse_check(game_info:card_protect_curse(OtherCardDataZone), Value);
		combine -> attribute_check:combine_check(OtherCardDataZone, Value);
		skill -> attribute_check:skill_effect_check(game_info:mystic_ability_effect(OtherCardDataZone), Value);
		ability -> attribute_check:ability_effect_check(game_info:mystic_ability_effect(OtherCardDataZone), Value);
		mystic -> attribute_check:mystic_paste_check(game_info:card_mystic_paste(OtherCardDataZone), Value);
		growth -> attribute_check:growth_check(game_info:card_growth(OtherCardDataZone), Value);
		counter -> attribute_check:counter_check(game_info:card_counter(OtherCardDataZone), Value);
		have_ability -> attribute_check:have_ability(OtherCardDataZone, Value);
		have_skill -> 
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:have_skill(CardID, Value);
		have_one ->
			{_, {_, _, CardID}} = OtherCardDataZone,
			attribute_check:have_skill_ability(CardID, Value);
		protect_skill ->
			attribute_check:check_protect_skill(AbilityOwnCardData, OtherCardDataZone, Value);
		can_move_to ->
			attribute_check:move_to_line_check(AbilityOwnCardData, OtherCardDataZone, Value);
		can_paste_to_seal ->
			attribute_check:paste_to_seal_check(AbilityOwnCardData, OtherCardDataZone, Value);
		stack_check -> attribute_check:stack_check(OtherCardDataZone, Value);
		check_flag ->
			attribute_check:check_flag(game_info:card_action(AbilityOwnCardData), Value);
		support_to ->
			attribute_check:support_to(AbilityOwnCardData, OtherCardDataZone, Value);
		protect_attack ->
			attribute_check:check_protect_attack(AbilityOwnCardData, OtherCardDataZone, Value);
		_ -> 
			io:format('mismatch case ~p~n', [Attribute]),
			false
	end.
%===============================================================================