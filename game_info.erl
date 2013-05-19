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
-module(game_info).
-import (mnesia_table, [do/1]).
-import (lists, [foreach/2]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
%===============================================================================
player_turn(PlayerID) ->
	{_, PlayerTurnPid} = mnesia_play:get_game_data(self(), player_turn),
	if 
		PlayerTurnPid =:= PlayerID -> PlayerTurn = at;
		PlayerTurnPid =/= PlayerID -> PlayerTurn = df
	end,
	PlayerTurn.
%===============================================================================
player_pharse() ->
	{_, PlayerPharse} = mnesia_play:get_game_data(self(), game_step),
	PlayerPharse.
%===============================================================================
player_mp(PlayerID) ->
	{_, PlayerMp} = mnesia_play:get_player_data(PlayerID, mp_rest),
	PlayerMp.
%===============================================================================
player_action(PlayerID) ->
	{_, PlayerAction} = mnesia_play:get_player_data(PlayerID, player_status),
	PlayerAction.
%=======================*****************************===========================
card_growth({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, growth, Zone),
			if
				Status =:= ok -> IsGrowth = Result;
				Status =/= ok -> IsGrowth = 0
			end;
		is_not_seal -> 
			IsGrowth = [];
		true -> 
			io:format('error ~n'),
			IsGrowth = []
	end,
	IsGrowth.
%===============================================================================
card_name({_CardZone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			%[CardName]= do(qlc:q([ X#seal_card.card_name || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]));
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			{ok, CardName} = seal_card:get_power_type(card_name, CardInfo);
		is_not_seal -> 
			%[CardName] = do(qlc:q([ X#mystic_card.card_name || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID]));
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = mystic_card:get_mystic_option(CardOption, information),
			{ok, CardName} = mystic_card:get_power_type(card_name, CardInfo);
		true -> 
			io:format('error ~n'),
			CardName = []
	end,
	%io:format('CardName ~p~n', [CardName]),
	CardName.
%===============================================================================
card_naming({_CardZone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			%[CardNaming] = do(qlc:q([ X#seal_card.card_naming || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]));
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			{ok, CardNaming} = seal_card:get_power_type(card_naming, CardInfo);
		is_not_seal -> 
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = mystic_card:get_mystic_option(CardOption, information),
			{ok, CardNaming} = mystic_card:get_power_type(card_naming, CardInfo)
	end,
	CardNaming.
%===============================================================================
card_line({Zone, {CardOwner, CardOrder, CardID}}) ->
	{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, Zone),
	if
		Status =:= ok -> CardLine = Result;
		Status =/= ok -> CardLine = 0
	end,
	CardLine.
%===============================================================================
card_active_status({Zone, {CardOwner, CardOrder, CardID}}) ->
	{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, active, Zone),
	if
		Status =:= ok -> CardActive = Result;
		Status =/= ok -> CardActive = 0
	end,
	CardActive.
%===============================================================================
card_action({Zone, {CardOwner, CardOrder, CardID}}) ->
	{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status, Zone),
	if
		Status =:= ok -> CardAction = Result;
		Status =/= ok -> CardAction = []
	end,
	CardAction.
%===============================================================================
card_element({_CardZone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			{ok, CardElement} = seal_card:get_power_type(card_element, CardInfo);
		is_not_seal -> CardElement = [];
		true -> 
			io:format('error ~n'),
			CardElement =[]
	end,
	CardElement.
%===============================================================================
card_type({_CardZone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			{ok, CardType} = seal_card:get_power_type(card_type, CardInfo);
		is_not_seal -> %CardType = [];
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = mystic_card:get_mystic_option(CardOption, information),
			{ok, MysticType} =mystic_card:get_power_type(card_type, CardInfo),
			CardType = [MysticType];
		true -> 
			io:format('error ~n'),
			CardType =[]
	end,
	CardType.
%===============================================================================
card_level({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			if 
				Zone =:= arena_zone -> {_, CardLevel} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, level);
				true -> [CardLevel] = do(qlc:q([ X#seal_card.level || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]))
			end;
		is_not_seal -> CardLevel = [];
		true -> 
			io:format('error ~n'),
			CardLevel = []
	end,
	CardLevel.
%===============================================================================
card_mpcast({Zone, {CardOwner, CardOrder, CardID}}) ->
	CardMP_Cast =
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			case Zone of
				arena_zone -> 
					{ok, CardMc} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, mp_cast),
					CardMc;
				hand_cards ->
					MpChange = 
					case hand_zone:check_ability(CardOwner, CardOrder, CardID, mc) of
						{ok, Value} -> Value;
						_ -> 0
					end,
					[CardMP] = do(qlc:q([X#seal_card.mp_cast || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID])),
					MpChange + CardMP;
				_ -> 
					[CardMc] = do(qlc:q([ X#seal_card.mp_cast || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID])),
					CardMc
			end;
		is_not_seal -> %CardMP_Cast = [];
			case Zone of
				arena_zone -> 
					[CardMc] = do(qlc:q([X#mystic_card.mp_cast || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID])),
					%{ok, CardMc} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, mp_cast),
					CardMc;
				hand_cards ->
					MpChange =
					case hand_zone:check_ability(CardOwner, CardOrder, CardID, mc) of
						{ok, Value} -> Value;
						_ -> 0
					end,
					[CardMP] = do(qlc:q([X#mystic_card.mp_cast || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID])),
					MpChange + CardMP;
				_ -> 
					[CardMc] = do(qlc:q([X#mystic_card.mp_cast || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID])),
					CardMc
			end
	end,
	CardMP_Cast.
%===============================================================================
card_mpat({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
		if 
			 Zone =:= arena_zone -> {ok, CardMP_AT} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, mp_atk);
			true -> [CardMP_AT] = do(qlc:q([ X#seal_card.mp_atk || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]))
		end;
		is_not_seal -> CardMP_AT = [];
		true -> 
			io:format('error ~n'),
			CardMP_AT = []
	end,
	CardMP_AT.
%===============================================================================
card_at({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			  if 
				  Zone =:= arena_zone -> {ok, CardAT} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, attack);
				  true -> [CardAT] = do(qlc:q([ X#seal_card.attack || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]))
			  end;
		is_not_seal -> CardAT = [];
		true -> 
			io:format('error ~n'),
			CardAT = []
	end,
	CardAT.
%===============================================================================
card_df({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			  if 
				  Zone =:= arena_zone -> {_, CardDF} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, defend);
				  Zone =:= hand_cards; Zone =:= shrine_cards; Zone =:= seal_deck ->
					  [CardDF] = do(qlc:q([ X#seal_card.defend || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]))
			  end;
		is_not_seal -> CardDF = [];
		true -> 
			io:format('error ~n'),
			CardDF = []
	end,
	CardDF.
%===============================================================================
card_sp({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			   if 
				  Zone =:= arena_zone ->
					  {_, Speed} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, speed),
					  CardSpeed =
					  case Speed > 5 of
					   	true -> 5;
							_ -> Speed
						end;
				  Zone =:= hand_cards; Zone =:= shrine_cards; Zone =:= seal_deck ->
					  [CardSpeed] = do(qlc:q([ X#seal_card.speed || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]))
			  end;
		is_not_seal-> CardSpeed = [];
		true -> 
			io:format('error ~n'),
			CardSpeed = []
	end,
	CardSpeed.
%===============================================================================
card_protect_curse({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status1, AbilResult} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, Zone),
			{Status2, SkillResult} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, skill_effect, Zone),
			  if
				  Status1 =:= ok, Status2 =:= ok -> CardProtectCurse = extract_protect_curse(AbilResult++SkillResult);
				  Status1 =:= ok, Status2 =/= ok -> CardProtectCurse = extract_protect_curse(AbilResult);
				  Status1 =/= ok, Status2 =:= ok -> CardProtectCurse = extract_protect_curse(SkillResult);
				  true -> CardProtectCurse = []
			  end;
		is_not_seal -> CardProtectCurse = [];
		true -> 
			io:format('error ~n'),
			CardProtectCurse = []
	end,	
	CardProtectCurse.
%===============================================================================
extract_protect_curse([Receive_effect_Head|Tail]) ->
	%A = [Receive_effect_Head|Tail],
	%io:format('Receive_effect ~p~n',[Receive_effect_Head]),
	extract_protect(Receive_effect_Head)++extract_protect_curse(Tail);
extract_protect_curse([]) -> [].
%====================================
extract_protect(Receive_effect) ->
	if
		Receive_effect =:= [] -> [];
		Receive_effect =/= [] ->
			{ _, ReceiveEffect, _} = Receive_effect,
			%io:format('ReceiveEffect ~p~n',[ReceiveEffect]),
			protect_curse(ReceiveEffect)
	end.
%====================================
protect_curse([ReceiveEffectHead|Tail]) ->
	{Effect, EffectResut} = ReceiveEffectHead,
	if 
		Effect =:= protect_curse -> [EffectResut|protect_curse(Tail)];
		Effect =/= protect_curse -> protect_curse(Tail)
	end;
protect_curse([]) -> [].
%===============================================================================
card_curse({_, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			CardCurse =extract_curse(AllFx);
		is_not_seal -> CardCurse = [];
		true -> 
			io:format('error ~n'),
			CardCurse = []
	end,	
	CardCurse.
%===============================================================================
extract_curse([{ _, ReceiveEffect, _}|Tail]) ->
	%A = [Receive_effect_Head|Tail],
	curse(ReceiveEffect)++extract_curse(Tail);
extract_curse([]) -> [].
%====================================
curse([{curse, CurseType}|Tail]) ->	[CurseType|curse(Tail)];
curse([_|Tail]) -> curse(Tail);
curse([]) -> [].
%===============================================================================
special_curse(Curse, [{CardOwner, CardOrder, CardID}|_]) ->
	AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	get_curse_value(Curse, AllFx).
	
get_curse_value(Curse, [{ _, ReceiveEffect, _}|Tail]) ->
	curse_value(Curse, ReceiveEffect)++get_curse_value(Curse, Tail) ;
get_curse_value(_, []) -> [].

curse_value(Curse, [{curse, {Curse, Value}}|_]) ->	[{Curse, Value}];
curse_value(Curse, [_|Tail]) -> curse_value(Curse, Tail);
curse_value(_, []) -> [].
%=========
%===============================================================================
card_combine({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, combine, Zone),
			if
				Status =:= ok -> CardCombine = Result;
				Status =/= ok -> CardCombine = []
			end;
		is_not_seal -> CardCombine = [];
		true -> 
			io:format('error ~n'),
			CardCombine = []
	end,	
	CardCombine.
%===============================================================================
card_skill_effect({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, skill_effect, Zone),
			if
				Status =:= ok -> Skill_effect = Result;
				Status =/= ok -> Skill_effect = []
			end;											
		is_not_seal -> Skill_effect = [];
		true -> 
			io:format('error ~n'),
			Skill_effect = []
	end,
	Skill_effect.
%===============================================================================
card_ability_effect({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, Zone),
			  if
				  Status =:= ok -> Receive_effect = Result;
				  Status =/= ok -> Receive_effect = []
			  end;
		is_not_seal -> Receive_effect = [];
		true -> 
			io:format('error ~n'),
			Receive_effect = []
	end,	
	Receive_effect.
%===============================================================================
card_mystic_paste({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{Status, Result} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, mystic, Zone),
			  if
				  Status =:= ok -> CardMysticPaste = Result;
				  Status =/= ok -> CardMysticPaste = []
			  end;
		is_not_seal -> CardMysticPaste = [];
		true -> 
			io:format('error ~n'),
			CardMysticPaste = []
	end,	
	CardMysticPaste.
%===============================================================================
card_counter({Zone, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			case Zone of 
				arena_zone ->
					{_, CardCounter} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, charge_counter);
				_ ->
					CardCounter = 0
			end;
		is_not_seal-> CardCounter = [];
		true -> 
			io:format('error ~n'),
			CardCounter = []
	end,
	CardCounter.
%===============================================================================
card_added_skill({_, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			AbilResult = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			CardAddedSkill = extract_added_skill(AbilResult);
		is_not_seal -> 
			CardAddedSkill = [];
		true -> 
			io:format('error ~n'),
			CardAddedSkill = []
	end,	
	CardAddedSkill.
%===============================================================================
extract_added_skill([Receive_effect_Head|Tail]) ->
	%A = [Receive_effect_Head|Tail],
	%io:format('Receive_effect ~p~n',[A]),
	added_skill_extract(Receive_effect_Head)++extract_added_skill(Tail);
extract_added_skill([]) -> [].

added_skill_extract(Receive_effect) ->
	if
		Receive_effect =:= [] -> [];
		Receive_effect =/= [] ->
			{ _, ReceiveEffect, _} = Receive_effect,
			%io:format('ReceiveEffect ~p~n',[ReceiveEffect]),
			added_skill(ReceiveEffect)
	end.

added_skill([ReceiveEffectHead|Tail]) ->
	% card which had added_skill must have {added_skill, {{GiverOwnId, GiverCardOrder, GiverCardID, AddedSkillNo}}} contain in ReceiveEffect
	{Effect, EffectResut} = ReceiveEffectHead,
	if 
		Effect =:= added_skill -> [EffectResut|added_skill(Tail)];
		true -> added_skill(Tail)
	end;
added_skill([]) -> [].
%===============================================================================
card_added_ability({_, {CardOwner, CardOrder, CardID}}) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			AbilResult = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			CardAddedAbility = extract_added_ability(AbilResult);
		is_not_seal -> 
			CardAddedAbility = [];
		true -> 
			io:format('error ~n'),
			CardAddedAbility = []
	end,	
	CardAddedAbility.
%===============================================================================
extract_added_ability([Receive_effect_Head|Tail]) ->
	%A = [Receive_effect_Head|Tail],
	%io:format('Receive_effect ~p~n',[A]),
	added_ability_extract(Receive_effect_Head)++extract_added_ability(Tail);
extract_added_ability([]) -> [].

added_ability_extract(Receive_effect) ->
	if
		Receive_effect =:= [] -> [];
		Receive_effect =/= [] ->
			{ _, ReceiveEffect, _} = Receive_effect,
			%io:format('ReceiveEffect ~p~n',[ReceiveEffect]),
			added_ability(ReceiveEffect)
	end.

added_ability([ReceiveEffectHead|Tail]) ->
	% card which had added_skill must have {added_skill, {{GiverOwnId, GiverCardOrder, GiverCardID, AddedSkillNo}}} contain in ReceiveEffect
	{Effect, EffectResut} = ReceiveEffectHead,
	if 
		Effect =:= added_ability -> [EffectResut|added_ability(Tail)];
		true -> added_ability(Tail)
	end;
added_ability([]) -> [].
%===============================================================================