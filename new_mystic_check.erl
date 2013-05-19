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
-module(new_mystic_check).
-import (mnesia_table, [do/1]).
-include_lib("mystic_ability.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
					check_mystic_ability/1,
					check_casting_condition/3,
					check_remain_id_condition/3,
					check_mystic_ability_target/3,
					check_then_ability_target/3,
					 select_mystic_target/0,
					 generalize_effect_to_each_target/0,
					 check_select_case/1,
					 m_owner_can_select_exact_target/0,
					 post_select_activation/3,
					 post_owner_can_select_exact_target/3,
					 skip_all_ability_no/0,
					 set_effect_to_selected_target/3,
					 verify_mystic_target/3,
					 check_paste_mystic_to_target/3,
					 is_continuous/1,
					 is_continuous/2,
					 check_then_id/1,
					 player_got_effect/3,
					 target_got_effect/3,
					 beyond_target_got_effect/3,
					 check_to_check_condition_id/1,
					 check_id_number/1,
					 check_cancel_priority/2
				]).	
				
check_paste_mystic_to_target(PlayerPid, CardOrder, CardID) ->
	{ok, UsingType} = mnesia_odbc:get_mystic_data(CardID, paste_type),
	%io:format("check_paste_mystic_to_target, Using type ~p~n", [UsingType]),
	case UsingType of
		1 -> check_paste_on_card(PlayerPid, CardOrder, CardID);
		2 -> {ok, paste_on_arena};%check_paste_on_arena (PlayerPid, CardOrder, CardID);
		3 -> check_paste_on_card(PlayerPid, CardOrder, CardID);%check_paste_on_mystic (PlayerPid, CardOrder, CardID);
		4 -> {ok, activate_effect_and_move_to_shrine};
		_ -> io:format("Using type ~p~n", [UsingType])
	end.
	
check_paste_on_card(PlayerPid, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), {target_selected, target}) of
		{ok, [{TPid, TOrder, TID}]} -> verify_seal_ability(PlayerPid, CardOrder, CardID, TPid, TOrder, TID);
		_ -> {ok, no_target_then_move_to_shrine}
	end.
		
verify_seal_ability(PlayerPid, _CardOrder, CardID, TPid, TOrder, TID) ->
	Interest = [{cancel_mystic, [all]}, {cancel_mystic, [all_opponent]}, {cancel_mystic, [opponent_relic]}],
	TargetFx = card_utility:get_all_card_effect(TPid, TOrder, TID),
	%smo_logger:fmsg("Target Effect are ~p~n", [TargetFx]),
	FxContain = function_utility:is_contain(Interest, TargetFx),
	%smo_logger:fmsg("Effect card contain are ~p~n", [FxContain]),
	case FxContain of
		[] -> {ok, mystic_pasted, TPid, TOrder, TID};
		_ ->
			case check_cancel_priority(FxContain, []) of
				[1|_] -> {ok, cancel_mystic, TPid, TOrder, TID};
				[2|_] -> check_cancel_opponent_mystic(PlayerPid, TPid, TOrder, TID);
				[3|_] -> check_cancel_opponent_relic_mystic(PlayerPid, CardID, TPid, TOrder, TID)
			end
		% [{cancel_mystic, [all]}] -> {ok, cancel_mystic, TPid, TOrder, TID};
		% [{cancel_mystic, [all_opponent]}] -> check_cancel_opponent_mystic(PlayerPid, TPid, TOrder, TID);
			% % case TPid of
				% % PlayerPid -> {ok, mystic_pasted, TPid, TOrder, TID};
				% % _ ->	{ok, cancel_mystic, TPid, TOrder, TID}
			% % end;
		% [{cancel_mystic, [opponent_relic]}] -> check_cancel_opponent_relic_mystic(PlayerPid, CardID, TPid, TOrder, TID);
			% % case TPid of
				% % PlayerPid -> {ok, mystic_pasted, TPid, TOrder, TID};
				% % _ ->
					% % Get = mnesia_odbc:get_mystic_data(CardID, card_type),
					% % io:format("---------------------------mystic data get is ~p~n", [Get]),
					% % case Get of
						% % {ok, 2} -> {ok, cancel_mystic, TPid, TOrder, TID};
						% % _ -> {ok, mystic_pasted, TPid, TOrder, TID}
					% % end
			% % end;
		% _A -> smo_logger:fmsg("Card can not cancel mystic case ~p~n", [_A]),
			% case check_cancel_priority([{cancel_mystic, [all]}, {cancel_mystic, [all_opponent]}, {cancel_mystic, [opponent_relic]}], []) of
				% [1|_] -> {ok, cancel_mystic, TPid, TOrder, TID};
				% [2|_] -> check_cancel_opponent_mystic(PlayerPid, TPid, TOrder, TID);
				% [3|_] -> check_cancel_opponent_relic_mystic(PlayerPid, CardID, TPid, TOrder, TID)
			% end
	end.
	
	
check_cancel_opponent_mystic(PlayerPid, TPid, TOrder, TID) ->
	case TPid of
		PlayerPid -> {ok, mystic_pasted, TPid, TOrder, TID};
		_ ->	{ok, cancel_mystic, TPid, TOrder, TID}
	end.
	
check_cancel_opponent_relic_mystic(PlayerPid, CardID, TPid, TOrder, TID) ->
	case TPid of
		PlayerPid -> {ok, mystic_pasted, TPid, TOrder, TID};
		_ ->
			Get = mnesia_odbc:get_mystic_data(CardID, card_type),
			io:format("---------------------------mystic data get is ~p~n", [Get]),
			case Get of
				{ok, 2} -> {ok, cancel_mystic, TPid, TOrder, TID};
				_ -> {ok, mystic_pasted, TPid, TOrder, TID}
			end
	end.

check_cancel_priority([], AllCancel) -> function_utility:qsort(AllCancel);
check_cancel_priority([Cancel|OtherCancel], AllCancel) ->
	case Cancel of
		{cancel_mystic, [all]} -> check_cancel_priority(OtherCancel, [1] ++ AllCancel);
		{cancel_mystic, [all_opponent]} -> check_cancel_priority(OtherCancel, [2] ++ AllCancel);
		{cancel_mystic, [opponent_relic]} -> check_cancel_priority(OtherCancel, [3] ++ AllCancel)
	end.
	
is_continuous(MAbilityID) ->
	do(qlc:q( [X#mystic_ability.continuous_type|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])).

is_continuous(TargetType, MAbilityID) ->
	case TargetType of
		owner -> do(qlc:q( [{X#mystic_ability.continuous_type, X#mystic_ability.fx_owner_got_type}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		player -> do(qlc:q( [{X#mystic_ability.continuous_type, X#mystic_ability.fx_player_got_type}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		this -> do(qlc:q( [{X#mystic_ability.continuous_type, X#mystic_ability.fx_this_got_type}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		target -> do(qlc:q( [{X#mystic_ability.continuous_type, X#mystic_ability.fx_target_got_type}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		beyond -> do(qlc:q( [{X#mystic_ability.continuous_type, X#mystic_ability.fx_beyond_target_got_type}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]))
	end.

check_then_id(MAbilityID) ->
	do(qlc:q( [X#mystic_ability.then_do_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])).
	
check_mystic_ability(CardID) ->
	AbilityList1 = do(qlc:q( [ X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID ])),
	AbilityList =function_utility:qsort(AbilityList1),
	DelDup = function_utility:del(AbilityList),	
	AbilityCount = length(DelDup),
	if
		AbilityCount > 1 -> {need_to_select_one, DelDup};
		AbilityCount =:= 1 -> {do_not_need_to_select, DelDup};
		true -> {error, this_is_not_mystic_or_it_have_no_ability}
	end.
	
check_id_number(MAbilityID) ->
	do(qlc:q( [X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])).
	
check_casting_condition({CardZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) ->
	AllAbilityID = do(qlc:q( [X#mystic_ability.m_ability_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID, X#mystic_ability.m_ability_number =:= AbilityNumber ])),
	RemainAbilityID = check_to_check_condition_id(function_utility:qsort(AllAbilityID)),
	smo_logger:fmsg("remain id to check are ~p~n", [RemainAbilityID]),
	check_remain_id_condition({CardZone, {CardOwner, CardOrder, CardID}}, RemainAbilityID, OpponentPid).
	
check_to_check_condition_id([]) -> [];
check_to_check_condition_id([MAbilityID|Ability]) ->
	ThenDoID = do(qlc:q( [X#mystic_ability.then_do_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	smo_logger:fmsg("this is is ~p and it have 2 pharse effect which is ~p~n", [MAbilityID, ThenDoID]),
	case ThenDoID of
		[n] -> [MAbilityID] ++ check_to_check_condition_id(Ability);
		[null] -> [MAbilityID] ++ check_to_check_condition_id(Ability);
		_ -> [MAbilityID] ++ check_to_check_condition_id(Ability -- ThenDoID)
	end.
	
check_remain_id_condition({_CardZone, {_CardOwner, _CardOrder, _CardID}}, [], _OpponentPid) -> {ok, can_cast};
check_remain_id_condition({CardZone, {CardOwner, CardOrder, CardID}}, [MAbilityID|AbilityID], OpponentPid) -> 
	case check_each_ability_id_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) of
		{ok, condition_match} -> check_remain_id_condition({CardZone, {CardOwner, CardOrder, CardID}}, AbilityID, OpponentPid);
		{ok, condition_mismatch} -> {ok, cannot_cast}
	end.
	
	
check_each_ability_id_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[{SelfCheck, OtherCheck}] = do(qlc:q( [{X#mystic_ability.need_check_self, X#mystic_ability.need_other_check}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID ])),
	check_which_to_check([{self_check, SelfCheck}, {other_check, OtherCheck}], {CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid).
	
check_which_to_check([{self_check, Need}|Tail], {CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	case Need of
		n -> check_which_to_check(Tail, {CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid);
		_ -> 
			case this_card_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) of
				true -> check_which_to_check(Tail, {CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid);
				_ -> {ok, condition_mismatch}
			end
	end;
check_which_to_check([{other_check, Need}|_Tail], {CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	case Need of
		n -> {ok, condition_match};
		_ ->
			case other_card_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) of
				true -> {ok, condition_match};
				_ -> {ok, condition_mismatch}
			end
	end.
%===============================================================================
this_card_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[SelfConditionRequire] = do(qlc:q([X#mystic_ability.self_condition_check|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	check_all_self_require_attribute({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid, SelfConditionRequire).

check_all_self_require_attribute(_, _, _, []) -> true;
check_all_self_require_attribute(AbilOwnCardData, MAbilityID, OpponentPid, [ConditionHead|Tail]) ->
	case check_self:check_each_self_require_attribute(AbilOwnCardData, OpponentPid, ConditionHead, MAbilityID) of
		true -> check_all_self_require_attribute(AbilOwnCardData, MAbilityID, OpponentPid, Tail);
		_ -> self_mismatch
	end.
%===============================================================================	
other_card_condition({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_ability.player_card_check,
																																						  X#mystic_ability.player_card_zone_check, 
																																						  X#mystic_ability.card_tyep_check
																																						 } || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	AllCard = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {CardOwner, OpponentPid}), % Return a list of cards of each player in require zone
	%smo_logger:fmsg("card on <<~p>> of ##~p## are @~p@}", [RequirePresentZone, RequirePlayer, PlayerZoneList1]),
	[ThisInclude] = do(qlc:q([X#mystic_ability.include_this_check || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	CardCheck =
	case ThisInclude of
		n -> AllCard -- [{CardZone, {CardOwner, CardOrder, CardID}}];
		_ -> AllCard
 	end,
	[InitCondition] = do(qlc:q([X#mystic_ability.other_condition_check|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Condition =
	case stack_pool:get_last_stack(self(), call_function) of
		{ok, move_to_arena} -> InitCondition -- [{action, {n, card_casting}}];
		_ -> InitCondition
	end,
	CardMatch = function_utility:card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, CardCheck, Condition),
	[Require] = do(qlc:q([X#mystic_ability.number_of_require || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	attribute_check:check_count(CardMatch, Require).
	
	
check_mystic_ability_target({CardZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) ->
	AllAbilityID = do(qlc:q( [X#mystic_ability.m_ability_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID, X#mystic_ability.m_ability_number =:= AbilityNumber])),
	RemainAbilityID = check_to_check_condition_id(function_utility:qsort(AllAbilityID)),
	OnlyHFx = only_id_have_to_select(RemainAbilityID),
	each_effect_type_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, OnlyHFx).

check_then_ability_target({CardZone, {CardOwner, CardOrder, CardID}}, RemainAbilityID, OpponentPid) ->
	OnlyHFx = only_id_have_to_select(RemainAbilityID),
	each_effect_type_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, OnlyHFx).

only_id_have_to_select([]) -> [];
only_id_have_to_select([MAbilityID|RAbilityID]) ->
	[{OSelect, PSelect, SSelect, TSelect, BSelect}] =
		do(qlc:q([{X#mystic_ability.owner_fx_select,
								X#mystic_ability.player_fx_select,
								X#mystic_ability.this_fx_select,
								X#mystic_ability.target_fx_select,
								X#mystic_ability.beyond_target_fx_select}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	only_target_type_have_effect(MAbilityID, [{owner, OSelect}, {player, PSelect}, {this, SSelect}, {target, TSelect}, {beyond, BSelect}]) ++ only_id_have_to_select(RAbilityID).
	
only_target_type_have_effect(_, []) -> [];
only_target_type_have_effect(MAbilityID, [{_TargetType, {}}|Tail]) -> only_target_type_have_effect(MAbilityID, Tail);
only_target_type_have_effect(MAbilityID, [{TargetType, {SelectType, SelectAmount}}|Tail]) ->	[{TargetType, MAbilityID, {SelectType, SelectAmount}}] ++ only_target_type_have_effect(MAbilityID, Tail).
		
each_effect_type_target({_CardZone, {_CardOwner, _CardOrder, _CardID}}, _OpponentPid, []) -> [];
each_effect_type_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, [{TargetType, MAbilityID, {SelectType, SelectAmount}}|TSelect]) ->
	case TargetType of
		owner -> 
			Target = [CardOwner],
			[{Fx, Duration}] = do(qlc:q( [{X#mystic_ability.fx_to_owner, X#mystic_ability.owner_fx_duration}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		player -> 
			Target = player_got_effect({CardOwner, CardOrder, CardID}, OpponentPid, MAbilityID),
			[{Fx, Duration}] = do(qlc:q( [{X#mystic_ability.fx_to_player, X#mystic_ability.player_fx_duration}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		this -> 
			Target = [{CardOwner, CardOrder, CardID}],
			[{Fx, Duration}] = do(qlc:q( [{X#mystic_ability.fx_to_this, X#mystic_ability.this_fx_duration}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
		target -> 
			Target = target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid),
			[{Fx, Duration}] = do(qlc:q( [{X#mystic_ability.fx_target_receive, X#mystic_ability.target_fx_duration}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]));
 		beyond -> 
			Target = beyond_target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid),
			[{Fx, Duration}] = do(qlc:q( [{X#mystic_ability.fx_beyond_target_receive, X#mystic_ability.beyond_target_fx_duration}|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID]))
	end,
	[{TargetType, MAbilityID, {SelectType, SelectAmount, Target, Fx, Duration}}] ++ each_effect_type_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, TSelect).
	
player_got_effect({CardOwner, CardOrder, CardID}, OpponentPid, MAbilityID) -> 
	[PlayerRequire] = do(qlc:q( [ X#mystic_ability.player_got_fx|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])), 
	{ControlPid, UnconPid, ReqPlayer} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, PlayerRequire),
	case ReqPlayer of
		opponent -> [UnconPid];
		owner -> [ControlPid]
	end.
	
target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_ability.target_card_owner,
																																						  X#mystic_ability.target_card_zone_check, 
																																						  X#mystic_ability.target_card_type
																																						 } || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	AllCard = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {CardOwner, OpponentPid}), % Return a list of cards of each player in require zone
	%smo_logger:fmsg("card on <<~p>> of ##~p## are @~p@}", [RequirePresentZone, RequirePlayer, PlayerZoneList1]),
	[ThisInclude] = do(qlc:q([X#mystic_ability.target_include_this || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	CardCheck =
	case ThisInclude of
		n -> AllCard -- [{CardZone, {CardOwner, CardOrder, CardID}}];
		_ -> AllCard
 	end,
	[InitCondition] = do(qlc:q([X#mystic_ability.target_condition|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Condition =
	case stack_pool:get_last_stack(self(), call_function) of
		{ok, move_to_arena} -> InitCondition -- [{action, {n, card_casting}}];
		_ -> InitCondition
	end,
	% smo_logger:fmsg("call function of ~p~n", [stack_pool:get_last_stack(self(), call_function)]),
	% smo_logger:fmsg("Card condition are ~p~n", [Condition]),
	function_utility:card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, CardCheck, Condition).
	
beyond_target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_ability.beyond_target_card_owner,
																																						  X#mystic_ability.beyond_target_card_zone, 
																																						  X#mystic_ability.beyond_target_card_type
																																						 } || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	AllCard = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {CardOwner, OpponentPid}), % Return a list of cards of each player in require zone
	%smo_logger:fmsg("card on <<~p>> of ##~p## are @~p@}", [RequirePresentZone, RequirePlayer, PlayerZoneList1]),
	[ThisInclude] = do(qlc:q([X#mystic_ability.beyond_target_include_this || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	CardCheck =
	case ThisInclude of
		n -> AllCard -- [{CardZone, {CardOwner, CardOrder, CardID}}];
		_ -> AllCard
 	end,
	[Condition] = do(qlc:q([X#mystic_ability.beyond_target_condition|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	function_utility:card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, CardCheck, Condition).
%------------------------------------------------------------------
% ส่วนของการเลือกเป้่าหมายของ mystic ability
select_mystic_target() ->
	case stack_pool:get_last_stack(self(), all_potential_mystic_ability) of
		{ok, []} -> 
			interfere_step:return_play(check_play_step);
		{ok, RemainID} ->
			{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}} = Last = lists:last(RemainID),
			stack_pool:set_stack_option(self(), this_mystic_ability_id, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}),
			stack_pool:set_stack_option(self(), all_potential_mystic_ability, RemainID--[Last]),
			mystic_precondition(IdFx);
			_ ->
				interfere_step:return_play(check_play_step)
	end.

mystic_precondition(IdFx) ->
	io:format("effect of This Id ~p~n", [IdFx]),
	B3Interest = [{elem, {select, 1}}],
	AllInterest = B3Interest,
	LeftOver = IdFx--AllInterest,
	Interest = IdFx--LeftOver,
	case Interest of 
		[{elem, {select, X}}] ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			stack_pool:set_stack_option(self(), select_element_case, mystic_condition),
			skill_utility:select_element_to_fx(PlayerPid, 0, [1, 2, 3, 4, 5, 6, 7] , X);
		_ -> 
			%{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
			%stack_pool:set_stack_option(self(), this_mystic_ability_id, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}),
			generalize_effect_to_each_target()
	end.

% select_mystic_target() ->
	% case stack_pool:get_last_stack(self(), all_potential_mystic_ability) of
		% {ok, []} -> 
			% interfere_step:return_play(check_play_step);
		% {ok, RemainID} ->
			% {TargetType, MAbilityID, {SelectType, SelectAmount, Target}} = Last = lists:last(RemainID),
			% stack_pool:set_stack_option(self(), this_mystic_ability_id, {TargetType, MAbilityID, {SelectType, SelectAmount, Target}}),
			% stack_pool:set_stack_option(self(), all_potential_mystic_ability, RemainID--[Last]),
			% generalize_effect_to_each_target();
			% _ ->
				% interfere_step:return_play(check_play_step)
	% end.
	
generalize_effect_to_each_target() ->
	case stack_pool:get_last_stack(self(), this_mystic_ability_id) of
		{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}} ->
		check_select_case({TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}});
		_ -> interfere_step:return_play(check_play_step)
	end.
	
check_select_case({TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}) ->
	stack_pool:set_stack_option(self(), this_selecting, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}),
	case SelectType of
		do_not_need_select -> 
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}]),
			select_mystic_target();
		owner_can_select_exact_target -> m_owner_can_select_exact_target();% เลือกว่าจะให้ Ability ทำงานหรือไม่ก่อนแล้วค่อย กลับมาเลือกว่าจะเลือกเป้าเป็นผู้เล่นฝั่งไหน
		player_select_exact_target -> m_player_select_exact_target();% เลือกเป้าของ Ability
		player_select_exact_player -> m_player_select_exact_player();
		controller_select_exact_target -> m_controller_select_exact_target();
		opponent_select_exact_target -> m_opponent_select_exact_target();
		random_select_exact_target -> 
			RandomTarget = function_utility:random_select(SelectAmount, Target),
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, RandomTarget, IdFx, IdDuration}}]),
			select_mystic_target()
		%owner_can_activate_ability -> m_owner_activate_ability(); % เจ้าของการ์ดเลือกว่าจะ ให้ Ability ทำงานหรือไม่
		%controller_can_activate_ability -> m_controller_activate_ability()
	end.
		
% เจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่ ใน กรณีที่ต้องมีการเลือกเป้าด้วย
m_owner_can_select_exact_target() ->
	%DisplayCode = dialog_text:text_code(Effect),
	{ok, {CardOwner, _CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	%self() ! {activate_player_select_activation_ability, CardOwner, CardOwner, CardOrder, CardID, 16#00, DisplayCode}.
	stack_pool:set_stack_option(self(), select_from_function, m_owner_can_select_exact_target),
	case stack_pool:get_last_stack(self(), reselect) of
		{ok, reselect} -> stack_pool:set_stack_option(self(), reselect_target, reselect_mystic_target);
		_ -> stack_pool:set_stack_option(self(), reselect_target, first_select_mystic_target)
	end,
	gen_server:cast(self(), {activate_player_select_activation_ability, CardOwner, CardOwner, CardID, 16#00}). % Original

% หลังจากเจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่ ตรวจสอบว่า เป็นการเลือกมาจาก Function ไหน
post_select_activation(PlayerPid, {Selecter, _CardOwner}, Data) ->
	% PlayerPid = Pid ของ Process ทีส่ง Msg กลับมา
	% Selecter = Pid ของ Player ที่เป็นผู้เลือก
	case stack_pool:get_last_stack(self(), select_from_function) of
		{ok, m_owner_can_select_exact_target} -> post_owner_can_select_exact_target(PlayerPid, Selecter, Data)
		%{ok, controller_activate_ability} -> post_controller_select_activate(PlayerPid, {Selecter, CardOwner}, Data);
		%{ok, owner_activate_ability} -> post_owner_select_activate(PlayerPid, {Selecter, CardOwner}, Data)
	end.
	
% หลังจากเจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่
post_owner_can_select_exact_target(PlayerPid, Selecter, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> skip_all_ability_no();
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> to_m_player_select_exact_target();
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	m_owner_can_select_exact_target()
	end.
	
skip_all_ability_no() ->
	{ok, {_TargetType, MAbilityID, {_SelectType, _SelectAmount, _Target, _IdFx, _IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	{ok, {_CardOwner, _CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	case stack_pool:get_last_stack(self(), all_potential_mystic_ability) of
		{ok, []} -> interfere_step:return_play(check_play_step);%check_ability_effect();
		{ok, AllPoten} -> skip_ability_id(CardID, MAbilityID, AllPoten);
		_NoAllPoten -> select_mystic_target()
	end.

skip_ability_id(_CardID, _MAbilityID, []) -> select_mystic_target();
skip_ability_id(CardID, MAbilityID, [{_TargetType, OtherID, {_SelectType, _SelectAmount, _Target, _IdFx, _IdDuration}}|Ability]) ->
	[MAbilityNo] = do(qlc:q( [ X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID ])),
	AllAbilityId = do(qlc:q( [ X#mystic_ability.m_ability_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID, X#mystic_ability.m_ability_number =:= MAbilityNo])),
	case [OtherID] -- AllAbilityId of
		[] ->
			stack_pool:set_stack_option(self(), all_potential_mystic_ability, Ability),
			stack_pool:set_stack_option(self(), all_potential_mystic_ability_to_skip, Ability),
			skip_ability_id(CardID, MAbilityID, Ability);
		_Other -> 
			smo_logger:fmsg("~n do not skip this ability ~p~n", [_Other]),
			stack_pool:set_stack_option(self(), all_potential_mystic_ability_to_skip, Ability),
			skip_ability_id(CardID, MAbilityID, Ability)
	end.

% กำหนดค่า ให้กับ Ability ใหม่ 
	%จากเดิม player_can_select_exact_target เป็น player_select_exact_target 
	%เพื่อวนกลับไปเลือกเป้าหมายของ Ability
to_m_player_select_exact_target() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	{_TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}} = Fx,
	NewFx = {owner_select_exact_target, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}},
	stack_pool:set_stack_option(self(), this_mystic_ability_id, NewFx),
	generalize_effect_to_each_target().

m_player_select_exact_target() ->
	{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, GetTarget, IdFx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	%{ok, {_CardOwner, _CardOrder, _CardID, _}} = stack_pool:get_last_stack(self()),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	DisplayCode = dialog_text:text_code(IdFx),
	stack_pool:set_stack_option(self(), who_select, PlayerPid),
	case stack_pool:get_last_stack(self(), reselect) of
		{ok, reselect} -> stack_pool:set_stack_option(self(), reselect_target, reselect_mystic_target);
		_ -> stack_pool:set_stack_option(self(), reselect_target, first_select_mystic_target)
	end,
	Target = 
	case TargetType of
		target -> 
			%smo_logger:fmsg("assign target is ~p~n", [stack_pool:get_last_stack(self(), assigned_target)]),
			case stack_pool:get_last_stack(self(), assigned_target) of
				{ok, Assigned} -> %smo_logger:fmsg("assign target is ~p~n", [Assigned]),
					Assigned;
				_ -> GetTarget
			end;
		_ -> GetTarget
	end,
	%smo_logger:fmsg("~nmystic ability id is: ~p,~nSelectAmount: ~p and Target check: ~p", [MAbilityID, SelectAmount, Target]),
	case SelectAmount =:= length(Target) of
		true ->
			stack_pool:set_stack_option(self(), {target_selected, TargetType}, Target),
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}]),
			select_mystic_target();
		_-> %gen_server:cast(self(), {select_ability_target, PlayerPid, CardOwner, CardOrder, CardID, 1, SelectAmount, DisplayCode, Target})
			ClientTarget = get_reply_target(Target, PlayerPid),
			gen_server:cast(self(), {activate_select_mystic_target, PlayerPid, 0, SelectAmount, length(Target), ClientTarget, DisplayCode})
	end.
	
m_player_select_exact_player() ->
	{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, GetTarget, IdFx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	%{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	DisplayCode = dialog_text:text_code(IdFx),
	stack_pool:set_stack_option(self(), who_select, PlayerPid),
	case stack_pool:get_last_stack(self(), reselect) of
		{ok, reselect} -> stack_pool:set_stack_option(self(), reselect_target, reselect_mystic_target);
		_ -> ""%stack_pool:set_stack_option(self(), reselect_target, first_select_mystic_target)
	end,
	Target = 
	case TargetType of
		target -> 
			smo_logger:fmsg("assign target is ~p~n", [stack_pool:get_last_stack(self(), assigned_target)]),
			case stack_pool:get_last_stack(self(), assigned_target) of
				{ok, Assigned} -> smo_logger:fmsg("assign target is ~p~n", [Assigned]),
					Assigned;
				_ -> GetTarget
			end;
		_ -> GetTarget
	end,
	%smo_logger:fmsg("~nmystic ability id is: ~p,~nSelectAmount: ~p and Target check: ~p", [MAbilityID, SelectAmount, Target]),
	case SelectAmount =:= length(Target) of
		true -> 
			stack_pool:set_stack_option(self(), {target_selected, TargetType}, Target),
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}]),
			select_mystic_target();
		_-> %gen_server:cast(self(), {select_ability_target, PlayerPid, CardOwner, CardOrder, CardID, 1, SelectAmount, DisplayCode, Target})
			ClientTarget = get_reply_target(Target, PlayerPid),
			gen_server:cast(self(), {activate_select_mystic_target, PlayerPid, 1, SelectAmount, length(Target), ClientTarget, DisplayCode})
	end.
	
m_controller_select_exact_target() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	{ok, {_TargetType, _MAbilityID, {_SelectType, SelectAmount, Target, _IdFx, _IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	DisplayCode = dialog_text:text_code(_IdFx),
	%{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	stack_pool:set_stack_option(self(), who_select, PlayerPid),
	case stack_pool:get_last_stack(self(), reselect) of
		{ok, reselect} -> stack_pool:set_stack_option(self(), reselect_target, reselect_mystic_target);
		_ -> stack_pool:set_stack_option(self(), reselect_target, first_select_mystic_target)
	end,
	ClientTarget = get_reply_target(Target, PlayerPid),
	gen_server:cast(self(), {activate_select_mystic_target, PlayerPid, 0, SelectAmount, length(Target), ClientTarget, DisplayCode}).
	%gen_server:cast(self(), {select_ability_target, PlayerPid, CardOwner, CardOrder, CardID, 1, SelectAmount, DisplayCode, Target}). % Original
	
m_opponent_select_exact_target() ->
	{ok, OppPid} = stack_pool:get_last_stack(self(), opponent),
	{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
	%smo_logger:fmsg("~nmystic ability id is: ~p,~nSelectAmount: ~p and Target check: ~p", [MAbilityID, SelectAmount, Target]),
	DisplayCode = dialog_text:text_code(IdFx),
	%{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	stack_pool:set_stack_option(self(), who_select, OppPid),
	case stack_pool:get_last_stack(self(), reselect) of
		{ok, reselect} -> stack_pool:set_stack_option(self(), reselect_target, reselect_mystic_target);
		_ -> stack_pool:set_stack_option(self(), reselect_target, first_select_mystic_target)
	end,
	%smo_logger:fmsg("~nmystic ability id is: ~p,~nSelectAmount: ~p and Target check: ~p", [MAbilityID, SelectAmount, Target]),
	case SelectAmount =:= length(Target) of
		true -> 
			stack_pool:set_stack_option(self(), {target_selected, TargetType}, Target),
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}]),
			select_mystic_target();
		_ ->	%gen_server:cast(self(), {select_ability_target, OppPid, CardOwner, CardOrder, CardID, 1, SelectAmount, DisplayCode, Target})
			ClientTarget = get_reply_target(Target, OppPid),
			gen_server:cast(self(), {activate_select_mystic_target, OppPid, 0, SelectAmount, length(Target), ClientTarget, DisplayCode})
	end.
	
% หลังจากเลือกเป้าหมายของ Ability แล้ว นำ Effect มากำหนดให้เป้าหมายที่เลือก
set_effect_to_selected_target(PlayerPid, PlayPlayerPid, Data) ->
	case PlayPlayerPid of
		PlayerPid ->
			{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, _InitTarget, IdFx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
			{ok, Selecter} = stack_pool:get_last_stack(self(), who_select),
			[{Sowner, SOrder, SId}] = function_utility:reverse_data_to_card(Selecter, Data),
			stack_pool:set_stack_option(self(), {target_selected, TargetType}, [{Sowner, SOrder, SId}]),
			stack_pool:add_stack_option_field(self(), selected_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, [{Sowner, SOrder, SId}], IdFx, IdDuration}}]),
			gen_server:cast(self(), {update_select_mystic_target, 1, 0, Sowner, SOrder, SId});
			%select_mystic_target();
		_ -> m_player_select_exact_target()
	end.
	
verify_mystic_target(CardOwner, CardOrder, CardID) ->
	%smo_logger:fmsg("verify_mystic_target ~p~n", [stack_pool:get_last_stack(self(), selected_target)]),
	case stack_pool:get_last_stack(self(), selected_target) of	
		{ok, []} -> interfere_step:return_play(check_play_step);
			%casting_card:reselect_mystic_target(CardOwner, CardOrder, CardID);%interfere_step:return_play(check_play_step);
		{ok, [{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}|Remain]} ->
			%stack_pool:set_stack_option(self(), verify_this_id, {TargetType, MAbilityID, {SelectType, SelectAmount, Target}}),
			stack_pool:set_stack_option(self(), selected_target, Remain),
			verify_mystic_target(CardOwner, CardOrder, CardID, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}});
		_ -> interfere_step:return_play(check_play_step)
	end.
	
verify_mystic_target(CardOwner, CardOrder, CardID, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}) ->
	CardZone = arena_zone,
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	ExactTarget = each_effect_type_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, [{TargetType, MAbilityID, {SelectType, SelectAmount}}]),
	smo_logger:fmsg("target verified ~p~n", [ExactTarget]),
	case verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}, ExactTarget) of
		{ok, no_target} -> 
			cancel_all_mystic_target(CardOwner, CardOrder, CardID, MAbilityID), 
			case mnesia_odbc:get_mystic_data(CardID, paste_type) of
				1 -> 
					%stack_pool:add_stack_option_field(self(), card_destroy, [{CardOwner, CardOrder, CardID}]),
					casting_card:casting_mystic_resume2(CardOwner, CardOrder, CardID);
				3 ->
					%stack_pool:add_stack_option_field(self(), card_destroy, [{CardOwner, CardOrder, CardID}]),
					casting_card:casting_mystic_resume2(CardOwner, CardOrder, CardID);
				_ -> verify_mystic_target(CardOwner, CardOrder, CardID)
			end;
		{ok, have_target} ->	verify_mystic_target(CardOwner, CardOrder, CardID)
	end.
	
cancel_all_mystic_target(_CardOwner, _CardOrder, CardID, MAbilityID) ->
	[MAbilityNo] = do(qlc:q( [ X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID ])),
	AllAbilityId = do(qlc:q( [ X#mystic_ability.m_ability_id|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID, X#mystic_ability.m_ability_number =:= MAbilityNo])),
	%[{TargetType, MAbilityID, {SelectType, SelectAmount, Target, IdFx, IdDuration}}
	case stack_pool:get_last_stack(self(), selected_target) of
		{ok, []} -> do_nothing;%interfere_step:return_play(check_play_step);%check_ability_effect();
		{ok, Select} -> 
			Selected = cancel_all_id(AllAbilityId, Select),
			stack_pool:set_stack_option(self(), selected_target, Selected);
		_ -> do_nothing
	end,
	%[{TargetType, MAbilityID, {SelectType, SelectAmount, LatestTarget, IdFx, IdDuration}}]
	case stack_pool:get_last_stack(self(), result_target) of
		{ok, []} -> do_nothing;%interfere_step:return_play(check_play_step);%check_ability_effect();
		{ok, Result} ->
			Resulted = cancel_all_id(AllAbilityId, Result),
			stack_pool:set_stack_option(self(), result_target, Resulted);
		_ -> do_nothing
	end.

cancel_all_id(_, []) -> [];
cancel_all_id(AllAbilityId, [{_TargetType, OtherID, {_SelectType, _SelectAmount, _Target, _IdFx, _IdDuration}}|Ability]) ->
	case [OtherID] -- AllAbilityId of
		[] -> cancel_all_id(AllAbilityId, Ability);
		_Other -> [{_TargetType, OtherID, {_SelectType, _SelectAmount, _Target, _IdFx, _IdDuration}}] ++  cancel_all_id(AllAbilityId, Ability)
	end.

	
	
verify_mystic_target(_, []) -> 	{ok, have_target};
verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, [{TargetType, MAbilityID, {_SelectType, SelectAmount, LatestTarget, _IdFx, IdDuration}}|Selected]) ->
	case SelectType of
		do_not_need_select -> 
			stack_pool:add_stack_option_field(self(), result_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, LatestTarget, IdFx, IdDuration}}]),
			verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget}}, Selected);
		_ ->
			case LatestTarget of
				[] -> 
					case TargetType of
						target -> stack_pool:set_stack_option(self(), {target_selected, target}, []);
						_ -> ""
					end,
					{ok, no_target};
				_ ->	
					case InitTarget -- LatestTarget of
						[] -> 
							stack_pool:add_stack_option_field(self(), result_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}]),
							verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, Selected);
						_ -> 
							case TargetType of
								target -> stack_pool:set_stack_option(self(), {target_selected, target}, []);
								_ -> ""
							end,
							{ok, no_target}
							%stack_pool:add_stack_option_field(self(), need_reselect_target, [{TargetType, MAbilityID, {SelectType, SelectAmount, LatestTarget, IdFx, IdDuration}}])
					end
			end
	end;
verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, [{_TargetType, MAbilityID, {_SelectType, _SelectAmount, _LatestTarget, _IdFx, _IdDuration}}|Selected]) ->
	verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, Selected ++ [{_TargetType, MAbilityID, {_SelectType, _SelectAmount, _LatestTarget, _IdFx, _IdDuration}}]);
verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, [{_TargetType, _MAbilityID, {_SelectType, _SelectAmount, _LatestTarget, _IdFx, _IdDuration}}|Selected]) ->
	verify_mystic_target({TargetType, MAbilityID, {SelectType, SelectAmount, InitTarget, IdFx, IdDuration}}, Selected ++ [{_TargetType, _MAbilityID, {_SelectType, _SelectAmount, _LatestTarget, _IdFx, _IdDuration}}]).
	
get_reply_target([], _) -> [];
get_reply_target([{PlayerPid, CardOrder, CardID} | T], PlayerPid) ->
	OptionalData = check_card_zone(PlayerPid, CardOrder, CardID),
	[1, CardOrder, <<CardID:16>>] ++ OptionalData ++ get_reply_target(T, PlayerPid);
get_reply_target ([{OwnerPid, CardOrder, CardID} | T], PlayerPid) ->
	OptionalData = check_card_zone(OwnerPid, CardOrder, CardID),
	[0, CardOrder, <<CardID:16>>] ++ OptionalData ++ get_reply_target(T, PlayerPid).
	
check_card_zone(PlayerPid, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
	case CardZone of
		arena_zone -> ZoneInt  = 0;
		shrine_cards -> ZoneInt  = 1;
		seal_deck -> ZoneInt  = 2;
		mystic_deck -> ZoneInt  = 3;
		hand_cards -> ZoneInt  = 4;
		remove_zone -> ZoneInt  = 5;
		Other -> io:format ("Card on ~p out of range~n", [Other]),
				ZoneInt = 99
	end,
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{ok, Line} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, line, CardZone);
		is_not_seal ->
			Line = 0
	end,
	[ZoneInt, Line].
