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
-module(s_ability).
-import (mnesia_table, [do/1]).
%-compile (export_all).
-export([
						ability_start/4,
						condition_check/4
					]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_ability.hrl").
%-include_lib("s_ability_new.hrl").

%===============================================================================
%% swap parameter position 23/04/2008 
%% add independent ability check 24/04/2008
ability_start({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID, PlayStatus) -> % use (CardOrder,{CardID, NoOfAbility}) to check, can ability take an effect under present condition
	case"independ"-- atom_to_list(AbilityId) of% check idependency of ability เช็ค ability ที่ได้รับจากความสามารถของการ์ดใบอื่น
		[] -> %if it independent do
			IndepList = card_list:got_indep_ability(CardID, AbilityId, CardOwner, PlayerOppID),%list of {CardZone, {CardOwner, CardOrder, CardID}
			start_list_effect(IndepList, AbilityId, PlayerOppID, PlayStatus);
		_ -> %else do as usaully
			start_effect({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID, PlayStatus)
	end.
%===============================================================================
start_list_effect([IndepListHead|Tail], AbilityId, PlayerOppID, PlayStatus) -> % start to check and return all ability of whatever got independent ability
	start_effect(IndepListHead, AbilityId, PlayerOppID, PlayStatus)++start_list_effect(Tail, AbilityId, PlayerOppID, PlayStatus);
start_list_effect([], _, _, _) -> [].
%===============================================================================
start_effect({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID, PlayStatus) ->
	Continue = do(qlc:q([X#card_ability.is_until|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	case Continue of
		[y] ->
			case condition_check({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID, PlayStatus) of
				[ok, ok, ok, ok] -> [{{CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID}];%s_ability_effect: s_ability_all_effect(CardData, AbilityId, PlayerOppID);
				_Mismatch -> []%smo_logger:fmsg("mismatch ability id ~p of {~p, ~p, ~p} condtion case ~p~n", [AbilityId, CardOwner, CardOrder, CardID, _Mismatch]), []
			end;
		_ ->
			AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			case function_utility:is_contain([{ability, {loss, [all]}}], AllFx) of
				[] -> 
					case condition_check({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID, PlayStatus) of
						[ok, ok, ok, ok] -> [{{CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID}];%s_ability_effect: s_ability_all_effect(CardData, AbilityId, PlayerOppID);
						_ -> []
					end;
				[{ability, {loss, [all]}}] -> []
			end
	end.
%===============================================================================
%% swap parameter position 23/04/2008 
condition_check({CardZone, {CardOwner, CardOrder,  CardID}}, AbilityId, PlayerOppID, PlayStatus) -> % compare require condition of ability with present condition(this function just return final comparition weather ok or mismatch)
	%io:format("CardData is {~p, {~p, ~p, ~p}}~n", [CardZone, CardOwner, CardOrder,  CardID]),
	% first compare player require condition with player present condition
	% AllFx = card_utility:get_all_card_effect (CardOwner, CardOrder,  CardID),
	% AllInterest = [{loss, skill_or_and_ability}, {loss, ability}],
	% case function_utility:is_contain(AllInterest, AllFx) of
		% [] ->
			[{Owner, Player, Self, Other}] = 
				do(qlc:q( [ {
											X#card_ability.playerown_must_check,
											X#card_ability.player_must_check,
											X#card_ability.owner_must_check,
											X#card_ability.other_must_check} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			check_loop([{1, Owner}, {2, Player}, {3, Self}, {4, Other}], {CardZone, {CardOwner, CardOrder,  CardID}}, PlayerOppID, AbilityId, PlayStatus).
		% [{loss, skill_or_and_ability}|_] -> [];
		% [{loss, ability}|_] -> []
	%end.
	
check_loop([], _, _, _, _) -> [];
check_loop([{What, ToCheck}|Check], {CardZone, {CardOwner, CardOrder,  CardID}}, PlayerOppID, AbilityId, PlayStatus) -> 
	case ToCheck of
		n -> [ok] ++check_loop(Check, {CardZone, {CardOwner, CardOrder,  CardID}}, PlayerOppID, AbilityId, PlayStatus);
		_ ->
			CheckedResult =
			case What of
				1 -> playerown_condition_check(CardOwner, AbilityId);
				2 -> player_condition_check({CardOwner, CardOrder,  CardID}, PlayerOppID, AbilityId);
				3 -> self_condition_check({CardZone ,{CardOwner, CardOrder, CardID}}, PlayerOppID, AbilityId, PlayStatus);
				4 -> list_condition_check({CardZone ,{CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID)
			end,
			case CheckedResult of
				[ok] -> CheckedResult ++ check_loop(Check, {CardZone, {CardOwner, CardOrder,  CardID}}, PlayerOppID, AbilityId, PlayStatus);
				Any -> Any
			end
	end.
%===============================================================================
playerown_condition_check(CardOwner, AbilityId) ->
	[PlayerOwnCondition] =  do(qlc:q([X#card_ability.playerown_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	check_all_playerown_require_attribute(CardOwner, PlayerOwnCondition).
%===============================================================================
check_all_playerown_require_attribute(CardOwner, [ConditionHead|Tail]) ->
	case check_each_playerown_require_attribute(CardOwner, ConditionHead) of
		true -> check_all_playerown_require_attribute(CardOwner, Tail);
		Any -> [Any]
	end;
check_all_playerown_require_attribute(_, []) -> [ok].
%===============================================================================
check_each_playerown_require_attribute(CardOwner, {Attribute, Value}) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(CardOwner), Value);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(), Value);
		mp -> attribute_check:player_mp_check(game_info:player_mp(CardOwner), Value);
		action -> attribute_check:player_action_check(game_info:player_action(CardOwner), Value)
	end.
%===============================================================================
%===============================================================================
player_condition_check({CardOwner, CardOrder,  CardID}, PlayerOppID, AbilityId) -> 
	[{PlayerCondition, RequirePlayer}] =  do(qlc:q([{X#card_ability.player_check, 
																											X#card_ability.player_side_check
																										  }|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	{ControlPid, UnconPid, ReqPlayer} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, PlayerOppID, RequirePlayer),
	check_all_player_require_attribute(ControlPid, UnconPid, PlayerCondition, ReqPlayer).
%===============================================================================
check_all_player_require_attribute(ControlPid, UnconPid, [ConditionHead|Tail], RequirePlayer) ->
	case check_each_player_require_attribute(ControlPid, UnconPid, ConditionHead, RequirePlayer) of
		true -> check_all_player_require_attribute(ControlPid, UnconPid, Tail, RequirePlayer);
		Any -> [Any]
	end;
check_all_player_require_attribute(_, _, [], _) -> [ok].
%===============================================================================
check_each_player_require_attribute(PlayerPid, PlayerOppPid, {Attribute, Value}, RequirePlayer) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(PlayerPid), game_info:player_turn(PlayerOppPid), Value, RequirePlayer);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(), Value);
		mp -> attribute_check:player_mp_check(game_info:player_mp(PlayerPid), game_info:player_mp(PlayerOppPid), Value, RequirePlayer);
		action -> attribute_check:player_action_check(game_info:player_action(PlayerPid), game_info:player_action(PlayerOppPid), Value, RequirePlayer)
	end.
%===============================================================================
%===============================================================================
self_condition_check({Zone ,{CardOwner, CardOrder, CardID}}, PlayerOppID, AbilityId, PlayStatus) ->
	[SelfConditionRequire] = do(qlc:q([X#card_ability.owner_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	check_all_self_require_attribute({Zone ,{CardOwner, CardOrder, CardID}}, PlayerOppID, SelfConditionRequire, AbilityId, PlayStatus).
%===============================================================================
check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, [ConditionHead|Tail], AbilityId, PlayStatus) ->
	case check_self:check_each_self_require_attribute(AbilOwnCardData, PlayerOppID, ConditionHead, AbilityId, PlayStatus) of
		true -> check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, Tail, AbilityId, PlayStatus);
		Any -> [Any]
	end;
check_all_self_require_attribute(_, _, [], _, _) -> [ok].
%===============================================================================
list_condition_check({CardZone, {CardOwner, AbilOwnCardOrder, CardID }}, AbilityId, PlayerOppID) ->
	[{RequirePlayer, ReqZone}] = do(qlc:q([{X#card_ability.other_player_check,
																							 X#card_ability.other_present_check
																						   }|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({CardOwner, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	PlayerZoneList1 = card_list:player_zone_list({ReqPlayer, ReqZone}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#card_ability.other_self_include_check || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{CardZone, {CardOwner, AbilOwnCardOrder, CardID}}];
		true -> PlayerZoneList = PlayerZoneList1
	end,
	[ConditionNeed] = do(qlc:q([X#card_ability.other_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	[MatchCount] = do(qlc:q([X#card_ability.other_match_count || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	check_all({CardZone, {CardOwner, AbilOwnCardOrder, CardID}}, ConditionNeed, PlayerZoneList, MatchCount).
	
	%case MatchCount of
		%more_than_owner ->
			%OpponentCount = length(card_utility:get_all_card(PlayerOppID, seal_card, arena_zone)),
			%OwnerCount = length(card_utility:get_all_card(CardOwner, seal_card, arena_zone)),
			%if 
				%OpponentCount > OwnerCount -> ok;
				%OpponentCount =< OwnerCount -> less_opponent_seal
			%end;
		%less_than_owner ->
			%OpponentCount = length(card_utility:get_all_card(PlayerOppID, seal_card, arena_zone)),
			%OwnerCount = length(card_utility:get_all_card(CardOwner, seal_card, arena_zone)),
			%if 
				%OpponentCount < OwnerCount -> ok;
				%OpponentCount >= OwnerCount -> more_opponent_seal
			%end;
		%_ ->
			%case attribute_check:check_count(CheckedAll, MatchCount) of
				%true -> ok;
				%Any -> Any
			%end.
	%end.
%===============================================================================
check_all(_, _, [], MatchCount) -> 
	Card_Match = stack_pool:get_last_stack(self(), card_match_ability),
	 case MatchCount of	
		 {less_than, X} -> 
			 case Card_Match of
				 {ok, []} -> [ok];
				 {ok, AllMatch} -> 
				 	case length(AllMatch) < X of
						true -> [ok];
						_ -> stack_pool:remove_stack_option(self(), card_match_ability), [card_match_over_requirement]
					end;
				 _ -> [ok]
			 end;				
		 {equal_to , X} -> 
			 case Card_Match of
				 {ok, AllMatch} ->
				 	case length(AllMatch) =:= X of
						true -> [ok];
						_ -> stack_pool:remove_stack_option(self(), card_match_ability), [card_match_not_equal_to_requirement]
					end;
				 _ -> 
				 	case X of
						0 -> [ok];
						_ -> [card_match_not_equal_to_requirement]
					end
			 end;		
		 _ -> 		 
			case Card_Match of
				{ok, _ } -> stack_pool:remove_stack_option(self(), card_match_ability), [card_require_not_enough];
				_ -> [card_require_not_enough]
			end
	end;

check_all({CardZone, {AbilOwnCardOwner, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, [PlayerZoneListHead|Tail], MatchCount) ->
	CardMatch =  check_other:check_all_other_require_attribute({CardZone, {AbilOwnCardOwner ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed),
	stack_pool:add_stack_option_field(self(), card_match_ability, CardMatch),
	{ok, AllCardMatch} = stack_pool:get_last_stack(self(), card_match_ability),
	%smo_logger:fmsg("all card match condtion are ~p~n", [AllCardMatch]),
	case attribute_check:check_count(AllCardMatch, MatchCount) of
		true -> 
			case MatchCount of
				 {less_than, _} ->
				 	check_all({CardZone, {AbilOwnCardOwner, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount);
				 {equal_to, _} ->
				 	check_all({CardZone, {AbilOwnCardOwner, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount);
				_->
					stack_pool:remove_stack_option(self(), card_match_ability), 
					[ok]
			end;
		_ -> check_all({CardZone, {AbilOwnCardOwner, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount)
	end.
%===============================================================================