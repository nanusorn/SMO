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
-module(s_ability_target).
-import (mnesia_table, [do/1]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
%-include_lib("s_ability_new.hrl").
-include_lib("s_ability.hrl").
%===============================================================================
playerown_target_check(AbilityId) -> 
	[Player1Effect ]= do(qlc:q( [ X#card_ability.playerown_have_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		Player1Effect =/= y -> [];
		Player1Effect =:= y -> 1
	end.
%===============================================================================
playeropp_target_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, AbilityId) -> 
	[Player2Effect] = do(qlc:q( [ X#card_ability.player_have_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		Player2Effect =/= y -> PlayerPid = [];
		true -> 
			[Player] = do(qlc:q([X#card_ability.player_which_got_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
			{ControlPid, UnconPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, Player),
			case ReqPlayer of
				opponent ->	PlayerPid = UnconPid;
				owner -> PlayerPid = ControlPid
			end
	end,
	PlayerPid.
%===============================================================================
owner_target_check(AbilityId) ->
	[OwnerEffect] = do(qlc:q( [ X#card_ability.owner_have_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		OwnerEffect =/= y -> [];
		OwnerEffect =:= y -> 1
	end.
%===============================================================================
% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
other_target_check({Zone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID) ->	
	[OtherTargetHaveEffect] = do(qlc:q( [ X#card_ability.other_target_have_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		OtherTargetHaveEffect =:= n -> [];
		true -> 
			%[IsUntil] = do(qlc:q( [ X#card_ability.is_until|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			%io:format('SIsUntil ~p~n',[IsUntil]),
			%if 
				%IsUntil =:= y -> [];
					  %io:format('Check by Requirement ~n'),
					  %requirement:card_requirement(AbilityId);
				%IsUntil =/= y ->
					list_target_check(Zone, {PlayerOwnID, CardOrder, CardID}, AbilityId, PlayerOppID)
			%end
	end.
	
other_targets_check({PlayerOwnID, CardOrder, CardID}, AbilityId, PlayerOppID) ->	
	[OtherTargetHaveEffect] = do(qlc:q( [ X#card_ability.other_target_have_effect|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		OtherTargetHaveEffect =:= n -> [];
		true ->
			Zone = card_utility:check_card_zone(PlayerOwnID, CardOrder, CardID),
			list_target_check(Zone, {PlayerOwnID, CardOrder, CardID}, AbilityId, PlayerOppID)
	end.
%===============================================================================
list_target_check(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityId, PlayerOppID) ->
	%io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, AbilityId, PlayerOppID]),
	[{RequirePlayer, ReqZone}] = do(qlc:q([{X#card_ability.target_player_check,
																							 X#card_ability.target_present_check
																							} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),       
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = card_list:player_zone_list({ReqPlayer, ReqZone}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#card_ability.target_self_include_check || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	case SelfInclude of
		n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		_ -> PlayerZoneList = PlayerZoneList1
	end,
	CheckAllResult = check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, AbilityId, PlayerZoneList),
	%io:format('CheckAllResult ~p~n',[CheckAllResult]),
	CheckAllResult.
%===============================================================================
check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, AbilityId, [PlayerZoneListHead|Tail]) ->
	[ConditionNeed] = do(qlc:q([X#card_ability.target_check || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	%io:format('ConditionNeed ~p~n',[ConditionNeed]),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed)++check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, AbilityId, Tail);
check_all(_, _, []) -> [].
%===============================================================================
ability_number_target_check(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityNumber, PlayerOppID) ->
	AbilityIdList = do(qlc:q( [ X#card_ability.ability_id|| X <- mnesia:table(card_ability), X#card_ability.ability_no =:= AbilityNumber ])),
	function_utility:del(function_utility:qsort(list_target_check_loop(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityIdList, PlayerOppID))).

list_target_check_loop(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, [AbilityId|Tail], PlayerOppID) ->
	list_target_check(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityId, PlayerOppID)++list_target_check_loop(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, Tail, PlayerOppID);
list_target_check_loop(_, _, [], _) -> [].
%===============================================================================
% remove_ability_target(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityId, PlayerOppID) ->
	% %io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, AbilityId, PlayerOppID]),
	% [{RequirePlayer, ReqZone}] = do(qlc:q([{X#card_ability.target_player_check,
																							 % X#card_ability.target_present_check
																							% } || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	% CheckArena = [arena_zone] -- ReqZone,
	% RequireZone = 
	% case CheckArena of
		% [] -> ReqZone ++ [support_cards];
		% _ -> ReqZone
	% end,
																																												% 
	% {PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	% % retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	% PlayerZoneList1 = card_list:player_zone_list({ReqPlayer, RequireZone}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	% [SelfInclude] = do(qlc:q([X#card_ability.target_self_include_check || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	% case SelfInclude of
		% n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		% _ -> PlayerZoneList = PlayerZoneList1
	% end,
	% CheckAllResult = check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, AbilityId, PlayerZoneList),
	% %io:format('CheckAllResult ~p~n',[CheckAllResult]),
	% CheckAllResult.