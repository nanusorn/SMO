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
-module(update_ability).
%-compile(export_all).
-export([update/3, retrieve_deck_ability/2]).
-import (mnesia_table, [do/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_ability.hrl").

retrieve_deck_ability(PlayerPid, OppPid) ->
	{ok, OwnSdeck} = mnesia_play:get_player_data(PlayerPid, seal_deck),
	{ok, OwnMdeck} = mnesia_play:get_player_data(PlayerPid, mystic_deck),
	{ok, OwnHand} = mnesia_play:get_player_data(PlayerPid, hand_cards),
	{ok, OppSdeck} = mnesia_play:get_player_data(OppPid, seal_deck),
	{ok, OppMdeck} = mnesia_play:get_player_data(OppPid, mystic_deck),
	{ok, OppHand} = mnesia_play:get_player_data(OppPid, hand_cards),
	AllDeck = OwnSdeck ++ OwnMdeck ++ OppSdeck ++ OppMdeck ++ OwnHand ++ OppHand,
	All = function_utility:exclude_option(AllDeck),
	card_with_match_ability(All).

card_with_match_ability([]) -> [];
card_with_match_ability([{CardOwner, CardOrder, CardID}|Card]) ->
	check_card_ability_match({CardOwner, CardOrder, CardID}) ++ card_with_match_ability(Card).

update(PlayStatus, PlayerPid, OppPid) ->
	%io:format('PlayStatus ~p~n',[PlayStatus]),
	CardList = card_list:player_zone_list({null, [null]}, {PlayerPid, OppPid}),
	DeckAbi = deck_ability_card(),
	case PlayStatus of
		into_library ->
			io:format("card abiltiy poten ~p~n", [CardList ++ DeckAbi]);
		_ -> ""
	end,
	each_play_status_effect(PlayStatus, PlayerPid, OppPid, CardList ++ DeckAbi).
	
deck_ability_card() ->
	{ok, DeckAbi} = mnesia_play:get_game_data(self(), deck_ability_card),
	extend_zone(DeckAbi).

extend_zone([]) -> [];
extend_zone([{CardOwner, CardOrder, CardID}|DeckAbi]) ->
	Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	case Zone of
		seal_deck -> [{Zone, {CardOwner, CardOrder, CardID}}] ++ extend_zone(DeckAbi);
		mystic_deck -> [{Zone, {CardOwner, CardOrder, CardID}}] ++ extend_zone(DeckAbi);
		_ -> extend_zone(DeckAbi)
	end.
%---------------------------------------------------------------------
each_play_status_effect(PlayStatus, PlayerPid, OppPid, CardList) ->
	AbilityListMatch = check_ability_match(PlayStatus, CardList),
	%io:format('card with potential with PlayStatus ~p are ~p:~n', [PlayStatus, AbilityListMatch]),
	ExtractList = list_operate(PlayerPid, OppPid, AbilityListMatch),
	%io:format('card with potential with PlayStatus ~p are ~p:~n', [PlayStatus, ExtractList]),
	do_it(ExtractList, PlayStatus).
%===============================================================================
do_it([{CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, OppPid|Tail], PlayStatus) ->
	s_ability:ability_start({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, OppPid, PlayStatus)++do_it(Tail, PlayStatus);
do_it([], _) -> [].
%===============================================================================	
check_ability_match(PlayStatus, [CardListHead|Tail]) ->
	check_card_ability_match(PlayStatus, CardListHead)++check_ability_match(PlayStatus, Tail);
check_ability_match(_, []) -> [].
%===============================================================================
check_card_ability_match({CardHolderID, CardOrder, CardID}) ->
	AbilityIdList = do(qlc:q([ X#card_ability.ability_id || X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
	ability_match({CardHolderID, CardOrder, CardID}, AbilityIdList).
	
check_card_ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}) ->

	% เช็คในส่วนของ การ์ดที่ส่งผลเมื่อจบ Subturn ในกรณีที่การ์ดไ้ด้รับผลมาอยู่แล้ว
	case PlayStatus of
		refresh_end_subturn ->
%			io:format ("Reflesh status ~n"),
			RefleshStatus = 		
			case CardZone of
				arena_zone ->
					stack_pool:add_stack_option_field (self(), on_arena_cards, [{CardHolderID, CardOrder, CardID}]),
					[combined, assign_line, cast_successful, attack_successful, break_combined, had_use_skill];
				_ ->	[]
			end,
			case CardZone of
				seal_deck -> not_process;
				mystic_deck -> not_process;
				_ ->	end_of_subturn:check_end_of_subturn_effect(CardHolderID, CardOrder, CardID, CardZone, RefleshStatus)
			end,
			[];
			
		_ ->	
			AbilityIdList = do(qlc:q([ X#card_ability.ability_id || X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
			ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}, AbilityIdList)
	end.
%===============================================================================
ability_match(_, []) -> [];
ability_match({CardHolderID, CardOrder, CardID}, [AbilityListHead|Tail]) ->
	return_ability_match({CardHolderID, CardOrder, CardID}, AbilityListHead)++ability_match({CardHolderID, CardOrder, CardID}, Tail).
			
ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}, [AbilityListHead|Tail]) ->
	return_ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}, AbilityListHead)++ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}, Tail);
ability_match(_, _, []) -> [].
%===============================================================================	
return_ability_match({CardOwner, CardOrder, CardID}, AbilityId) ->
	QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_present_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	if 
		QueryReturn =:= [] -> [];
		QueryReturn =/= [] ->	[{SelfCheck, OtherZoneCheck}] = QueryReturn,
			SelfSDeck = [{zone, [seal_deck]}]--SelfCheck,
			OtherSDeck = [seal_deck]--OtherZoneCheck,
			SelfMDeck = [{zone, [mystic_deck]}]--SelfCheck,
			OtherMDeck = [mystic_deck]--OtherZoneCheck,
			SelfSMDeck = [{zone, [seal_deck, mystic_deck]}]--SelfCheck,
			OtherSMDeck = [seal_deck, mystic_deck]--OtherZoneCheck,
			if
				SelfSDeck =:= []; OtherSDeck =:= []; SelfMDeck =:= []; OtherMDeck =:= []; SelfSMDeck =:= []; OtherSMDeck =:= [] -> [{CardOwner, CardOrder, CardID}];
				true -> []
			end
	end.
	
return_ability_match(PlayStatus, {CardZone,{CardHolderID, CardOrder, CardID}}, AbilityId) ->
	case PlayStatus of
		attacker_fight ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn, 
					SelfFight = [{action, fight}]--SelfCheck,
					OtherFight = [{action, fight}]--OtherCheck,
					  if
						  SelfFight =:= []; OtherFight =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
		
		attacked_fight ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn, 
					SelfFight = [{action, fight}]--SelfCheck,
					OtherFight = [{action, fight}]--OtherCheck,
					  if
						  SelfFight =:= []; OtherFight =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		seal_attacker ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn, 
					SelfAttacker = [{action, attacker}]--SelfCheck,
					OtherAttacker = [{action, attacker}]--OtherCheck,
					SelfAttacked = [{action, attacked}]--SelfCheck,
					  if
						  SelfAttacker =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  OtherAttacker =:= [], SelfAttacked =/= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		seal_attacked ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn, 
					SelfAttacked = [{action, attacked}]--SelfCheck,
					OtherAttacked = [{action, attacked}]--OtherCheck,
					SelfAttacker = [{action, attacker}]--SelfCheck,
					  if
						  SelfAttacked =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  OtherAttacked =:= [], SelfAttacker =/= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		attack_success ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn,
				SelfAttackSuccess = [{action, attack_success}]--SelfCheck,
				OtherAttackSuccess = [{action, attack_success}]--OtherCheck,
					  if 
						  SelfAttackSuccess =:= []; OtherAttackSuccess =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		been_attack_success -> [];
		
		hand_attack_success ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn,
				SelfHandAttackSuccess = [{action, hand_attack_success}]--SelfCheck,
				OtherHandAttackSuccess = [{action, hand_attack_success}]--OtherCheck,
					  if 
						  SelfHandAttackSuccess =:= []; OtherHandAttackSuccess =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		end_of_fighting -> 
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{SelfCheck, OtherCheck}] = QueryReturn,
				SelfAttackSuccess = [{action, end_of_fighting}]--SelfCheck,
				OtherAttackSuccess = [{action, end_of_fighting}]--OtherCheck,
					  if 
						  SelfAttackSuccess =:= []; OtherAttackSuccess =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		player_cast ->
			QueryReturn = do(qlc:q( [{X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, SomePlayerCheck}] = QueryReturn,
				OwnPlayerAssignCast = [{action, assign_cast}]--OwnPlayerCheck ,
				SomePlayerAssignCast = [{action, assign_cast}]--SomePlayerCheck,
					  if
						  OwnPlayerAssignCast =:= []; SomePlayerAssignCast =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					  end
			end;
			
		player_attack -> %change
			QueryReturn = do(qlc:q( [{X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, SomePlayerCheck}] = QueryReturn,
				OwnPlayerAssignAttack = [{action, assign_attack}]--OwnPlayerCheck,
				SomePlayerAssignAttack = [{action, assign_attack}]--SomePlayerCheck,
					  if
							OwnPlayerAssignAttack =:= []; SomePlayerAssignAttack =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
							true -> []
					  end
			end;
		
		on_hand ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_present_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->	[{SelfCheck, OtherZoneCheck}] = QueryReturn,
					SelfHand = [{zone, [hand_cards]}]--SelfCheck,
					OtherHand = [hand_cards]--OtherZoneCheck,
					if
						SelfHand =:= []; OtherHand =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
		into_arena ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfIntoArena = [{action, into_arena}]--SelfCheck,
					OtherIntoArena = [{action, into_arena}]--OtherCheck,
					SelfCast = [{action, casting}]--SelfCheck,
					OtherCast = [{action, casting}]--OtherCheck,
					SelfMoveAre = [{action, move_to_arena}]--SelfCheck,
					OtherMoveAre = [{action, move_to_arena}]--OtherCheck,
					if 
						SelfIntoArena =:= []; OtherIntoArena =:= []; SelfCast =:= []; OtherCast =:= []; SelfMoveAre =:= []; OtherMoveAre =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		growth_into_arena ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfIntoArena = [{action, growth_into_arena}]--SelfCheck,
					OtherIntoArena = [{action, growth_into_arena}]--OtherCheck,
					SelfCast = [{action, casting}]--SelfCheck,
					OtherCast = [{action, casting}]--OtherCheck,
					SelfMoveAre = [{action, move_to_arena}]--SelfCheck,
					OtherMoveAre = [{action, move_to_arena}]--OtherCheck,
					if 
						SelfIntoArena =:= []; OtherIntoArena =:= []; SelfCast =:= []; OtherCast =:= []; SelfMoveAre =:= []; OtherMoveAre =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		on_arena ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfCastSuccess = [{action, on_arena}]--SelfCheck,
					OtherCastSuccess = [{action, on_arena}]--OtherCheck,
					if 
						SelfCastSuccess =:= []; OtherCastSuccess =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		cast_success ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfCastSuccess = [{action, cast_success}]--SelfCheck,
					OtherCastSuccess = [{action, cast_success}]--OtherCheck,
					if 
						SelfCastSuccess =:= []; OtherCastSuccess =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		use_skill ->
			QueryReturn = do(qlc:q( [{ X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}]= QueryReturn,
					SelfSkill = [{action, using_skill}]--SelfCheck,
					OtherSkill = [{action, using_skill}]--OtherCheck,
					if 
						  SelfSkill =:= []; OtherSkill =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					end
			end;
			
		use_skill_success ->
			QueryReturn = do(qlc:q( [{ X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}]= QueryReturn,
					SelfSkill = [{action, use_skill_success}]--SelfCheck,
					OtherSkill = [{action, use_skill_success}]--OtherCheck,
					if 
						  SelfSkill =:= []; OtherSkill =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					end
			end;
			
		into_any_subturn ->
			QueryReturn = do(qlc:q( [{ X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, OtherPlayerCheck}] = QueryReturn,
					OwnPlayerAt = [{turn, any}, {pharse, checkup}]--OwnPlayerCheck,
					OtherPlayerAt = [{turn, any}, {pharse, checkup}]--OtherPlayerCheck,			
					if 
						OwnPlayerAt =:= []; OtherPlayerAt =:= [] ->
							ChangeTurn = [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> 
							ChangeTurn = []
					end,
					ChangeTurn
			end;
		into_at_subturn ->
			QueryReturn = do(qlc:q( [{ X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, OtherPlayerCheck}] = QueryReturn,
					OwnPlayerAt = [{turn, at}, {pharse, checkup}]--OwnPlayerCheck,
					OtherPlayerAt = [{turn, at}, {pharse, checkup}]--OtherPlayerCheck,			
					if 
						OwnPlayerAt =:= []; OtherPlayerAt =:= [] ->
							ChangeTurn = [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> 
							ChangeTurn = []
					end,
					ChangeTurn
			end;
		into_df_subturn ->
			QueryReturn = do(qlc:q( [{ X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, OtherPlayerCheck}] = QueryReturn,
					OwnPlayerDf = [{turn, df}, {pharse, checkup}]--OwnPlayerCheck,
					OtherPlayerDf = [{turn, df}, {pharse, checkup}]--OtherPlayerCheck,					
					if 
						OwnPlayerDf =:= []; OtherPlayerDf =:= [] ->
							ChangeTurn = [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> 
							ChangeTurn = []
					end,
					ChangeTurn
			end;
		
		end_at_subturn ->
			%QueryReturn = do(qlc:q( [{ X#card_ability.playerown_check, X#card_ability.player_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			[QueryReturn1] = do(qlc:q( [ X#card_ability.playerown_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			[QueryReturn2] = do(qlc:q( [ X#card_ability.player_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			%[PlayerSide] = do(qlc:q( [X#card_ability.player_side_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			OwnPlayerTurn = [{turn, at}, {pharse, eos}]--QueryReturn1,
			PlayerTurn = [{turn, at}, {pharse, eos}]--QueryReturn2,
			if
				OwnPlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				%PlayerTurn =:= [], PlayerSide =:= controller -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				PlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				true -> []
			end;
			
		end_df_subturn ->
			[QueryReturn1] = do(qlc:q( [ X#card_ability.playerown_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			[QueryReturn2] = do(qlc:q( [ X#card_ability.player_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			%[PlayerSide] = do(qlc:q( [X#card_ability.player_side_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			OwnPlayerTurn = [{turn, df}, {pharse, eos}]--QueryReturn1,
			PlayerTurn = [{turn, df}, {pharse, eos}]--QueryReturn2,
			if
				OwnPlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				%PlayerTurn =:= [], PlayerSide =:= controller -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				PlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
				true -> []
			end;
			
		end_any_at_subturn ->
			[QueryReturn] = do(qlc:q( [X#card_ability.playerown_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			case	QueryReturn of
				[] -> [];
				OwnPlayerCheck ->
					OwnPlayerTurn = [{turn, any}, {pharse, eos}]--OwnPlayerCheck,
					if 					
						 OwnPlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];					
						 true -> []
					end
			end;
			
		end_any_df_subturn ->
			[QueryReturn] = do(qlc:q( [X#card_ability.player_check|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			case	QueryReturn of
				[] -> [];
				OtherPlayerCheck ->
					OtherPlayerTurn = [{turn, any}, {pharse, eos}]--OtherPlayerCheck,					
					if 					
						 OtherPlayerTurn =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];					
						 true -> []
					end
			end;
			
		destroyed ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfDestroy = [{action, destroyed}]--SelfCheck,
					OtherDestroy = [{action, destroyed}]--OtherCheck,
					if 
						 SelfDestroy =:= []; OtherDestroy =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						 true -> []
					end
			end;
		
		into_shrine ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfShrine = [{action, into_shrine}]--SelfCheck,
					OtherShrine = [{action, into_shrine}]--OtherCheck,
					if 
						 SelfShrine =:= []; OtherShrine =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						 true -> []
					end
			end;
			
		in_shrine ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfShrine = [{action, in_shrine}]--SelfCheck,
					OtherShrine = [{action, in_shrine}]--OtherCheck,
					if 
						 SelfShrine =:= []; OtherShrine =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						 true -> []
					end
			end;
		
		remove_from_arena -> [];
		
		changing_line -> [];
		
		change_line ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}]= QueryReturn,
					SelfLine = [{action, changed_line}]--SelfCheck,
					OtherLine = [{action, changed_line}]--OtherCheck,
					if 
						  SelfLine =:= []; OtherLine =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						  true -> []
					end
			end;
			
		being_combine -> 
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfConbine = [{action, being_combine}]--SelfCheck,
					OtherConbine = [{action, being_combine}]--OtherCheck,
					if 
						SelfConbine =:= []; OtherConbine =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		
		combine_success ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfConbine = [{action, combine_success}]--SelfCheck,
					OtherConbine = [{action, combine_success}]--OtherCheck,
					if 
						SelfConbine =:= []; OtherConbine =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		breaking_combine -> [];
		
		break_combine_success -> [];
			
		leave_play ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfLeave = [{action, leave_play}]--SelfCheck,
					OtherLeave = [{action, leave_play}]--OtherCheck,
					if 
						SelfLeave =:= []; OtherLeave =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		destroy ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfDestroy = [{action, destroy}]--SelfCheck,
					OtherDestroy = [{action, destroy}]--OtherCheck,
					if 
						SelfDestroy =:= []; OtherDestroy =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		growthing ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfGrowth = [{action, growthing}]--SelfCheck,
					OtherGrowth = [{action, growhting}]--OtherCheck,
					if 
						SelfGrowth =:= []; OtherGrowth =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		into_remove ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfRemove = [{action, into_remove}]--SelfCheck,
					OtherRemove = [{action, into_remove}]--OtherCheck,
					if 
						SelfRemove =:= []; OtherRemove =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		in_remove ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfRemove = [{action, in_remove}]--SelfCheck,
					OtherRemove = [{action, in_remove}]--OtherCheck,
					if 
						SelfRemove =:= []; OtherRemove =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
		
		into_library ->
			QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if 
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] ->
					[{SelfCheck, OtherCheck}] = QueryReturn,
					SelfRemove = [{action, into_library}]--SelfCheck,
					OtherRemove = [{action, into_library}]--OtherCheck,
					if 
						SelfRemove =:= []; OtherRemove =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
						true -> []
					end
			end;
			
		assign_attack ->
			QueryReturn = do(qlc:q( [{ X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, SomePlayerCheck}] = QueryReturn,
				OwnPlayerAssignAttack = [{action, assign_attack}]--OwnPlayerCheck,
				SomePlayerAssignAttack = [{action, assign_attack}]--SomePlayerCheck,
					  if
							OwnPlayerAssignAttack =:= []; SomePlayerAssignAttack =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
							true -> []
					  end
			end;
		% moving_to_hand ->
			% QueryReturn = do(qlc:q( [{X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			% if 
				% QueryReturn =:= [] -> [];
				% QueryReturn =/= [] ->
					% [{SelfCheck, OtherCheck}] = QueryReturn,
					% SelfMoveToHand = [{action, moving_to_hand}] -- SelfCheck,
					% SelfArenaToHand = [{action, arena_to_hand}] -- SelfCheck,
					% OtherMoveToHand = [{action, moving_to_hand}] -- OtherCheck,
					% OtherArenaToHand = [{action, arena_to_hand}] -- OtherCheck,
				% if
					% SelfMoveToHand =:= []; SelfArenaToHand =:= []; OtherMoveToHand =:= []; OtherArenaToHand =:= [] ->[{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
					% true -> []
				% end
			% end;
		moving_to_hand ->
			QueryReturn = do(qlc:q( [{ X#card_ability.owner_check, X#card_ability.other_check}|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			if
				QueryReturn =:= [] -> [];
				QueryReturn =/= [] -> [{OwnPlayerCheck, SomePlayerCheck}] = QueryReturn,
				OwnPlayerAssignAttack = [{action, moving_to_hand}]--OwnPlayerCheck,
				SomePlayerAssignAttack = [{action, moving_to_hand}]--SomePlayerCheck,
					  if
							OwnPlayerAssignAttack =:= []; SomePlayerAssignAttack =:= [] -> [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}];
							true -> []
					  end
			end;
		_  -> []
	end.
%===============================================================================	
list_operate(_, _, []) -> [];
list_operate(PlayerPid, OppPid, [{{CardHolderID, CardOrder, CardID}, AbilityId}|Tail]) ->
	if 
		CardHolderID =:= PlayerPid -> 
			[{PlayerPid, CardOrder, CardID}, AbilityId, OppPid| list_operate(PlayerPid, OppPid, Tail)];
		CardHolderID =:= OppPid ->
			[{OppPid, CardOrder, CardID}, AbilityId, PlayerPid|list_operate(PlayerPid, OppPid, Tail)]
	end;
list_operate(PlayerPid, OppPid, [{CardZone, {CardHolderID, CardOrder, CardID}, AbilityId}|Tail]) ->
	if 
		CardHolderID =:= PlayerPid -> 
			[{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OppPid|list_operate(PlayerPid, OppPid, Tail)];
		CardHolderID =:= OppPid ->
			[{CardZone, {OppPid, CardOrder, CardID}}, AbilityId, PlayerPid|list_operate(PlayerPid, OppPid, Tail)]
	end;
list_operate(PlayerPid, OppPid, [Any|Tail]) -> [Any|list_operate(PlayerPid, OppPid, Tail)].
%===============================================================================