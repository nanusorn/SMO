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
-module(card_list).
-compile(export_all).
-import (mnesia_table, [do/1]).

-include_lib("s_ability.hrl").
%-include_lib("m_ability.hrl").
-include_lib("stdlib/include/qlc.hrl").

%===============================================================================
player_zone_list({Player, [Zone|Tail]}, {PlayerOwnID, PlayerOppID}) ->
	%io:format('Player = ~p, Zone = ~p, PlayerOwn = ~p, PlayerOpp = ~p ~n', [Player, Zone, PlayerOwnID, PlayerOppID]),
	case {Player, Zone} of 
		{owner, {RealZone, top, X}} ->
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, RealZone),
			if
				OwnStatus =:= ok ->
					  OwnZone = function_utility:exclude_option(OwnZone1),
					  OwnXZone1 = function_utility:at_zone(RealZone, OwnZone),
					  io:format("~p~n", [skill_card_list:get_target_as_require(X, OwnXZone1, [])]),
					  OwnXZone = skill_card_list:get_target_as_require(X, OwnXZone1, []);
				OwnStatus =/= ok -> OwnXZone = []
			end,
			%io:format ("CardReturn Opponent  ~p~n", [OppXZone]),
			CardReturn = OwnXZone;
		{null, null} -> % purpose get data, option, effect recieve, ....
			CardReturn = player_zone_list({owner, [null]}, {PlayerOwnID, PlayerOppID}) ++ player_zone_list({opponent, [null]}, {PlayerOwnID, PlayerOppID});
			% {AStatus1, Arena1} = mnesia_play:get_player_data(PlayerOwnID, arena_zone),
			% %io:format ("mnesia_play:get_player_data(~p, arena_zone) = {~p, ~p} ~n", [PlayerOwnID, AStatus1, Arena1]),
			% if
				% AStatus1 =:= ok -> OwnArena1 = Arena1;
				% true -> OwnArena1 = []
			% end,
			 % %io:format('ownarena1_operate ~n'),
			% OwnArena = function_utility:exclude_option(OwnArena1),
			% OwnArenaZone = function_utility:at_zone(arena_zone, OwnArena),
			% %%io:format ("OwnArenaZone ~p~n", [OwnArenaZone]),
			% { SStatus1, Shrine1} = mnesia_play:get_player_data(PlayerOwnID, shrine_cards),
			% %io:format ("Shrine1 ~p~n", [Shrine1]),
			% if
				% SStatus1 =:= ok -> OwnShrine1 = Shrine1;
				% true -> OwnShrine1 = []
			% end,
			% OwnShrine = function_utility:exclude_option(OwnShrine1),
			% OwnShrineZone = function_utility:at_zone(shrine_cards, OwnShrine),
			% %%io:format ("OwnShrineZone ~p~n", [OwnShrineZone]),
			% 
			% {RStatus1, Remove1} = mnesia_play:get_player_data(PlayerOwnID, remove_cards),
			% %io:format ("Shrine1 ~p~n", [Shrine1]),
			% if
				% RStatus1 =:= ok -> OwnRemove1 = Remove1;
				% true -> OwnRemove1 = []
			% end,
			% OwnRemove = function_utility:exclude_option(OwnRemove1),
			% OwnRemoveZone = function_utility:at_zone(remove_cards, OwnRemove),
										% 
						% % {DStatus1, Sdeck1} = mnesia_play:get_player_data(PlayerOwnID, seal_deck),
						% % if
							% % DStatus1 =:= ok -> SownDeck1 = Sdeck1;
							% % true -> SownDeck1 = []
						% % end,
						% % SownDeck = function_utility:exclude_option(SownDeck1),
						% % SownDeckZone = function_utility:at_zone(seal_deck, SownDeck),
			% % 
						% % {MDStatus1, Mdeck1} = mnesia_play:get_player_data(PlayerOwnID, mystic_deck),
						% % if
							% % MDStatus1 =:= ok -> MownDeck1 = Mdeck1;
							% % true -> MownDeck1 = []
						% % end,
						% % MownDeck = function_utility:exclude_option(MownDeck1),
						% % MownDeckZone = function_utility:at_zone(mystic_deck, MownDeck),
											% 
			% { HStatus1, Hand1} = mnesia_play:get_player_data(PlayerOwnID, hand_cards),
			% %io:format ("Hand1 ~p~n", [Hand1]),
			% if
				% HStatus1 =:= ok -> OwnHand1 = Hand1;
				% true -> OwnHand1 = []
			% end,
			% %io:format('ownhand1_operate ~n'),
			% OwnHand = function_utility:exclude_option(OwnHand1),
			% OwnHandZone = function_utility:at_zone(hand_cards, OwnHand),
			% %%io:format ("OwnHandZone ~p~n", [OwnHandZone]),
			% 
			% {AStatus2, Arena2} = mnesia_play:get_player_data(PlayerOppID, arena_zone),
			% %io:format ("Arena2 ~p~n", [Arena2]),
			% if
				% AStatus2 =:= ok -> OppArena1 = Arena2;
				% true -> OppArena1 = []
			% end,
			% %io:format('opparena1_operate ~n'),
			% OppArena = function_utility:exclude_option(OppArena1),
			% OppArenaZone = function_utility:at_zone(arena_zone, OppArena),
			% %%io:format ("OppArenaZone ~p~n", [OppArenaZone]),
			% { SStatus2, Shrine2} = mnesia_play:get_player_data(PlayerOppID, shrine_cards),
			% %io:format ("Shrine2 ~p~n", [Shrine2]),
			% if
				% SStatus2 =:= ok -> OppShrine1 = Shrine2;
				% true -> OppShrine1 = []
			% end,
			% OppShrine = function_utility:exclude_option(OppShrine1),
			% OppShrineZone = function_utility:at_zone(shrine_cards, OppShrine),
			% 
			% {RStatus2, Remove2} = mnesia_play:get_player_data(PlayerOppID, remove_cards),
			% %io:format ("Shrine1 ~p~n", [Shrine1]),
			% if
				% RStatus2 =:= ok -> OppRemove1 = Remove2;
				% true -> OppRemove1 = []
			% end,
			% OppRemove = function_utility:exclude_option(OppRemove1),
			% OppRemoveZone = function_utility:at_zone(remove_cards, OppRemove),
			% 
			% %%io:format ("OppShrineZone ~p~n", [OppShrineZone]),
			% 
												% %{ DStatus2, Sdeck2} = mnesia_play:get_player_data(PlayerOppID, seal_deck),
												% %if
													% %DStatus2 =:= ok -> SoppDeck1 = Sdeck2;
													% %true -> SoppDeck1 = []
												% %end,
												% %SoppDeck = function_utility:exclude_option(SoppDeck1),
												% %SoppDeckZone = function_utility:at_zone(seal_deck, SoppDeck),
												% 
												% %{ MDStatus2, Mdeck2} = mnesia_play:get_player_data(PlayerOppID, mystic_deck),
												% %if
													% %MDStatus2 =:= ok -> MoppDeck1 = Mdeck2;
													% %true -> MoppDeck1 = []
												% %end,
												% %MoppDeck = function_utility:exclude_option(MoppDeck1),
												% %MoppDeckZone = function_utility:at_zone(mystic_deck, MoppDeck),
												% 
			% { HStatus2, Hand2} = mnesia_play:get_player_data(PlayerOppID, hand_cards),
			% if
				% HStatus2 =:= ok -> OppHand1 = Hand2;
				% true -> OppHand1 = []
			% end,
			% %io:format('opphand1_operate ~n'),
			% OppHand = function_utility:exclude_option(OppHand1),
			% OppHandZone = function_utility:at_zone(hand_cards, OppHand),
			% %%io:format ("OppHandZone ~p~n", [OppHandZone]),
			% %io:format ("CardReturn ~p~n", [OwnArenaZone++OwnShrineZone++OwnDeckZone++OwnHandZone++OppArenaZone++OppShrineZone++OppDeckZone++OppHandZone]),
			% %CardReturn = OwnArenaZone++OwnShrineZone++SownDeckZone++MownDeckZone++OwnHandZone++OppArenaZone++OppShrineZone++SoppDeckZone++MoppDeckZone++OppHandZone;
			% CardReturn = OwnArenaZone++OwnShrineZone++OwnHandZone++OwnRemoveZone ++ OppArenaZone++OppShrineZone++OppHandZone++ OppRemoveZone;
			{null, _ } -> 
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, Zone),
			if
				OwnStatus =:= ok ->		
					OwnZone = function_utility:exclude_option(OwnZone1),	
					OwnXZone = function_utility:at_zone(Zone, OwnZone);
				true -> OwnXZone = []
			end,
			{OppStatus, OppZone1} = mnesia_play:get_player_data(PlayerOppID, Zone),
			if
				OppStatus =:= ok ->
					  OppZone = function_utility:exclude_option(OppZone1),						
					  OppXZone = function_utility:at_zone(Zone, OppZone);
				true -> OppXZone = []
			end,
			%io:format ("CardReturn ~p~n", [OppXZone]),
			CardReturn = OwnXZone++OppXZone;
			
		
		{opponent, null } -> 
			{AStatus2, Arena2} = mnesia_play:get_player_data(PlayerOppID, arena_zone),
			if
				AStatus2 =:= ok -> OppArena1 = Arena2;
				true -> OppArena1 = []
			end,
			%io:format('opparena1_operate ~n'),
			OppArena = function_utility:exclude_option(OppArena1),
			OppArenaZone = function_utility:at_zone(arena_zone, OppArena),
			%%io:format ("OppArenaZone ~p~n", [OppArenaZone]),
			
			{ SStatus2, Shrine2} = mnesia_play:get_player_data(PlayerOppID, shrine_cards),
			if
				SStatus2 =:= ok -> OppShrine1 = Shrine2;
				true -> OppShrine1 = []
			end,
			OppShrine = function_utility:exclude_option(OppShrine1),
			OppShrineZone = function_utility:at_zone(shrine_cards, OppShrine),
			
										%{ DStatus2, Sdeck2} = mnesia_play:get_player_data(PlayerOppID, seal_deck),
										%if
											%DStatus2 =:= ok -> SoppDeck1 = Sdeck2;
											%true -> SoppDeck1 = []
										%end,
										%SoppDeck = function_utility:exclude_option(SoppDeck1),
										%SoppDeckZone = function_utility:at_zone(seal_deck, SoppDeck),
										
										%{ MDStatus2, Mdeck2} = mnesia_play:get_player_data(PlayerOppID, mystic_deck),
										%if
											%MDStatus2 =:= ok -> MoppDeck1 = Mdeck2;
											%true -> MoppDeck1 = []
										%end,
										%MoppDeck = function_utility:exclude_option(MoppDeck1),
										%MoppDeckZone = function_utility:at_zone(mystic_deck, MoppDeck),
			{RStatus2, Remove2} = mnesia_play:get_player_data(PlayerOppID, remove_cards),
			%io:format ("Shrine1 ~p~n", [Shrine1]),
			if
				RStatus2 =:= ok -> OppRemove1 = Remove2;
				true -> OppRemove1 = []
			end,
			OppRemove = function_utility:exclude_option(OppRemove1),
			OppRemoveZone = function_utility:at_zone(remove_cards, OppRemove),
													
			{ HStatus2, Hand2} = mnesia_play:get_player_data(PlayerOppID, hand_cards),
			if
				HStatus2 =:= ok -> OppHand1 = Hand2;
				true -> OppHand1 = []
			end,
			%io:format('opphand3_operate ~n'),
			OppHand = function_utility:exclude_option(OppHand1),
			OppHandZone = function_utility:at_zone(hand_cards, OppHand),
			%io:format ("CardReturn ~p~n", [OppHandZone]),
			%CardReturn = OppArenaZone++OppShrineZone++SoppDeckZone++MoppDeckZone++OppHandZone;
			CardReturn = OppArenaZone++OppShrineZone++OppHandZone ++ OppRemoveZone;
				
		{opponent, _ } -> 
			{ OppStatus, OppZone1} = mnesia_play:get_player_data(PlayerOppID, Zone),
			if
				OppStatus =:= ok ->
					  OppZone = function_utility:exclude_option(OppZone1),
					  OppXZone = function_utility:at_zone(Zone, OppZone);
				true -> OppXZone = []
			end,
			%io:format ("CardReturn Opponent  ~p~n", [OppXZone]),
			CardReturn = OppXZone;
		
		{owner, null } -> 
			{AStatus1, Arena1} = mnesia_play:get_player_data(PlayerOwnID, arena_zone),
			if
				AStatus1 =:= ok -> OwnArena1 = Arena1;
				true -> OwnArena1 = []
			end,
			 %io:format('ownarena1_operate ~n'),
			OwnArena = function_utility:exclude_option(OwnArena1),
			OwnArenaZone = function_utility:at_zone(arena_zone, OwnArena),
			%%io:format ("OwnArenaZone ~p~n", [OwnArenaZone]),
			
			{SStatus1, Shrine1} = mnesia_play:get_player_data(PlayerOwnID, shrine_cards),
			if
				SStatus1 =:= ok -> OwnShrine1 = Shrine1;
				true -> OwnShrine1 = []
			end,
			OwnShrine = function_utility:exclude_option(OwnShrine1),
			OwnShrineZone = function_utility:at_zone(shrine_cards, OwnShrine),	
			
										%{DStatus1, Sdeck1} = mnesia_play:get_player_data(PlayerOwnID, seal_deck),
										%if
											%DStatus1 =:= ok -> SownDeck1 = Sdeck1;
											%true -> SownDeck1 = []
										%end,
										%SownDeck = function_utility:exclude_option(SownDeck1),
										%SownDeckZone = function_utility:at_zone(seal_deck, SownDeck),
										
										%{MDStatus1, Mdeck1} = mnesia_play:get_player_data(PlayerOwnID, mystic_deck),
										%if
											%MDStatus1 =:= ok -> MownDeck1 = Mdeck1;
											%true -> MownDeck1 = []
										%end,
										%MownDeck = function_utility:exclude_option(MownDeck1),
										%MownDeckZone = function_utility:at_zone(mystic_deck, MownDeck),
										
			{RStatus1, Remove1} = mnesia_play:get_player_data(PlayerOwnID, remove_cards),
			%io:format ("Shrine1 ~p~n", [Shrine1]),
			if
				RStatus1 =:= ok -> OwnRemove1 = Remove1;
				true -> OwnRemove1 = []
			end,
			OwnRemove = function_utility:exclude_option(OwnRemove1),
			OwnRemoveZone = function_utility:at_zone(remove_cards, OwnRemove),
													
			{ HStatus1, Hand1} = mnesia_play:get_player_data(PlayerOwnID, hand_cards),
			if
				HStatus1 =:= ok -> OwnHand1 = Hand1;
				true -> OwnHand1 = []
			end,
			%io:format('ownhand5_operate ~n'),
			OwnHand = function_utility:exclude_option(OwnHand1),
			OwnHandZone = function_utility:at_zone(hand_cards, OwnHand),
			%io:format ("CardReturn ~p~n", [CardReturn]),
			%CardReturn = OwnArenaZone++OwnShrineZone++SownDeckZone++MownDeckZone++OwnHandZone;
			CardReturn = OwnArenaZone++OwnShrineZone++ OwnRemoveZone ++OwnHandZone;
			
			
		{owner, _ } -> 
			%io:format('PlayerOwnID ~p~n',[PlayerOwnID]),
			%io:format('Zone ~p~n',[Zone]),
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, Zone),
			%io:format('OwnZone ~p~n',[OwnZone1]),
			if
				OwnStatus =:= ok ->
					  OwnZone = function_utility:exclude_option(OwnZone1),
					  OwnXZone = function_utility:at_zone(Zone, OwnZone);
				true -> OwnXZone = []
			end,
			%io:format ("CardReturn ~p~n", [OwnXZone]),
			CardReturn = OwnXZone	
	end,
	CardReturn++player_zone_list({Player, Tail}, {PlayerOwnID, PlayerOppID});
player_zone_list({_, []}, {_, _}) -> [].
%===============================================================================
%return all card which got this AbilityId as a receiveEffect
got_indep_ability(CardID, AbilityId, PlayerOwnID, PlayerOppID) ->
	
	[{PlayerReq, ZonReq}] =
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			[{Player, Zone}]	= do(qlc:q([{X#card_ability.target_player_check, 
																					 		 X#card_ability.target_present_check
																						  }|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId]));
		is_not_seal -> [{dummy_need_correct, dummy_need_correct}]
			% [{PlayerReq, ZonReq}]	= do(qlc:q([{X#mystic_ability.target_player_check, 
																					 		 % X#mystic_ability.target_present_check
																						  % }|| X <- mnesia:table(mystic_ability), X#mystic_ability.mability_id =:= AbilityId]))
	end,
	CardList = player_zone_list({PlayerReq, ZonReq}, {PlayerOwnID, PlayerOppID}),
	a(AbilityId, CardList).
	
%check which this card got AbilityId as a ReceiveEffect
a(AbilityId, [ListHead|Tail]) ->
	B = game_info:card_added_ability(ListHead),
	Result = [AbilityId]--B,
	if
		Result =:= [] -> [ListHead]++a(AbilityId, Tail);
		true -> a(AbilityId, Tail)
	end;
a(_, []) -> [].