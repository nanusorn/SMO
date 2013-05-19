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
-module(skill_card_list).
-compile(export_all).

%===============================================================================
get_target_as_require(all, Cards, _) -> Cards;
get_target_as_require(0, _, ResTarget) -> ResTarget;
get_target_as_require(_, [], ResTarget) -> ResTarget;
get_target_as_require(TargetDo, [Target|Remain], ResTarget) -> get_target_as_require(TargetDo - 1, Remain, ResTarget ++ [Target]).

player_zone_list({Player, [Zone|Tail], CardType}, {PlayerOwnID, PlayerOppID}) ->
	 %io:format('Start player_zone_list ~n'),
	 %io:format('Zone ~p~n',[Zone]),
	 %io:format('CardType ~p~n',[CardType]),
	 %io:format('PlayerOwnID ~p~n',[PlayerOwnID]),
	 %io:format('PlayerOppID ~p~n',[PlayerOppID]),
	case {Player, Zone} of 
		{owner, {RealZone, top, X}} ->
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, RealZone),
			if
				OwnStatus =:= ok ->
					  OwnZone = function_utility:exclude_option(OwnZone1),
					  OwnXZone1 = function_utility:at_zone(RealZone, OwnZone),
					  io:format("~p~n", [get_target_as_require(X, OwnXZone1, [])]),
					  OwnXZone = get_target_as_require(X, OwnXZone1, []);
				OwnStatus =/= ok -> OwnXZone = []
			end,
			%io:format ("CardReturn Opponent  ~p~n", [OppXZone]),
			CardReturn = OwnXZone;
		
		
		{null, null} -> % purpose get data, option, effect recieve, ....
			case mnesia_play:get_player_data(PlayerOwnID, arena_zone) of
				{ok, Arena1} -> OwnArena1 = Arena1;
				_ -> OwnArena1 = []
			end,
			 %io:format('ownarena1_operate ~n'),
			OwnArena = function_utility:exclude_option(OwnArena1),
			OwnArenaZone = function_utility:at_zone(arena_zone, OwnArena),
			%%io:format ("OwnArenaZone ~p~n", [OwnArenaZone]),
			{ SStatus1, Shrine1} = mnesia_play:get_player_data(PlayerOwnID, shrine_cards),
			if
				SStatus1 =:= ok -> OwnShrine1 = Shrine1;
				SStatus1 =/= ok -> OwnShrine1 = []
			end,
			OwnShrine = function_utility:exclude_option(OwnShrine1),
			OwnShrineZone = function_utility:at_zone(shrine_cards, OwnShrine),
			%%io:format ("OwnShrineZone ~p~n", [OwnShrineZone]),
			
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
			
			{ HStatus1, Hand1} = mnesia_play:get_player_data(PlayerOwnID, hand_cards),
			if
				HStatus1 =:= ok -> OwnHand1 = Hand1;
				HStatus1 =/= ok -> OwnHand1 = []
			end,
			%io:format('ownhand1_operate ~n'),
			OwnHand = function_utility:exclude_option(OwnHand1),
			OwnHandZone = function_utility:at_zone(hand_cards, OwnHand),
			%%io:format ("OwnHandZone ~p~n", [OwnHandZone]),
			
			{AStatus2, Arena2} = mnesia_play:get_player_data(PlayerOppID, arena_zone),
			if
				AStatus2 =:= ok -> OppArena1 = Arena2;
				AStatus2 =/= ok -> OppArena1 = []
			end,
			%io:format('opparena1_operate ~n'),
			OppArena = function_utility:exclude_option(OppArena1),
			OppArenaZone = function_utility:at_zone(arena_zone, OppArena),
			%%io:format ("OppArenaZone ~p~n", [OppArenaZone]),
			{ SStatus2, Shrine2} = mnesia_play:get_player_data(PlayerOppID, shrine_cards),
			if
				SStatus2 =:= ok -> OppShrine1 = Shrine2;
				SStatus2 =/= ok -> OppShrine1 = []
			end,
			OppShrine = function_utility:exclude_option(OppShrine1),
			OppShrineZone = function_utility:at_zone(shrine_cards, OppShrine),
			%%io:format ("OppShrineZone ~p~n", [OppShrineZone]),
												
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
												%%io:format ("OppDeckZone ~p~n", [OppDeckZone]),
												
			{ HStatus2, Hand2} = mnesia_play:get_player_data(PlayerOppID, hand_cards),
			if
				HStatus2 =:= ok -> OppHand1 = Hand2;
				HStatus2 =/= ok -> OppHand1 = []
			end,
			%io:format('opphand1_operate ~n'),
			OppHand = function_utility:exclude_option(OppHand1),
			OppHandZone = function_utility:at_zone(hand_cards, OppHand),
			%%io:format ("OppHandZone ~p~n", [OppHandZone]),
			%io:format ("CardReturn ~p~n", [OwnArenaZone++OwnShrineZone++OwnDeckZone++OwnHandZone++OppArenaZone++OppShrineZone++OppDeckZone++OppHandZone]),
			%CardReturn = OwnArenaZone++OwnShrineZone++SownDeckZone++MownDeckZone++OwnHandZone++OppArenaZone++OppShrineZone++SoppDeckZone++MoppDeckZone++OppHandZone;
			CardReturn = OwnArenaZone++OwnShrineZone++OwnHandZone++OppArenaZone++OppShrineZone++OppHandZone;
			
			
		{null, _ } -> 
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, Zone),
			if
				OwnStatus =:= ok ->		
					OwnZone = function_utility:exclude_option(OwnZone1),	
					OwnXZone = function_utility:at_zone(Zone, OwnZone);
				OwnStatus =/= ok -> OwnXZone = []
			end,
			{OppStatus, OppZone1} = mnesia_play:get_player_data(PlayerOppID, Zone),
			if
				OppStatus =:= ok ->
					  OppZone = function_utility:exclude_option(OppZone1),						
					  OppXZone = function_utility:at_zone(Zone, OppZone);
				OppStatus =/= ok -> OppXZone = []
			end,
			%io:format ("CardReturn ~p~n", [OppXZone]),
			CardReturn = OwnXZone++OppXZone;
			
		
		{opponent, null } -> 
			{AStatus2, Arena2} = mnesia_play:get_player_data(PlayerOppID, arena_zone),
			if
				AStatus2 =:= ok -> OppArena1 = Arena2;
				AStatus2 =/= ok -> OppArena1 = []
			end,
			%io:format('opparena1_operate ~n'),
			OppArena = function_utility:exclude_option(OppArena1),
			OppArenaZone = function_utility:at_zone(arena_zone, OppArena),
			%%io:format ("OppArenaZone ~p~n", [OppArenaZone]),
			
			{ SStatus2, Shrine2} = mnesia_play:get_player_data(PlayerOppID, shrine_cards),
			if
				SStatus2 =:= ok -> OppShrine1 = Shrine2;
				SStatus2 =/= ok -> OppShrine1 = []
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
			
			{HStatus2, Hand2} = mnesia_play:get_player_data(PlayerOppID, hand_cards),
			if
				HStatus2 =:= ok -> OppHand1 = Hand2;
				HStatus2 =/= ok -> OppHand1 = []
			end,
			%io:format('opphand3_operate ~n'),
			OppHand = function_utility:exclude_option(OppHand1),
			OppHandZone = function_utility:at_zone(hand_cards, OppHand),
			%io:format ("CardReturn ~p~n", [OppHandZone]),
			%CardReturn = OppArenaZone++OppShrineZone++SoppDeckZone++MoppDeckZone++OppHandZone;
			CardReturn = OppArenaZone++OppShrineZone++OppHandZone;
				
		{opponent, _ } -> 
			{ OppStatus, OppZone1} = mnesia_play:get_player_data(PlayerOppID, Zone),
			if
				OppStatus =:= ok ->
					  OppZone = function_utility:exclude_option(OppZone1),
					  OppXZone = function_utility:at_zone(Zone, OppZone);
				OppStatus =/= ok -> OppXZone = []
			end,
			%io:format ("CardReturn Opponent  ~p~n", [OppXZone]),
			CardReturn = OppXZone;
		
		{owner, null } -> 
			{AStatus1, Arena1} = mnesia_play:get_player_data(PlayerOwnID, arena_zone),
			if
				AStatus1 =:= ok -> OwnArena1 = Arena1;
				AStatus1 =/= ok -> OwnArena1 = []
			end,
			 %io:format('ownarena1_operate ~n'),
			OwnArena = function_utility:exclude_option(OwnArena1),
			OwnArenaZone = function_utility:at_zone(arena_zone, OwnArena),
			%%io:format ("OwnArenaZone ~p~n", [OwnArenaZone]),
			
			{ SStatus1, Shrine1} = mnesia_play:get_player_data(PlayerOwnID, shrine_cards),
			if
				SStatus1 =:= ok -> OwnShrine1 = Shrine1;
				SStatus1 =/= ok -> OwnShrine1 = []
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
			
			{ HStatus1, Hand1} = mnesia_play:get_player_data(PlayerOwnID, hand_cards),
			if
				HStatus1 =:= ok -> OwnHand1 = Hand1;
				HStatus1 =/= ok -> OwnHand1 = []
			end,
			%io:format('ownhand5_operate ~n'),
			OwnHand = function_utility:exclude_option(OwnHand1),
			OwnHandZone = function_utility:at_zone(hand_cards, OwnHand),
			%io:format ("CardReturn ~p~n", [CardReturn]),
			%CardReturn = OwnArenaZone++OwnShrineZone++SownDeckZone++MownDeckZone++OwnHandZone;
			CardReturn = OwnArenaZone++OwnShrineZone++OwnHandZone;
			
			
		{owner, _ } -> 
			%io:format('PlayerOwnID ~p~n',[PlayerOwnID]),
			%io:format('Zone ~p~n',[Zone]),
			{OwnStatus, OwnZone1} = mnesia_play:get_player_data(PlayerOwnID, Zone),
			if
				OwnStatus =:= ok ->
					  OwnZone = function_utility:exclude_option(OwnZone1),
					  OwnXZone = function_utility:at_zone(Zone, OwnZone);
				OwnStatus =/= ok -> OwnXZone = []
			end,
			%io:format ("CardReturn ~p~n", [OwnXZone]),
			CardReturn = OwnXZone	
	end,
	PreLastReturn = CardReturn++player_zone_list({Player, Tail, CardType}, {PlayerOwnID, PlayerOppID}),
	LastReturn = 
	if 
		CardType =:= seal -> function_utility:seal_only(PreLastReturn);
		CardType =:= mystic -> mystic_only(PreLastReturn);
		CardType =:= all -> PreLastReturn;
		true -> PreLastReturn
	end,
	LastReturn;
player_zone_list({_, [], _}, {_, _}) -> [].
%===============================================================
mystic_only([{Zone, {PlayerId, CardOrder, CardID}}|Tail]) ->
	%io:format('Mystic Only ~n'),
	Result = mnesia_odbc:is_seal_card(CardID),
	%io:format('Result ~p~n',[Result]),
	if
		Result =:= is_not_seal ->  [{Zone, {PlayerId, CardOrder, CardID}}|mystic_only(Tail)];
		Result =:= is_seal  -> mystic_only(Tail)
	end;
mystic_only([]) -> [].
%===============================================================================
with_zone_line([{PlayerPid, CardOrder, CardID}|Tail]) ->
	%io:format('Card without Zone and Line is ~p~n', [{PlayerPid, CardOrder, CardID}|Tail]),
	CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
	{Zone, CardLine} = 
	case CardZone of
		arena_zone ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> {ok, Line} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, line, CardZone);
				_ -> Line = 0
			end,
			{0, Line};
		shrine_cards -> {1, 0};
		seal_deck -> {2, 0};
		mystic_deck -> {3, 0};
		hand_cards -> {4, 0};
		remove_zone -> {5, 0};
		support_cards -> {6, 0}
	end,
	[{PlayerPid, CardOrder, CardID, Zone, CardLine}]++with_zone_line(Tail);		
with_zone_line([]) -> [].
%===============================================================================
only_on_zone(ReqZone, [{PlayerPid, CardOrder, CardID}|Tail]) ->
	case card_utility:check_card_zone(PlayerPid, CardOrder, CardID) of
		ReqZone -> [{PlayerPid, CardOrder, CardID}]++only_on_zone(ReqZone, Tail);
		_ -> only_on_zone(ReqZone, Tail)
	end;
only_on_zone(_, []) -> [].
%===============================================================================
check_condition_in_zone(Zone,  SeaOrMys, PlayerRequire, PlayerPid, {Case, Subcase}) ->
	CardsList = skill_card_list:player_zone_list({PlayerRequire, [Zone], SeaOrMys}, {PlayerPid, xxx}),
	check_condition(CardsList, {Case, Subcase}).
%===============================================================================
check_condition([ListHead|Tail], {Case, Subcase}) ->
	case check_other:check_all_other_require_attribute(xxx, ListHead, [{Case, Subcase}]) of
		[] -> check_condition(Tail, {Case, Subcase});
		_ -> [ok]++check_condition(Tail, {Case, Subcase})
	end;
check_condition([], _) -> [].