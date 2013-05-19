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
-module(effect_value).
-export([
						check_value/5
					]).
					
% Remark
	% CardOwner, CardOrder, CardID จะเป็น Target ที่ได้รับ Effect
check_value(CardOwner, CardOrder, CardID, {FxType, Value}, {TOwner, TOrder, TID}) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	case Value of
		{Operator, Atom} ->
			case Atom of
					% ตามจำนวน Seal ของผู้ควบคุม ที่การ์ดที่ถูก paste นั้น ควบคุมอยู่
					paste_seal_controller_count ->
						TOppPid = mnesia_play:get_opponent_pid(TOwner),
						{ControllerPid, UncontrolPid, _} = attribute_check:check_controller({TOwner, TOrder, TID}, TOppPid, controller),
						ControllerSeal = card_utility:get_all_card(ControllerPid, seal_card, arena_zone),
						UncontrolSeal = card_utility:get_all_card(UncontrolPid, seal_card, arena_zone),
						SealList = target_attack:attacker_uncontrol(UncontrolPid, ControllerPid, ControllerSeal ++ UncontrolSeal),
						Power = length(SealList);
					% OwnerOrTarget ระบุว่าจะเช็คที่ เจ้าของ Effect หรือเป้่าที่ได้รับ Effect
					{{OwnerOrTarget, WhoseCard, ZoneRequire, CardType, IncludeS}, Condition} ->
						case OwnerOrTarget of
							target ->
								TOppPid = mnesia_play:get_opponent_pid(TOwner),
								{Play1, Play2, Require} =
								case WhoseCard of
									owner -> {TOwner, TOppPid, owner};
									opponent -> {TOwner, TOppPid, opponent};
									controller -> attribute_check:check_controller({TOwner, TOrder, TID}, TOppPid, controller);
									uncontol -> attribute_check:check_controller({TOwner, TOrder, TID}, TOppPid, uncontrol);
									null -> {TOwner, TOppPid, null}
								end,
								AllCardList = skill_card_list:player_zone_list({Require, ZoneRequire, CardType}, {Play1, Play2}), % Return a list of cards of each player in require zone
								CardList = 
								case IncludeS of
									n -> 
										TZone = card_utility:check_card_zone(TOwner, TOrder, TID),
										AllCardList -- [{TZone, {TOwner, TOrder, TID}}];
									_ -> AllCardList
								 end,
								Cards = function_utility:card_match_condition(CardList, Condition),
								Power = length(Cards);
							owner ->
								OppPid = mnesia_play:get_opponent_pid(CardOwner),
								{Play1, Play2, Require} =
								case WhoseCard of
									owner -> {CardOwner, OppPid, owner};
									opponent -> {CardOwner, OppPid, opponent};
									controller -> attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller);
									uncontol -> attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, uncontrol);
									null -> {CardOwner, OppPid, null}
								end,
								AllCardList = skill_card_list:player_zone_list({Require, ZoneRequire, CardType}, {Play1, Play2}), % Return a list of cards of each player in require zone
								CardList = 
								case IncludeS of
									n -> 
										OwnerZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
										AllCardList -- [{OwnerZone, {CardOwner, CardOrder, CardID}}];
									_ -> AllCardList
								 end,
								Cards = function_utility:card_match_condition(CardList, Condition),
								Power = length(Cards)
						end;
					%+ At ตาม Lv ของ Seal ใน Shrine เรา
					lv_s_in_shrine ->
						LvSealShrine = shrine_zone:get_shrine_level(CardOwner),
						Power = LvSealShrine;
					% +ตาม Lv ของ Seal ที่โจมตี [S]
					attack_to_s_level ->
						SealCards = card_utility:get_all_card(seal_card, arena_zone),
						[{APlayerID, ACardOrder, ACardID}] = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards), [{action, attacker}]),
						{_, Power} = arena_zone:get_card_power(APlayerID, ACardOrder, ACardID, level),
						Power;
																													% - ตามจำนวน [Evil] ใน Shrine เรา
																														evil_in_shrine -> 
																															SealShrine = card_utility:get_all_card (CardOwner, seal_card, shrine_cards),%{ok, ShrineCards} = mnesia_play:get_player_data (CardOwner, shrine_cards),
																															ShrineEvil = function_utility:card_match_condition(function_utility:at_zone(shrine_cards, SealShrine), [{type, "Evil"}]),%Power = count_card_condition (ShrineCards, "CB", 0, inverse);
																															Power = length(ShrineEvil);
																													% ตามจำนวนซีลการ์ดที่อยู่ใน AT line ฝ่ายเรา
																														t_owner_at_line_seal ->
																															TSealList = card_utility:get_all_card(TOwner, seal_card, arena_zone),
																															TAtSeal = function_utility:card_match_condition(function_utility:at_zone(arena_zone, TSealList), [{line, 1}]),
																															Power = length(TAtSeal);
																													%-At ตามจำนวน Seal ใบอื่นที่อยู่ใน At Line ของผู้ควบคุม
																													controller_at_line_seal ->
																														{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
																														case ControllerPid of
																															CardOwner -> % เจ้าของกับผู้ควบคุมเป็นคนเดียวกัน
																																SealList = card_utility:get_all_card (CardOwner, seal_card, arena_zone)--[{CardOwner, CardOrder, CardID}],
																																Power= line_check (SealList, 1, 0);
																															_ -> % เจ้าของกับผู้ควบคุมไม่ใช่คนเดียวกัน
																																SealList = card_utility:get_all_card (OpponentPid, seal_card, arena_zone)--[{CardOwner, CardOrder, CardID}],
																																Power= line_check (SealList, 1, 0)
																														end;
																													controller_seal_count ->
																														{ControllerPid, _, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
																														SealList = card_utility:get_all_card(ControllerPid, seal_card, arena_zone),
																														Power = length(SealList);
																													% ตามจำนวนซีลการ์ดที่อยู่ใน DF line ฝ่ายเรา
																													our_df_line_seal -> SealList = card_utility:get_all_card (CardOwner, seal_card, arena_zone),
																														Power = line_check (SealList, 0, 0);
																													% -ตามจำนวน Seal ที่อยู่ใน At Line ฝ่ายตรงข้าม
																													t_opponent_at_line_seal ->
																														TOppPid = mnesia_play:get_opponent_pid(TOwner),
																														SealList = card_utility:get_all_card(TOppPid, seal_card, arena_zone),
																														Power = line_check(SealList, 1, 0);
																													% ตามจำนวนการ์ดที่ชื่อ [S] ใบอื่นในสนาม
																													same_name_as_s_arena -> 
																														Power = check_card_same (CardID, null, all_arena, remove_self);
																													% - ตามจำนวน Mystic Card ในมือฝ่ายตรงข้าม
																													opponent_hand_mystic ->
																														Power= hand_zone:check_card_size (OpponentPid, is_not_seal);
																													% ตามจำนวนมิสติกการ์ดในมือผู้ควบคุม
																													controller_hand_mystic ->
																														{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
																														case ControllerPid of
																															CardOwner -> % เจ้าของกับผู้ควบคุมเป็นคนเดียวกัน
																																Power = hand_zone:check_card_size (CardOwner, is_not_seal);
																															_ -> % เจ้าของกับผู้ควบคุมไม่ใช่คนเดียวกัน
																																Power = hand_zone:check_card_size (OpponentPid, is_not_seal)
																														end;
																													% '+ตามจำนวน Seal ในสนามฝ่ายตรงข้าม
																													opponent_arena_seal -> SealList = card_utility:get_all_card (OpponentPid, seal_card, arena_zone),
																														Power = length(SealList);
																													%+At ตามจำนวน Seal ที่รวมร่างในสนามฝ่ายตรงข้าม
																													opponent_arena_combine_seal ->
																														OppSealList = card_utility:get_all_card(OpponentPid, seal_card, arena_zone),
																														OppComSeal = function_utility:card_match_condition(function_utility:at_zone(arena_zone, OppSealList), [{combine, y}]),
																														Power = length(OppComSeal);
																													% +ตามจำนวน Seal ที่ติด Curse ในสนาม
																													arena_curse_seal 	-> 
																														SealCards = card_utility:get_all_card (seal_card, arena_zone),
																														Power = ability_effect:count_effect (SealCards, any_curse);
																													{arena_curse, Curse} 	-> 
																														SealCards = card_utility:get_all_card(seal_card, arena_zone),
																														PoisonCards = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards), [{curse, Curse}]),
																														Power = length(PoisonCards);
																													our_seal_in_shrine -> 
																														SealShrine = card_utility:get_all_card(CardOwner, seal_card, shrine_cards),%{ok, ShrineCards} = mnesia_play:get_player_data (CardOwner, shrine_cards),
																														Power = length(SealShrine);
																													dark_in_controller_shrine ->
																														{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
																														%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
																														SealShrine = card_utility:get_all_card(ControllerPid, seal_card, shrine_cards),%{ok, ShrineCards} = mnesia_play:get_player_data (CardOwner, shrine_cards),
																														ShrineDark = function_utility:card_match_condition(function_utility:at_zone(shrine_cards, SealShrine), [{elem, 5}]),%Power = count_card_condition (ShrineCards, "CB", 0, inverse);
																														Power = length(ShrineDark);
																													other_penguin_name_arena ->
																														ArenaSeal = card_utility:get_all_card(seal_card, arena_zone),
																														PenguinArena = function_utility:card_match_condition(function_utility:at_zone(arena_zone, ArenaSeal), [{naming, "Penguin"}]),%Power = count_card_condition (ShrineCards, "CB", 0, inverse);
																														Power = length(PenguinArena -- [{CardOwner, CardOrder, CardID}]);
																													other_dark_dream_pegasus_name_shrine ->
																														SealShrine = card_utility:get_all_card(seal_card, shrine_cards),
																														DarkDreamShrine =  function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealShrine), [{name, "Dark Dream Pegasus"}]),
																														Power = length(DarkDreamShrine -- [{CardOwner, CardOrder, CardID}]);
																													other_s_in_shrine ->
																														SealShrine = card_utility:get_all_card(CardOwner, seal_card, shrine_cards),%{ok, ShrineCards} = mnesia_play:get_player_data (CardOwner, shrine_cards),
																														SZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
																														[CardName] = game_info:card_name({SZone, {CardOwner, CardOrder, CardID}}),
																														smo_logger:fmsg("card name is ~p~n", [CardName]),
																														SShrine = function_utility:card_match_condition(function_utility:at_zone(shrine_cards, SealShrine), [{name, CardName}]),%Power = count_card_condition (ShrineCards, "CB", 0, inverse);
																														%smo_logger:fmsg("card match condition are ~p~n", [SShrine]),
																														Power = length(SShrine);
					%+ ตาม Sp ของ Seal ที่ต่อสู้กับ [S]
					%'+P'
					fighting_with_s_speed ->
						SealCards = card_utility:get_all_card(seal_card, arena_zone),
						CardFight = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards), [{action, fight}]),
						[{FPlayerID, FCardOrder, FCardID}] = CardFight--[{TOwner, TOrder, TID}],
						{_, CheckPower} = arena_zone:get_card_power(FPlayerID, FCardOrder, FCardID, speed),
						 Power = 
						case CheckPower > 5  of
							true -> 5;
							_ -> CheckPower
						end; 
					% +ตาม Sp ที่แตกต่างกับ Seal ที่ต่อสู้กับ [S]
					fighting_with_diff_speed ->
						SealCards = card_utility:get_all_card (seal_card, arena_zone),
						CardFight = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards), [{action, fight}]),
						[{FPlayerID, FCardOrder, FCardID}] = CardFight--[{CardOwner, CardOrder, CardID}],
						{_, CheckPowerOwn} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, speed),
						{_, CheckPowerOpp} = arena_zone:get_card_power(FPlayerID, FCardOrder, FCardID, speed),
						CheckPower = abs(CheckPowerOwn - CheckPowerOpp),
						 Power = 
						case CheckPower > 5  of
							true -> 5;
							_ -> CheckPower
						end; 
					% ตาม mp ค่าร่ายของซีล {CardOwner, CardOrder, CardID} ถ้าเป็น skill คือ Seal ที่เป็นเป้าแต่ถ้าเป็น Ability จะเป็น Seal ที่เป็น ...
					%'+Q'
					
					% ใช้ กับ 'และ' คือ ต้องเป็นค่าพลัง ของ การ์ด ตอนที่ยังไม่ เปลี่ยน zone
					{get_erase, Collect} -> 
						Power =
						case get(Collect) of
							undefined -> 0;
							Get -> erase(Collect), Get
						end;
					%% ใช้ กับ 'จากนั้น' คือ ต้องเป็นค่าพลัง ของ การ์ด ตอนที่ เปลี่ยน zone ไปแล้ว
					{target, Collect} ->
						[{PTOwner, PTOrder, PTId}] = get(target),
						TZone = card_utility:check_card_zone(PTOwner, PTOrder, PTId),
						Power =
						case Collect of
							mc -> game_info:card_mpcast({TZone, {PTOwner, PTOrder, PTId}})
						end;
					select_seal_mp ->
						{_, Power} = arena_zone:get_card_power(TOwner, TOrder, TID, mp_cast),
						%io:format('MpCast ~p~n' , [MpCast]),
						Power;
																					
					
					% %+At เกิดได้แค่ใบเดียว
					{PowerNum, {active_single_s, AbilityNo, Whose}} ->
						Side =
						case Whose of
							owner -> CardOwner;
							_ -> OpponentPid
						end,
						Power =
						case get({power_round, Side}) of
							undefined -> 
								put({power_round, Side}, [{{CardID, AbilityNo}, {CardOwner, CardOrder}, [{TOwner, TOrder, TID}]}]),
								PowerNum;
							CardActive -> 
								case function_utility:keysearch_all({CardID, AbilityNo}, CardActive) of
									%{CardID, AbilityNo} ยังไม่ได้ Active ดังนั้น Power = PowerNum
									[] -> 
										% และ put
										put({power_round, Side}, CardActive ++ [{{CardID, AbilityNo}, {CardOwner, CardOrder}, [{TOwner, TOrder, TID}]}]),
										PowerNum;
									%{CardID, AbilityNo} Active แล้ว ดังนั้น Power = 0
									OtherActive -> 
										case function_utility:search_single_ability_target({CardID, AbilityNo}, [{TOwner, TOrder, TID}], OtherActive) of
											target_had_receive_effect -> 0;
											ResultActive ->
												put({power_round, Side}, (CardActive ++ ResultActive) -- OtherActive ),
												PowerNum
										end;
									_ -> 0
								end
						end,
						Power;
						%SealShrine = card_utility:get_all_card(seal_card, shrine_cards),
						% %DarkmistUnicornShrine =  function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealShrine), [{name, "Darkmist Unicorn"}]),
						% %Amount = length(DarkmistUnicornShrine -- [{CardOwner, CardOrder, CardID}]),
						% %	if	Amount =:= 1 -> Power = 1;
						% %		true -> Power = 0
						% %	end,
					target_level ->
						{ok, Level} = mnesia_odbc:get_seal_data(TID, level),
						Power = Level;
					{target_level, Max} ->
						{ok, Level} = mnesia_odbc:get_seal_data(TID, level),
						Return = 
						case Level< Max of
							true -> Level;
							_ -> Max
						end,							
						Power = Return;
					{PowerType, {Owner, Order, ID}} ->
						{ok, Power} = arena_zone:get_card_power(Owner, Order, ID, PowerType),
						Power;
					{get_value, ID} ->
						Power = get(ID),
						erase(ID);
					{Condition, SignPower} ->
						smo_logger:fmsg("condition check is ~p~n", [Condition]),
						case is_list(Condition) of
							true ->
								case function_utility:card_match_condition([{TOwner, TOrder, TID}], Condition) of
									[] -> Power = 0;
									_CardMatch -> %smo_logger:fmsg("card match condition are ~p~n", [_CardMatch]),
										Power = SignPower
								end;
							_ -> Power = 0
						end;
					% ตามจำนวน charge counter ของ [S]
					s_charge_counter ->
						{_, Power} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, charge_counter),
						%io:format('MpCast ~p~n' , [MpCast]),
						Power;
					s_current_df ->
						{_, Power} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, defend),
						%io:format('MpCast ~p~n' , [MpCast]),
						Power;
					{CardType, SearchType, Data, zone, Zone, owner, Owner} ->
						case Owner of
							all -> 
								CardList = card_utility:get_all_card (CardOwner, CardType, Zone) ++ card_utility:get_all_card (OpponentPid, CardType, Zone);
							owner ->
								CardList = card_utility:get_all_card (CardOwner, CardType, Zone);
							opponent ->
								CardList = card_utility:get_all_card (OpponentPid, CardType, Zone);
							controller ->
								{ControllerPid, _, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
								CardList = card_utility:get_all_card (ControllerPid, CardType, Zone);
							{controller, Action} ->
								case Action of
									attacker -> 
										SealCards = card_utility:get_all_card (seal_card, arena_zone),
										[{APlayerID, ACardOrder, ACardID}] = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards), [{action, attacker}]),
										CardList = card_utility:get_all_card (APlayerID, CardType, Zone)
								end
						end,

						case Data of
							attacking_type ->
								SealCards2 = card_utility:get_all_card (seal_card, arena_zone),
								[{APlayerID2, ACardOrder2, ACardID2}] = function_utility:card_match_condition(function_utility:at_zone(arena_zone, SealCards2), [{action, attacker}]),
								{ok, InterestData} = mnesia_odbc:get_seal_data(ACardID2, card_type);								 
							_ ->
								InterestData = Data
						end,
						CardMatch = get_search_match_list(Zone, CardList, SearchType, InterestData,[]),
						Power = length(CardMatch);
						
					Other ->	io:format("undefine case of '~p' then power change is +-0-+ ~n", [Other]), Power = 0
			end,
			FinalPower=
			case Operator of
				'+' -> Power;
				'-' -> -Power;
				%_ -> FinalPower = check_other_case (CardOwner, CardOrder, CardID, Value);
				% equal -> 
					% case Atom of
						% set_new_sp -> {Operator, Power};
						% _ -> {Operator, Atom}		
					% end;
				_ -> {Operator, Atom}
			end;
		_ -> 
			%case is_integer(Value) of
				 FinalPower = Value
				%true -> FinalPower = Value
				%false -> FinalPower= check_other_case (CardOwner, CardOrder, CardID, Value)
			%end
	end,
	[{FxType, FinalPower}];
check_value(CardOwner, CardOrder, CardID, {FxType, Value}, PlayerPid) -> "".
	
line_check ([], _, Amount) -> Amount;
line_check ([{_, CardOption} | T], Line, Amount) ->
	case seal_card:is_on_line (CardOption, Line) of
		on_line -> line_check (T, Line, Amount + 1);
		off_line -> line_check (T, Line, Amount)
	end.
	
check_card_same (CardID, PlayerPid, ArenaType, CountType) ->
	case ArenaType of
		all_arena -> Seals = card_utility:get_all_card (seal_card, arena_zone);
		player_arena -> Seals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone)
	end,
	{ok, [CardName]} = mnesia_odbc:get_seal_data (CardID, card_name),
	Count = count_card_name (Seals, CardName),
	case CountType of
		remove_self -> Count - 1;
		count_all -> Count
	end.
	
count_card_name ([], _) -> 0;
count_card_name ([{{PlayerPid, CardOrder, CardID}, _} | T], NameCheck) ->
	{ok, Name} = mnesia_odbc:get_seal_data (CardID, card_name),
	{ok, RFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, arena_zone),
	{ok, SFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, arena_zone),
	NameAddOn = get_effect_add_on_name (RFx ++ SFx),
	case check_name_match (Name ++ NameAddOn, NameCheck) of
		match -> 1 + count_card_name (T, NameCheck);
		not_match -> count_card_name (T, NameCheck)
	end.
	
get_effect_add_on_name ([]) -> [];
get_effect_add_on_name ([{_, Effect, _} | T]) ->
	get_name_effect (Effect) ++ get_effect_add_on_name (T).
	
get_name_effect ([]) -> [];
get_name_effect ([{name, Name} | T]) -> [Name] ++ get_name_effect (T);
get_name_effect ([_ | T]) -> get_name_effect (T).

check_name_match ([], _) -> not_match;
check_name_match ([NameCheck | _], NameCheck) -> match;
check_name_match ([_ | T], NameCheck) -> check_name_match (T, NameCheck).

get_search_match_list(_,_,_,[],Result) -> Result;
get_search_match_list(Zone,CardList,SearchType,[Data|T],Result) -> 
	CardMatch = function_utility:card_match_condition(function_utility:at_zone(Zone, CardList), [{SearchType, Data}]),
	NewCardMatch = CardMatch -- Result,
	NewResult = Result ++ NewCardMatch,
	get_search_match_list(Zone,CardList,SearchType,T,NewResult).
