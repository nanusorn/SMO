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
-module(mod_ability_activate).
-import (mnesia_table, [do/1]).
-import(lists, [foreach/2, append/2]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("s_ability.hrl").
-include_lib("play_record.hrl").
-compile(export_all).
-export([
						set_a/4,
						set_b/4,
						check_any_ability_activate/2, % เรียกโดย Function ที่มีการกระทำ เกี่ยวกับ Ability
						set_all_ability_activate/2,
						set_effect_to_selected_target/3, % เรียกโดย room_controller:room_controller(....)
						post_select_activation/3, % เรียกโดย room_controller:room_controller(....)
						verify_ability_condition/1, % เรียกโดย Function ที่มีการกระทำ เกี่ยวกับ Ability
						add_all_fx_to_reselect_target/3, %เรียกโดย room_controller:room_controller(....)
						post_owner_select_activate/3,
						update_opponent_what_selected/2,
						generalize_effect_to_each_target/0
					]).

%check_ability_acitvate(Status, PlayerPid) ->
%	OpoonentPid = mnesia_play:get_opponent_pid (PlayerPid),
%	UpdateAbility = update_ability:update(Status, PlayerPid, OpponentPid),
%	Size = length([UpdateAbility]),
%	AbilityNo = query_ability:no_of_ability(AbilityID),
%	[{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail] = UpdateAbility,
	
% ตรวจสอบว่าจะมี Effect ของ Ability ใดเกิดหรือไม่
	% PlayerPid คือ Pid ของผู้เล่น เช่นผู้สั่งให้การ์ด รวมร่าง ใช้ skill แยกร่าง ...
check_any_ability_activate(Status, PlayerPid) ->
	% PlayerPid คือ Player เจ้าของ Subturn โจมตี
	put(s, Status),
	OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	% ตรวจสอบว่ามี Seal ใบไหน ที่จะเกิด Effect บ้างใน Status นี้ ถ้ามีจะคืนค่ามาเป็น 
	% [{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail]
	UpdateAbility = update_ability:update(Status, PlayerPid, OpponentPid),
	%smo_logger:fmsg("-------------###### ---------------Ability will Activate ~p Play are ~p~n---------------", [Status, UpdateAbility]),
	CheckAbilityNoSelect = check_ability_do_not_select(UpdateAbility),
	%smo_logger:fmsg("-------------###### CheckAbilityNoSelect --------------- ~p~n---------------", [CheckAbilityNoSelect]),
	put(x, CheckAbilityNoSelect),
	AbSelect = UpdateAbility -- CheckAbilityNoSelect,
	{ok, AtPid} = mnesia_play:get_game_data (self(), player_turn),
	DfPid = mnesia_play:get_opponent_pid (AtPid),
	{Own_Abi, Opp_Abi} = set_a(AbSelect, AtPid, DfPid, Status),
	List_Ab = Own_Abi ++ Opp_Abi,
		case List_Ab of
			[0,0] -> 
				set_ability_activate(Status, AtPid, DfPid, AbSelect);
			[1, _, _, _, _, _, _, 0 ] -> 
				set_ability_activate(Status, AtPid, DfPid, AbSelect);
			[0, 1, _, _, _, _, _, _ ] -> 
				set_ability_activate(Status, AtPid, DfPid, AbSelect);
			[1, _, _, _, _, _, _, 1|_] -> 
				Ab_All = set_ab_select(AbSelect),
				set_ability_activate(Status, AtPid, DfPid, Ab_All);
			_ ->
				case Own_Abi of
					[0] ->
						send_ability_to_client_op(Opp_Abi, DfPid);			
					_ ->
						send_ability_to_client_ow(Own_Abi, Opp_Abi, AtPid)
				end
		end.

set_ab_select(N) -> 
	set_ab_select(N, []).
set_ab_select([{{CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail], AbAll)->
	Ab = {{CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID},
		set_ab_select(Tail, [Ab|AbAll]);
set_ab_select([], AbAll) -> AbAll.
	
send_ability_to_client_ow(Own_Abi, Opp_Abi, AtPid) ->
	put(z, Opp_Abi),
	case Own_Abi of
	[1|_] ->
		[_Size, 1, CardOrder, CardID, AbilityNo, Zone, _Line] = Own_Abi,
		[CardSet, CardNo] = binary_to_list(CardID),
		Own_Ability = [1, CardOrder, CardSet, CardNo, AbilityNo, Zone],
		set_all_ability_activate(AtPid, Own_Ability);
	_ ->
		{ok, PlayerLists} = mnesia_play:get_game_data(self(), player_list),
			foreach( fun({PlayerPid, _}) ->
					case PlayerPid of
						AtPid ->
							gen_server:cast(PlayerPid, {send, [16#88 ,16#85]++Own_Abi});
						_ ->
							gen_server:cast(PlayerPid, {send, [16#88 ,16#85, 0]})
					end
				end, PlayerLists)
	end.

send_ability_to_client_op(Opp_Abi, AtPid) ->
	case Opp_Abi of
		[1|_] ->
		[_Size, 0, CardOrder, CardID, AbilityNo, Zone, _Line] = Opp_Abi,
		[CardSet, CardNo] = binary_to_list(CardID),
		Opp_Ability = [0, CardOrder, CardSet, CardNo, AbilityNo, Zone],
		set_all_ability_activate(AtPid, Opp_Ability);
		_ ->
			{ok, PlayerLists} = mnesia_play:get_game_data(self(), player_list),
			foreach( fun({PlayerPid, _}) ->
					case PlayerPid of
						AtPid ->
							gen_server:cast(PlayerPid, {send, [16#88 ,16#85]++Opp_Abi});
						_ ->
							gen_server:cast(PlayerPid, {send, [16#88 ,16#85, 0]})
					end
				end, PlayerLists)
	end.

%io:format("all potential ability ~p~n", [function_utility:qsort(UpdateAbility)]),
set_all_ability_activate(AtPid, Data) ->
	Status = get(s),
	AbilityNoSelect = get(x),
	DfPid = mnesia_play:get_opponent_pid(AtPid),
	Opp_Abi = get(z),
	erase(z),
		case Opp_Abi of
			[0] -> 
				UpdateAb = set_b(Data, AtPid, DfPid, Status),
				All_Ability = UpdateAb ++ AbilityNoSelect,
				erase(s),
				erase(x),
				set_ability_activate(Status, AtPid, DfPid, All_Ability);
			undefined ->
				UpdateAb = set_b(Data, AtPid, DfPid, Status),
				case get(d) of
					undefined ->
					All_Ability = UpdateAb++AbilityNoSelect;
					_->
					All_Ability = UpdateAb ++ get(d) ++ AbilityNoSelect
				end,
				erase(d),
				erase(s),
				erase(x),
				set_ability_activate(Status, AtPid, DfPid, All_Ability);	
			_ ->
				Data1 = set_b(Data, AtPid, DfPid, Status),
				put(d, Data1),
				send_ability_to_client_op(Opp_Abi, DfPid)
		end.
	
set_ability_activate(Status, _AtSub, _DfSub, All_Ability) ->
	%smo_logger:fmsg("$$$$$$$$$$$$$$$-------------set_ability_activate ~p~n", [All_Ability]),
	case All_Ability of
		% ถ้าไม่มีก็ไม่ต้องทำการใดๆ ให้ไปทำงานตามขั้นตอนของ PlatStatus นั้นๆ
		[] -> interfere_step:return_play (check_play_step);
		% ถ้ามีให้ส่งไปทำงานต่อไป
		_ -> 
			stack_pool:set_stack_option(self(), play_status, Status),
			%stack_pool:set_stack_option(self(), player, CardOwner),
			%stack_pool:set_stack_option(self(), opponent, OpponentPid),
			stack_pool:set_stack_option(self(), {all_potential_ability, Status}, All_Ability),
			stack_pool:set_stack_option(self(), all_potential_ability, All_Ability),
			check_ability_effect()
	end.

%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
check_ability_do_not_select(M) -> check_ability_do_not_select(M, []).
check_ability_do_not_select([{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail], ListDoNot) ->
	[{Owner, Player, Self, Other}] = do(qlc:q([
												{X#card_ability.owner_effect,
												X#card_ability.other_effect,
												X#card_ability.player_effect,
												X#card_ability.playerown_effect}||X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	ThisDoNot = do_not_need_select({{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}, [Owner, Player, Self, Other]) ++ ListDoNot,
	check_ability_do_not_select(Tail, ThisDoNot);
check_ability_do_not_select([], ListDoNot) -> ListDoNot.

do_not_need_select(_, []) -> [];
do_not_need_select(AbilityData, [{}|Tail]) -> do_not_need_select(AbilityData, Tail);
do_not_need_select(AbilityData, [[]|Tail]) -> do_not_need_select(AbilityData, Tail);
do_not_need_select(AbilityData, [{do_not_need_select, _, _}|Tail]) -> [AbilityData] ++ do_not_need_select(AbilityData, Tail);
do_not_need_select(AbilityData, [{{_, _, do_not_need_select}, _, _}|Tail]) -> [AbilityData] ++ do_not_need_select(AbilityData, Tail);
do_not_need_select(AbilityData, [{_, _, _}|Tail]) -> do_not_need_select(AbilityData, Tail).
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
set_effect(Effect, OwnAbi, OppAbi, Update, WhoseCard, Controller) ->
	case WhoseCard of
		at_subturn ->
			case Effect of
				{_,_,{owner_can_select_exact_target,_,_},_}-> {Update++OwnAbi, OppAbi};
				{_,_,{opponent_can_select_exact_target,_,_},_}-> {OwnAbi, Update++OppAbi};
				{_,_,{owner_select_exact_target,_,_},_}	-> {Update++OwnAbi, OppAbi};
				{_,_,{controller_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {Update++OwnAbi, OppAbi};
						opponent -> {OwnAbi, Update ++ OppAbi}
					end;
				{_,_,{controller_can_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {Update++OwnAbi, OppAbi};
						opponent -> {OwnAbi, Update ++ OppAbi}
					end;
				{_,_,{opponent_select_exact_target,_,_},_}	-> {OwnAbi, Update++OppAbi};
				{_,_,{owner_can_activate_ability,_,_},_}	-> {Update++OwnAbi, OppAbi};
				{_,_,{controller_can_activate_ability,_,_},_}	-> 
					case Controller of
						owner -> {Update++OwnAbi, OppAbi};
						opponent -> {OwnAbi, Update ++ OppAbi}
					end;
				{_,_,{uncontrol_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {OwnAbi, Update++OppAbi};
						opponent -> {Update ++ OwnAbi, OppAbi}
					end
			end;
		df_subturn ->
			case Effect of
				{_,_,{owner_can_select_exact_target,_,_},_}-> {OwnAbi, Update++OppAbi};
				{_,_,{opponent_can_select_exact_target,_,_},_}-> {Update++OwnAbi, OppAbi};
				{_,_,{owner_select_exact_target,_,_},_}	-> {OwnAbi, Update++OppAbi};
				{_,_,{controller_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {OwnAbi, Update++OppAbi};
						opponent -> {Update ++ OwnAbi, OppAbi}
					end;
				{_,_,{controller_can_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {OwnAbi, Update++OppAbi};
						opponent -> {Update ++ OwnAbi, OppAbi}
					end;
				{_,_,{opponent_select_exact_target,_,_},_}	-> {Update++OwnAbi, OppAbi};
				{_,_,{owner_can_activate_ability,_,_},_}	-> {OwnAbi, Update++OppAbi};
				{_,_,{controller_can_activate_ability,_,_},_}	-> {OwnAbi, Update++OppAbi};
				{_,_,{uncontrol_select_exact_target,_,_},_}	-> 
					case Controller of
						owner -> {Update++OwnAbi, OppAbi};
						opponent -> {OwnAbi, Update ++ OppAbi}
					end
			end
	end.

% แบ่งว่า Ability ไหนควรจะเป็นของผู้เล่นฝ่ายไหนเลือก
set_a(L, M, N, O) ->
		set_a(L, M, N, O, [], []).
set_a([{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail], AtSubPid, DfSubPid, Status, OwnAbi, OppAbi) ->
		%case attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, req) of
	AbilityNo = query_ability:no_of_ability(AbilityId),
	{Zone, Line} = 
		case CardZone of
			arena_zone -> 
				LineCard =
				case card_utility:get_card_option_field(PlayerOwnID, CardOrder, CardID, line, arena_zone) of
					% Seal
					{ok, LineCheck} -> LineCheck;
					% Mystic
					_ -> 2
				end,
				{0, LineCard};
			shrine_cards -> {1, 2};
			seal_deck -> {2, 2};
			mystic_deck -> {3, 2};
			hand_cards -> {4, 2};
			remove_cards -> {5, 2}
		end,
	% ถ้าการ์ด มีเจ้าของเป็นเจ้าของ Turn โจมตี
case PlayerOwnID of
	AtSubPid  ->
		case get(o) of
			[1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line] ->set_a(Tail, AtSubPid, DfSubPid, Status, OwnAbi, OppAbi);
		_ ->
			Update = [1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
			put(o, Update),
			case CardZone of
				arena_zone ->
					case attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, controller) of %ตรวจสอบผู้ควบคุมการ์ด
						% ผู้่ควบคุมคือ เจ้าของการ์ดเอง
						{PlayerOwnID, _, _} ->
							[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
							case EffectToAll of
								[]->
									%Update = [1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									set_a(Tail, AtSubPid, DfSubPid, Status, Update++OwnAbi, OppAbi);
								[{_, Effect}|_] ->
									% ถ้าการ์ด มีเจ้าของและผู้ควบคุมเป็นเจ้าของ Turn โจมตี
									%Update = [1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, at_subturn, owner),
									set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
							end;
					%case attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, req) of
						% ถ้าการ์ด มีเจ้าของเป็น เป็นเจ้าของ Turn โจมตี แต่ถูก ฝั่งตรงข้ามควบคุม
						{PlayerOppID, _, _} ->
							[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
							case EffectToAll of
								[]-> 
									%Update = [3, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									set_a(Tail, AtSubPid, DfSubPid, Status, Update++OwnAbi, OppAbi);
								[{_, Effect}|_] ->
									%Update = [3, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, at_subturn, opponent),
									set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
							end
						end;
				%case CardZone of
				_OtherZone -> 
					[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
							case EffectToAll of
								[]-> 
									%Update = [1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									set_a(Tail, AtSubPid, DfSubPid, Status, Update++OwnAbi, OppAbi);
								[{_, Effect}|_] ->
									%Update = [1, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, at_subturn, owner),
									set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
							end
				end
			end;
		% ถ้าการ์ด มีเจ้าของไม่ใช่เจ้าของ Turn โจมตี
	DfSubPid ->
		case get(o) of
			[0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line] ->set_a(Tail, AtSubPid, DfSubPid, Status, OwnAbi, OppAbi);
		_ ->
			Update = [0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
			put(o, Update),
			case CardZone of
				arena_zone ->
					case attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, controller) of %ตรวจสอบผู้ควบคุมการ์ด
						{PlayerOwnID, _, _} ->
							[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
								case EffectToAll of
									[]-> 
										%Update = [0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
										set_a(Tail, AtSubPid, DfSubPid, Status, OwnAbi, Update++OppAbi);
									[{_, Effect}|_] ->
										%Update = [0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
										{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, df_subturn, owner),
										set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
								end;
					%case attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, req) of %ตรวจสอบผู้ควบคุมการ์ด
						{PlayerOppID, _, _} ->
							[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
							case EffectToAll of
								[]-> 
									%Update = [2, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									set_a(Tail, AtSubPid, DfSubPid, Status, OwnAbi, Update++OppAbi);
								[{_, Effect}|_] ->
									%Update = [2, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
									{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, df_subturn, opponent),
									set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
							end
					end;
				%case CardZone of
				_OtherZone ->
					[{_, _, EffectToAll}] = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
					case EffectToAll of
						[]-> 
							%Update = [0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
							set_a(Tail, AtSubPid, DfSubPid, Status, OwnAbi, Update++OppAbi);
						[{_, Effect}|_] ->
							%Update = [0, CardOrder, <<CardID:16>>, AbilityNo, Zone, Line],
							{OwnerUpdate, OppUpdate} = set_effect(Effect, OwnAbi, OppAbi, Update, df_subturn, owner),
							set_a(Tail, AtSubPid, DfSubPid, Status, OwnerUpdate, OppUpdate)
					end
			end
		end
	end;
set_a([], _, _, _Status, OwnAbi, OppAbi) ->
	erase(o),
	OwnSize = round(length(OwnAbi)/6),
	OppSize = round(length(OppAbi)/6),
	Own_Abi = [OwnSize] ++ OwnAbi,
	Opp_Abi = [OppSize] ++ OppAbi,
	{Own_Abi, Opp_Abi}.
%-----------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------
set_b(W, X, Y, Z) ->
		set_b(W, X, Y, Z, []).
			% [0,1,1,112,1,0|0,2,1,112,1,0|0,5,1,112,1,0]
set_b([PlayerOwnID, CardOrder, CardSet, CardNo, AbilityNo, Zone|Tail], PlayerPid, OpponentPid, Status, Ability_List) ->
	%smo_logger:fmsg("{~p, ~p, ~p, ~p, ~p, ~p}~n", [PlayerOwnID, CardOrder, CardSet, CardNo, AbilityNo, Zone]),
	<<CardID:16>> = list_to_binary([CardSet, CardNo]),
	AbilityId = query_ability:id_of_ability(AbilityNo, CardID),
	case Zone of
		0 -> CardZone = arena_zone;
		1 -> CardZone = shrine_cards;
		2 -> CardZone = seal_deck;
		3 -> CardZone = mystic_deck;
		4 -> CardZone = hand_cards;
		5 -> CardZone = remove_cards
	end,
	{_, AtSubPid} = mnesia_play:get_game_data(self(), player_turn),
	DfSubPid = mnesia_play:get_opponent_pid(AtSubPid),
	case PlayerOwnID of
		% หมายถึงผู้เล่นที่เป็นเจ้าของ Subturn โจมตี
		0 ->
			% [{_,_,[{_,{_,_,{SelectType,_,_},_}}]}] = s_ability_effect:s_ability_effect({CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid),
			% case SelectType of
				% owner_can_select_exact_target-> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% owner_select_exact_target	-> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% controller_select_exact_target	 -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% opponent_select_exact_target	  -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% owner_can_activate_ability -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% controller_can_activate_ability	 -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid}
			% end,
			case length(AbilityId) of
				1 ->
					[A] =AbilityId,
					UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, A, AtSubPid},
					set_b(Tail, PlayerPid, OpponentPid, Status, [UpdateA|Ability_List]);
				_ ->
					UpdateA = set_c({{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid}),
					set_b(Tail, PlayerPid, OpponentPid, Status, UpdateA ++ Ability_List)
			end;			
		1 ->
			% [{_,_,[{_,{_,_,{SelectType,_,_},_}}]}] = s_ability_effect:s_ability_effect({CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid),
			% case SelectType of
				% {_,_,{owner_can_select_exact_target,_,_},_}-> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{owner_select_exact_target,_,_},_}	-> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{controller_select_exact_target,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{opponent_select_exact_target,_,_},_}	  -> UpdateA = {{CardZone, {OpponentPid, CardOrder, CardID}}, AbilityId, PlayerPid};
				% {_,_,{owner_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{controller_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid}
			% end,
			case length(AbilityId) of
				1 ->
					[A] = AbilityId,
					UpdateA = {{CardZone, {AtSubPid, CardOrder, CardID}}, A, DfSubPid},
					set_b(Tail, PlayerPid, OpponentPid, Status, [UpdateA|Ability_List]);
				_ ->
					UpdateA = set_c({{CardZone, {AtSubPid, CardOrder, CardID}}, AbilityId, DfSubPid}),
					set_b(Tail, PlayerPid, OpponentPid, Status, UpdateA ++ Ability_List)
			end
		% 2 -	>
			% [{_,_,[{_,{_,_,{SelectType,_,_},_}}]}] = s_ability_effect:s_ability_effect({CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid),
			% case SelectType of
				% {_,_,{owner_can_select_exact_target,_,_},_}-> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% {_,_,{owner_select_exact_target,_,_},_}	-> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% {_,_,{controller_select_exact_target,_,_},_}	 -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% {_,_,{opponent_select_exact_target,_,_},_}	  -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% {_,_,{owner_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid};
				% {_,_,{controller_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid}
			% end,
			% set_b(Tail, PlayerPid, OpponentPid, Status, [UpdateA|Ability_List]);
		% 3 ->
			% [{_,_,[{_,{_,_,{SelectType,_,_},_}}]}] = s_ability_effect:s_ability_effect({CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid),
			% case SelectType of
				% {_,_,{owner_can_select_exact_target,_,_},_}-> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{owner_select_exact_target,_,_},_}	-> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{controller_select_exact_target,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{opponent_select_exact_target,_,_},_}	  -> UpdateA = {{CardZone, {OpponentPid, CardOrder, CardID}}, AbilityId, PlayerPid};
				% {_,_,{owner_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid};
				% {_,_,{controller_can_activate_ability,_,_},_}	 -> UpdateA = {{CardZone, {PlayerPid, CardOrder, CardID}}, AbilityId, OpponentPid}
			% end,
			% set_b(Tail, PlayerPid, OpponentPid, Status, [UpdateA|Ability_List])
	end;
set_b([], _, _, _, Ability_List) -> Ability_List.
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
%เซ็ตค่า กรณี AbilityNo เดียวกัน แต่มีหลาย AbilityId
set_c(O) ->
	set_c(O,[]).
	
set_c({{CardZone, {DfSubPid, CardOrder, CardID}}, [AbilityId|Tail], AtSubPid}, Data) -> 
	Ab_id = {{CardZone, {DfSubPid, CardOrder, CardID}}, AbilityId, AtSubPid},
	set_c({{CardZone, {DfSubPid, CardOrder, CardID}}, Tail, AtSubPid}, Data++[Ab_id]);
	
set_c({{_CardZone, {_DfSubPid, _CardOrder, _CardID}}, [], _AtSubPid},Data) -> Data.
%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------
% ตรวจสอบว่า Effect ทีีเกิดคืออะไร
check_ability_effect() ->
	case stack_pool:get_last_stack(self(), all_potential_ability) of
		% เมื่อ generalize_effect_to_each_target ให้กับทุก AbilityId แล้ว ให้ไปทำงานตามขั้นตอนของ PlatStatus นั้นๆ
		{ok, []} -> 
			interfere_step:return_play (check_play_step);
		%{ok, [{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|T]} ->
		{ok, AllAbility} ->
			%smo_logger:fmsg("#############All potential ability are ~p~n", [AllAbility]),
			{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID} = Last = lists:last(AllAbility),
			stack_pool:set_stack_option(self(), all_potential_ability, AllAbility--[Last]),
			% สนใจเฉพาะ 1 AbilityId เท่านั้น
				% Fx = [{AbilityType, EnableCondition, [POwnEffect, PlayerEffect, SelfEffect, OtherEffect]}].
			Fx = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
			%smo_logger:fmsg("Card effect of {~p, ~p ~p} to check type is ~p~n", [PlayerOwnID, CardOrder, CardID, Fx]),
			check_ability_type(Fx);
			_ ->
				interfere_step:return_play (check_play_step)
	end.
	
% ยัง ตรวจสอบไม่ครบในกรณี ที่เป็น ตราบเท่าที่ .........และอื่นๆ
check_ability_type([{AbilityType, EnebleCondition, EffectToAll}]) ->
	stack_pool:set_stack_option(self(), this_ability_type_and_condition, {AbilityType, EnebleCondition}),
	stack_pool:set_stack_option(self(), this_ability_effect, EffectToAll),
	generalize_effect_to_each_target().
	
% ดูว่า แต่ละ ประเภทของเป้าหมาย ต้่องทำอะไรบ้าง 
		%(ประเภทของเป้าหมายคือ CardOwner, AnyPlayer, Card ที่ีเป็นเจ้าของ Ability และ การ์ด อื่นๆ)
		% อยู่ในรูปแบบ [{player_own_effect, PlayerOwnEffect}, {opponent_effect, OpponentEffect}, {self_card_effect, SelfEffect}, {other_cards_effect, OtherEffect}]
generalize_effect_to_each_target() ->
	case stack_pool:get_last_stack(self(), this_ability_effect) of
		{ok, []} -> check_ability_effect();
		{ok, [{WhoseFx, Effect}|T]} ->
			stack_pool:set_stack_option(self(), this_ability_effect, T),
			stack_pool:set_stack_option(self(), whose_effect, WhoseFx),
			check_select_case(Effect)
	end.

% ตรวจสอบว่า Effect ที่ีเกิดต้องเลือก ให้่มีการทำงาน หรือ มีการเลือกเป้าหมายหรือไม่ และส่งไปทำงาน ตามแต่ละกรณี
check_select_case({{CardOwner, CardOrder, CardID, AbilityId}, Target, {SelectType, SelectAmount, Effect}, Duration}) ->
	stack_pool:set_stack_option(self(), effect_before_select_activate, {{CardOwner, CardOrder, CardID, AbilityId}, Target, {SelectType, SelectAmount, Effect}, Duration}),
	case SelectType of
		do_not_need_select -> generalize_effect_to_each_target();%do_not_need_select();% ไม่ต้องเลือกเป้า
		owner_can_select_exact_target -> owner_can_select_exact_target();% เลือกว่าจะให้ Ability ทำงานหรือไม่ก่อนแล้วค่อย กลับมาเลือกว่าจะเลือกเป้าเป็นผู้เล่นฝั่งไหน
		opponent_can_select_exact_target -> opponent_can_select_exact_target();
		owner_select_exact_target -> owner_select_exact_target();% เลือกเป้าของ Ability
		controller_select_exact_target -> controller_select_exact_target();
		controller_can_select_exact_target -> controller_can_select_exact_target();
		opponent_select_exact_target -> opponent_select_exact_target();
		owner_can_activate_ability -> owner_activate_ability(); % เจ้าของการ์ดเลือกว่าจะ ให้ Ability ทำงานหรือไม่
		controller_can_activate_ability -> controller_activate_ability();
		uncontrol_select_exact_target -> uncontrol_select_exact_target();
		{check, _AbiIDCheck, _Select} -> generalize_effect_to_each_target()%do_not_need_select();% ไม่ต้องเลือกเป้า
	end.
% -----------------กรณี "owner_can_select_exact_target"-------------------------
% เจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่ ใน กรณีที่ต้องมีการเลือกเป้าด้วย
owner_can_select_exact_target() ->
	%{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	%{{CardOwner, CardOrder, CardID, _}, _, {_, _, Effect}, _} = Fx,
	{{CardOwner, _, CardID, _}, _, {_, _, _}, _} = Fx,
	%DisplayCode = dialog_text:text_code(Effect),
	%self() ! {activate_player_select_activation_ability, CardOwner, CardOwner, CardOrder, CardID, 16#00, DisplayCode}.
	stack_pool:set_stack_option(self(), select_from_function, owner_can_select_exact_target),
	gen_server:cast(self(), {activate_player_select_activation_ability, CardOwner, CardOwner, CardID, 16#00}). % Original
	
opponent_can_select_exact_target() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, _, CardID, _}, _, {_, _, _}, _} = Fx,
	PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
	stack_pool:set_stack_option(self(), select_from_function, opponent_can_select_exact_target),
	gen_server:cast(self(), {activate_player_select_activation_ability, PlayerOppID, CardOwner, CardID, 16#00}). % Original


controller_can_select_exact_target() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, CardOrder, CardID, _}, _, {_, _, _}, _} = Fx,
	PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
	case attribute_check:check_controller({CardOwner, CardOrder, CardID}, PlayerOppID, controller) of 
		{CardOwner, _, _} ->
			stack_pool:set_stack_option(self(), select_from_function, controller_can_select_exact_target),
			gen_server:cast(self(), {activate_player_select_activation_ability, CardOwner, CardOwner, CardID, 16#00});
		{PlayerOppID, _, _}->
			stack_pool:set_stack_option(self(), select_from_function, controller_can_select_exact_target),
			gen_server:cast(self(), {activate_player_select_activation_ability, PlayerOppID, CardOwner, CardID, 16#00})
	end.
	
% หลังจากเจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่ ตรวจสอบว่า เป็นการเลือกมาจาก Function ไหน
post_select_activation(PlayerPid, {Selecter, CardOwner}, Data) ->
	% PlayerPid = Pid ของ Process ทีส่ง Msg กลับมา
	% Selecter = Pid ของ Player ที่เป็นผู้เลือก
	case stack_pool:get_last_stack(self(), select_from_function) of
		{ok, owner_can_select_exact_target} -> post_owner_can_select_exact_target(PlayerPid, Selecter, Data);
		{ok, opponent_can_select_exact_target} -> post_opponent_can_select_exact_target(PlayerPid, Selecter, Data);
		{ok, controller_can_select_exact_target} -> post_controller_can_select_exact_target(PlayerPid, Selecter, Data);
		{ok, controller_activate_ability} -> post_controller_select_activate(PlayerPid, {Selecter, CardOwner}, Data);
		{ok, owner_activate_ability} -> post_owner_select_activate(PlayerPid, {Selecter, CardOwner}, Data)
	end.
% หลังจากเจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่
post_owner_can_select_exact_target(PlayerPid, Selecter, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> skip_all_ability_no();
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> to_owner_select_exact_target();
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	owner_can_select_exact_target()
	end.
	
post_opponent_can_select_exact_target(PlayerPid, Selecter, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> skip_all_ability_no();
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> to_opponent_select_exact_target();
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	opponent_can_select_exact_target()
	end.
	
post_controller_can_select_exact_target(PlayerPid, Selecter, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> skip_all_ability_no();
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> to_controller_select_exact_target();
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	controller_can_select_exact_target()
	end.
	
skip_all_ability_no() ->
	{ok, {{PlayerOwnID, CardOrder, CardID, AbilityId}, _, {_, _, _}, _}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	smo_logger:fmsg("~neffect before select activate {~p, ~p, ~p, ~p} ~n", [PlayerOwnID, CardOrder, CardID, AbilityId]),
	All = stack_pool:get_last_stack(self(), all_potential_ability),
	smo_logger:fmsg("~nAll potnetial Ability {~p} ~n", [All]),
	case stack_pool:get_last_stack(self(), all_potential_ability) of
		{ok, []} -> check_ability_effect();
		{ok, AllPoten} -> skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, AllPoten);
		_NoAllPoten -> check_ability_effect()
	end.
	% case stack_pool:get_last_stack(self(), all_potential_ability_to_skip) of
		% % Ability ที่มาจากการ์ดเดียวกัน ตรวจสอบต่อว่า มาจาก Ability Number เดียวกันหรือไม่
		% {ok, [{{_, {PlayerOwnID, CardOrder, CardID}}, NextAbilityId, _}|T]} ->
			% [AbilityNo] = do(qlc:q( [ X#card_ability.ability_no|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
			% AllAbilityId = do(qlc:q( [ X#card_ability.ability_id|| X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID, X#card_ability.ability_no =:= AbilityNo])),
			% case [NextAbilityId] -- AllAbilityId of
					% [] ->
						% stack_pool:set_stack_option(self(), all_potential_ability, T),
						% stack_pool:set_stack_option(self(), all_potential_ability_to_skip, T),
						% skip_all_ability_no();
					% _Oher -> 
						% smo_logger:fmsg("~n do not skip this ability ~p~n", [_Other]),
						% stack_pool:set_stack_option(self(), all_potential_ability_to_skip, T),
						% skip_all_ability_no()
			% end;
		% _AbilityOtherCard -> smo_logger:fmsg("~nall ability to skill ~p~n",  [_AbilityOtherCard]),
			% check_ability_effect()
	% end.
	
skip_ability_id(_PlayerOwnID, _CardOrder, _CardID, _AbilityId, []) -> check_ability_effect();
skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, [{{_, {PlayerOwnID, CardOrder, CardID}}, NextAbilityId, _}|Ability]) ->
	[AbilityNo] = do(qlc:q( [ X#card_ability.ability_no|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	AllAbilityId = do(qlc:q( [ X#card_ability.ability_id|| X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID, X#card_ability.ability_no =:= AbilityNo])),
	case [NextAbilityId] -- AllAbilityId of
		[] ->
			stack_pool:set_stack_option(self(), all_potential_ability, Ability),
			stack_pool:set_stack_option(self(), all_potential_ability_to_skip, Ability),
			skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, Ability);
		_Other -> 
			smo_logger:fmsg("~n do not skip this ability ~p~n", [_Other]),
			stack_pool:set_stack_option(self(), all_potential_ability_to_skip, Ability),
			skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, Ability)
	end;
skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, [{{_, {_PlayerOwnID, _CardOrder, _CardID}}, _, _}|Ability]) -> skip_ability_id(PlayerOwnID, CardOrder, CardID, AbilityId, Ability).
	
	
	
	
	
% กำหนดค่า ให้กับ Ability ใหม่ 
	%จากเดิม player_can_select_exact_target เป็น player_select_exact_target 
	%เพื่อวนกลับไปเลือกเป้าหมายของ Ability
to_owner_select_exact_target() ->
	{ok, Remain} = stack_pool:get_last_stack(self(), this_ability_effect),
	{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {_, SelectAmount, Effect}, Duration} = Fx,
	NewFx = {{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {owner_select_exact_target, SelectAmount, Effect}, Duration},
	stack_pool:set_stack_option(self(), this_ability_effect, [{WhoseFx, NewFx}]++Remain),
	generalize_effect_to_each_target().

to_opponent_select_exact_target() ->
	{ok, Remain} = stack_pool:get_last_stack(self(), this_ability_effect),
	{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {_, SelectAmount, Effect}, Duration} = Fx,
	NewFx = {{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {opponent_select_exact_target, SelectAmount, Effect}, Duration},
	stack_pool:set_stack_option(self(), this_ability_effect, [{WhoseFx, NewFx}]++Remain),
	generalize_effect_to_each_target().
	
to_controller_select_exact_target() ->
	{ok, Remain} = stack_pool:get_last_stack(self(), this_ability_effect),
	{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {_, SelectAmount, Effect}, Duration} = Fx,
	NewFx = {{CardOwner, CardOrder, CardID, AbilityId}, TargetPlayer, {controller_select_exact_target, SelectAmount, Effect}, Duration},
	stack_pool:set_stack_option(self(), this_ability_effect, [{WhoseFx, NewFx}]++Remain),
	generalize_effect_to_each_target().

% -----------------กรณี "owner_select_exact_target"-------------------------
owner_select_exact_target() ->
	{ok, {{CardOwner, CardOrder, CardID, _}, Target, {_, SelectAmount, Effect}, _}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	DisplayCode = dialog_text:text_code(Effect),
	stack_pool:set_stack_option(self(), who_select, CardOwner),
	case SelectAmount of
		{Type, Value} ->
			gen_server:cast(self(), {select_ability_target, CardOwner, CardOwner, CardOrder, CardID, 1, Value, DisplayCode, Target});
		_ ->
			gen_server:cast(self(), {select_ability_target, CardOwner, CardOwner, CardOrder, CardID, 0, SelectAmount, DisplayCode, Target})
	end.
	
controller_select_exact_target() ->
	%{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	{ok, {{CardOwner, CardOrder, CardID, _}, Target, {_, SelectAmount, Effect}, _}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	DisplayCode = dialog_text:text_code(Effect),
	stack_pool:set_stack_option(self(), who_select, CardOwner),
	PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UnconPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, PlayerOppID, controller),
	gen_server:cast(self(), {select_ability_target, ControllerPid, CardOwner, CardOrder, CardID, 0, SelectAmount, DisplayCode, Target}). % Original
	
opponent_select_exact_target() ->
	{ok, {{CardOwner, CardOrder, CardID, _}, Target, {_, SelectAmount, Effect}, _}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	DisplayCode = dialog_text:text_code(Effect),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	stack_pool:set_stack_option(self(), who_select, OppPid),
	gen_server:cast(self(), {select_ability_target, OppPid, CardOwner, CardOrder, CardID, 0, SelectAmount, DisplayCode, Target}).

uncontrol_select_exact_target() ->
	{ok, {{CardOwner, CardOrder, CardID, _}, Target, {_, SelectAmount, Effect}, _}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	DisplayCode = dialog_text:text_code(Effect),
	stack_pool:set_stack_option(self(), who_select, CardOwner),
	PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UnconPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, PlayerOppID, controller),
	gen_server:cast(self(), {select_ability_target, _UnconPid, CardOwner, CardOrder, CardID, 0, SelectAmount, DisplayCode, Target}).
% หลังจากเลือกเป้าหมายของ Ability แล้ว นำ Effect มากำหนดให้เป้าหมายที่เลือก
set_effect_to_selected_target(PlayerPid, PlayPlayerPid, [SelectAmount | Data]) ->
	case PlayPlayerPid of
		PlayerPid ->
			{ok, Status} = stack_pool:get_last_stack(self(), play_status),
			{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
			{ok, {{CardOwner, CardOrder, CardID, AbilityId}, _, {_, _, Effect}, Duration}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
			{ok, Select} = stack_pool:get_last_stack(self(), who_select),
			%Target = reverse_data_to_card(CardOwner, Data),
			Target = reverse_data_to_card(PlayPlayerPid, Data),
			{ok, A_Type_Cond} = stack_pool:get_last_stack(self(), this_ability_type_and_condition),
			DisplayCode = dialog_text:text_code(Effect),
			stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, {by_selected, {Select, 1, SelectAmount}}}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
			gen_server:cast(self(), {update_ability_target, PlayerPid, DisplayCode, [SelectAmount | Data]});
		_ -> owner_select_exact_target()
	end.
% -----------------กรณี "owner_can_activate_ability"-------------------------
% เจ้าของการ์ดเลือกว่าจะให้ Ability ทำงานหรือไม่
owner_activate_ability() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	%{{CardOwner, CardOrder, CardID, _}, _, {_, _, Effect}, _} = Fx,
	{{CardOwner, _, CardID, _}, _, {_, _, _}, _} = Fx,
	%DisplayCode = dialog_text:text_code(Effect),
	%self() ! {activate_player_select_activation_ability, CardOwner, CardOwner, CardOrder, CardID, 16#00, DisplayCode}.
	stack_pool:set_stack_option(self(), select_from_function, owner_activate_ability),
	gen_server:cast(self(), {activate_player_select_activation_ability, CardOwner, CardOwner, CardID, 16#00}). % Original

controller_activate_ability() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	%{{CardOwner, CardOrder, CardID, _}, _, {_, _, Effect}, _} = Fx,
	{{CardOwner, CardOrder, CardID, _}, _, {_, _, _}, _} = Fx,
	PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UnconPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, PlayerOppID, controller),
	%DisplayCode = dialog_text:text_code(Effect),
	%self() ! {activate_player_select_activation_ability, CardOwner, CardOwner, CardOrder, CardID, 16#00, DisplayCode}.
	stack_pool:set_stack_option(self(), select_from_function, controller_activate_ability),
	gen_server:cast(self(), {activate_player_select_activation_ability, ControllerPid, CardOwner, CardID, 16#00}). % change 20081230

% หลังจากที่ผู้ควบคุมการ์ดเลือกว่าจะให้่ Ability ทำงานหรือไม่แล้ว
post_controller_select_activate(PlayerPid, {Selecter, CardOwner}, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> check_set_effect(0, {Selecter, CardOwner});
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> check_set_effect(1, {Selecter, CardOwner});
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	controller_activate_ability()
	end.
	
% หลังจากที่เจ้าของการ์ดเลือกว่าจะให้่ Ability ทำงานหรือไม่แล้ว
post_owner_select_activate(PlayerPid, {Selecter, CardOwner}, Data) ->
	case PlayerPid of
		Selecter -> 
			case Data of
				%ผู้เล่นเลือกว่า จะไม่ให้ Ability ทำงาน ก็ผ่าน AbilityNo (ผ่านทัง Number ไม่ใช่ Id) นี้ไปเลย
				[0] -> check_set_effect(0, {Selecter, CardOwner});
				%ผู้เล่นเลือกว่า จะให้ Ability ทำงาน ให้ไป Set ค่าเพื้อ เลือกเป้าสำหรับ Ability นี้ต่อ
				[1] -> check_set_effect(1, {Selecter, CardOwner});
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	owner_activate_ability()
	end.

% ดูว่าเจ้าของการ์ดเลือกให้ Ability ทำงานหรือไม่ ถ้า ไม่ใช่ ดู case HaveActivate of "0" แต่ถ้่าใช่ ให้ดู case "1"
check_set_effect(HaveActivate, {Selecter, CardOwner}) ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	%{{CardOwner, CardOrder, CardID, AbilityId}, Target, {_, _, InitFx}, _} = Fx,
	{{CardOwner, _, _, _}, _, {_, _, InitFx}, Duration} = Fx,
	case HaveActivate of
		0 -> skip_all_ability_no();
		1 ->
			case InitFx of
				[{action, can_compare_df_to_attacker_power}] -> gen_server:cast(self(), {player_select_ability, Selecter, CardOwner, 0});
				[{action, can_compare_to_at_or_df_of_attacked}] -> gen_server:cast(self(), {player_select_ability, Selecter, CardOwner, 2});
				[{action,{show_opponent_card,all}}] -> gen_server:cast(self(), {player_select_ability, Selecter, CardOwner, 3});
				[{action, can_compare_df_to_target_power}] -> gen_server:cast(self(), {player_select_ability, Selecter, CardOwner, 4});
				_ -> 
					{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
					{{CardOwner, CardOrder, CardID, AbilityId}, Target, {_, _, Effect}, Duration}  = Fx,
					{ok, Status} = stack_pool:get_last_stack(self(), play_status),
					{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
					{ok, A_Type_Cond} = stack_pool:get_last_stack(self(), this_ability_type_and_condition),
					stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
					generalize_effect_to_each_target()
			end
			% ส่งไปบอก Client ให้เลือก ความสามารถตามกรณี ถ้า 0 ใช้ Dialog ที่หน้า Client ไม่เหมือนกันกับ 2
	end.
% หลังจาก เจ้่าของการ์ดเลือก ความสามารถแล้ว
% ส่งไปบอกฝั่งตรงข้ามว่าเจ้าของการ์ดเลือกความสามารถอะไร
update_opponent_what_selected(PlayerPid, AbilitySelect) ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{_, _, {_, _, Effect}, _} = Fx,
	InterestAbility =
	case Effect of
		[{action, can_compare_df_to_attacker_power}] -> 0;
		[{action, can_compare_to_at_or_df_of_attacked}] -> 2;
		[{action,{show_opponent_card,all}}] -> 3;
		[{action, can_compare_df_to_target_power}] -> 4
	end,
	post_owner_select_effect(PlayerPid, InterestAbility, AbilitySelect).

% เมื้อกลับมาจากบอกฝั่งตรงข้ามแล้ว...
post_owner_select_effect(PlayerPid, InterestAbility, AbilitySelect) ->
	{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{{CardOwner, CardOrder, CardID, AbilityId}, Target, {_, _, Effect}, Duration}  = Fx,
	{ok, Status} = stack_pool:get_last_stack(self(), play_status),
	{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
	{ok, A_Type_Cond} = stack_pool:get_last_stack(self(), this_ability_type_and_condition),
	case InterestAbility of
		0 ->	case AbilitySelect of
				0 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_target, {power, power}}}], 0}]);
				1 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_target, {power, df}}}], 0}]);
				_ -> io:format("Add other ability select -- ~p -- ~n", [AbilitySelect])
			end;
		2 ->
			case AbilitySelect of
				0 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_attack, {power, at}}}], 0}]);
				1 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_attack, {power, df}}}], 0}]);
				_ -> io:format("Add other ability select -- ~p -- ~n", [AbilitySelect])
			end;
		3 ->
			stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]);
		4 ->
			case AbilitySelect of
				0 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_attack, {power, power}}}], 0}]);
				1 ->	stack_pool:add_stack_option_field(self(), {effect_to_target, Status}, [{{WhoseFx, assigned}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  [{combat, {when_attack, {df, power}}}], 0}]);
				_ -> io:format("Add other ability select -- ~p -- ~n", [AbilitySelect])
			end;
		_ -> io:format("Add other ability -- ~p -- ~n", [InterestAbility])
	end,
	gen_server:cast(self(), {update_select_ability, PlayerPid, InterestAbility, AbilitySelect}).
%------------------------------------------------------------------%
%Function อื่นๆ 
%------------------------------------------------------------------%
reverse_data_to_card(_, []) -> [];
reverse_data_to_card(PlayerPid, [WhoseCard, CardOrder, B1, B2|T]) ->
	OppPid = mnesia_play:get_opponent_pid(PlayerPid),
	<<CardID:16>> = <<B1, B2>>,
	case WhoseCard of
		1 -> [{PlayerPid, CardOrder, CardID}]++reverse_data_to_card(PlayerPid, T);
		_ -> [{OppPid, CardOrder, CardID}]++reverse_data_to_card(PlayerPid, T)
	end.
%++++++++++++++++++++++++++++++++++%
% หลังจากผ่านช่วง Interfere ของ การกระทำใดๆ กลับมาทำงาน ตาม Function ต่างๆ ข่้างล่างนี้
%++++++++++++++++++++++++++++++++++%

% ตรวจสอบว่า Ability ถูกต้องตามเงื่อนไขในการทำงานหรือไม่ ถ้าเงื่อนไขไม่ครบ Ability นั้นจะไม่ทำงาน
verify_ability_condition(Status) ->
	%io:format("---------------------------------------verify_ability_condition(~p), ~n", [Status]),
	%smo_logger:fmsg("verify_ability_condition {effect_to_target, ~p} ###### ~p", [Status, stack_pool:get_last_stack(self(), {effect_to_target, Status})]),
	case stack_pool:get_last_stack(self(), {effect_to_target, Status}) of
		% เมื่อตรวจสอบ Effect และเป้าหมายครบแล้ว ให้ไป Activate Effect ที่เหลืออยู่
		{ok, []} -> interfere_step:return_play(check_play_step);
		{ok,  [{{WhoseFx, SelectType}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}|T]} ->
			stack_pool:set_stack_option(self(), {effect_to_target, Status}, T),
			CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่
			AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			case function_utility:is_contain([{ability, {loss, [all]}}], AllFx) of
				[] -> 
					case s_ability:condition_check({CardZone, {CardOwner, CardOrder,  CardID}}, AbilityId, OpponentPid, Status) of
						[ok, ok, ok, ok] -> verify_ability_target(Status, {{WhoseFx, SelectType}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration});
						_Mismatch ->
							smo_logger:fmsg("ability condition mismatch case ~p~n", [_Mismatch]),
							verify_ability_condition(Status)
					end;
				[{ability, {loss, [all]}}] -> verify_ability_condition(Status)
			end;
		_ -> interfere_step:return_play(check_play_step)
	end.
	

%[{player_own_effect, PlayerOwnEffect}, {opponent_effect, OpponentEffect}, {self_card_effect, SelfEffect}, {other_cards_effect, OtherEffect}]
verify_ability_target(Status, {{WhoseFx, SelectType}, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}) ->
	AbilityTarget = 
	case WhoseFx of
		player_own_effect ->
			case s_ability_target:playerown_target_check(AbilityId) of
				1 -> [CardOwner];
				_ -> []
			end;
		any_player_effect ->
			PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
			[s_ability_target:playeropp_target_check({CardOwner, CardOrder, CardID}, PlayerOppID, AbilityId)];	
		self_card_effect -> 
			case s_ability_target:owner_target_check(AbilityId) of
				1 -> [{CardOwner, CardOrder, CardID}];
				_ -> []
			end;
		other_cards_effect -> 
			CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
			PlayerOppID = mnesia_play:get_opponent_pid(CardOwner),
			s_ability_target:other_target_check({CardZone, {CardOwner, CardOrder, CardID}}, AbilityId, PlayerOppID)
	end,
	smo_logger:fmsg("ability verify target ~p and Target Select are ~p~n", [AbilityTarget, Target]),
	case AbilityTarget of
		[] -> % ถ้าเป็น Ability ที่ไม่ใช่ continuous ถ้าไม่พบ Target ให้ข้าม Effect ที่เกิดกับ Target นี้ไปเลย
			case A_Type_Cond of
				{y, _} -> 
					stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, AbilityTarget,  Effect, Duration}]),
					verify_ability_condition(Status);
				_ -> verify_ability_condition(Status)
			end;
		_NotEmpty1 ->
			case Target--AbilityTarget of
				[] -> % target ก่อน Interfere กับ Target หลัง Interfere เหมือนกัน ไม่ต้องเลือกใหม่
					stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
					verify_ability_condition(Status);
				_NotEmpty2 ->
					case SelectType of
						{by_selected, {Select, SelectWay, SelectAmount}} -> %เลือกเป้าใหม่
							stack_pool:set_stack_option(self(), effect_before_reselect_target, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
							stack_pool:set_stack_option(self(), play_status, Status),
							reselect_target({Select, SelectWay, SelectAmount}, CardOwner, CardOrder, CardID, Effect, AbilityTarget);
						assigned -> 
							stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, AbilityTarget,  Effect, Duration}]),
							verify_ability_condition(Status);
						_Other ->
							io:fomat("other case of select type ~p~n", [_Other])
					end
			end
	end.

reselect_target({Select, SelectType, SelectAmount}, CardOwner, CardOrder, CardID, Effect, AbilityTarget) ->
	stack_pool:set_stack_option(self(), reselect_target, yes),
	DisplayCode = dialog_text:text_code(Effect),
	gen_server:cast(self(), {select_ability_target, Select, CardOwner, CardOrder, CardID, SelectType, SelectAmount, DisplayCode, AbilityTarget}).

% เมือเลือกเป้า ใหม่เสร็จแล้ว ให้ กลับมาทำ Function นี้แล้วกลับไป ตรวจสอบ Condtion ของ Effect ถัดไป
add_all_fx_to_reselect_target(PlayerPid, PlayPlayerPid, [_ | Data]) ->
	{ok, Status} = stack_pool:get_last_stack(self(), play_status),
	case PlayPlayerPid of
		PlayerPid ->
			{ok, Fx} = stack_pool:get_last_stack(self(), effect_before_reselect_target),
			[{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, _,  Effect, Duration}] = Fx,
			Target = reverse_data_to_card(CardOwner, Data),
			stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
			verify_ability_condition(Status);
		_ -> io:format("--xxx-- Player select target is out of range ~n"),
			verify_ability_condition(Status)
	end.
