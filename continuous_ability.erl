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
-module(continuous_ability).
-compile(export_all).
%-export().
-import (mnesia_table, [do/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("s_ability.hrl").
-include_lib("record.hrl").
-include_lib("mystic_ability.hrl").

% สำหรับเช็คว่า มี Ability Number ไหนที่เป็น Continuous บ้่างใน Subturn แรก
check_continuous() ->
	{ok, [{Player1, _}, {Player2, _}]} = mnesia_play:get_game_data(self(), player_list),
	Cards= skill_card_list:player_zone_list({null, [seal_deck, mystic_deck, hand_cards], all}, {Player1, Player2}),
	%smo_logger:fmsg("card form hand and mystic deck and seal deck are ~p~n", [Cards]),
	%Mystics = skill_card_list:player_zone_list({null, [seal_deck, hand_cards], mystic}, {Player1, Player2}),
	Continuous = do(qlc:q( [X#continue_condition.card_id || X <- mnesia:table(continue_condition)])),
	ConID = get_continuous(Cards, Continuous),
	%smo_logger:fmsg("continuous id got are ~p~n", [ConID]),
	mnesia_play:set_game_data(self(), continue_ability, ConID).
	
get_continuous([], _) -> [];
get_continuous([{_Zone, {OwnerPid, CardOrder, CardID}}|Tail], Continuous) ->
	case [CardID] -- Continuous of
		[] -> 
			% การ์ด 1 ใบในสนาม ดูว่า มี AbilityNo อะไรที่เป็น Continuous บ้่าง
			ContinuousNo = 
				case mnesia_odbc:is_seal_card(CardID) of
					is_seal -> do(qlc:q( [{X#card_ability.ability_no, X#card_ability.ability_id} || X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID, X#card_ability.is_until =:= y]));
					is_not_seal -> 
						do(qlc:q( [{X#mystic_ability.m_ability_number, X#mystic_ability.m_ability_id}|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= CardID, X#mystic_ability.continuous_type =:= y]))
				end,
			%smo_logger:fmsg("continuous number got are ~p~n", [ContinuousNo]),
			distribute_id({OwnerPid, CardOrder, CardID}, ContinuousNo) ++ get_continuous(Tail, Continuous);
		_ -> get_continuous(Tail, Continuous)
	end.
	
distribute_id(_, []) -> [];
% ในกรณีที่มีหลาย AbilityNo ให้กระจาย แต่ ละ AbilityNo ไปแต่ละ Tuple และให้ Target ที่จะได้รับ Effect จาก AbilityID นี้เป็น []
distribute_id({CardHolderId, CardOrder, CardID}, [{AbilityNo, AbilityID}|Tail]) ->
	[{{CardHolderId, CardOrder, CardID, AbilityNo, AbilityID}, inactive, []}] ++ distribute_id({CardHolderId, CardOrder, CardID}, Tail).
%----------------------------------------------------------------------------------------
%----------------------สำหรับ เช็คว่า มีการ์ดใบไหนที่จะได้รับ Effect บ้าง---------------------
% Function เริ่่มต้นในการทำงาน Continuous Ability
check_continuous_target() ->
	%put(add_effect, []),
	stack_pool:set_stack_option(self(), add_effect, []),
	%put(remove_effect, []),
	stack_pool:set_stack_option(self(), remove_effect, []),
	%put(check_value, []),
	stack_pool:set_stack_option(self(), check_value, []),
	stack_pool:set_stack_option(self(), effect_change, no),
	{ok, AllConID} = mnesia_play:get_game_data(self(), continue_ability),
	%io:format("All Continuous Condition ~p~n", [AllConID]),
	NewAllCont = renew_continuous_activate(AllConID),
	%io:format("All Active Condition ~p~n", [NewAllCont]),
	mnesia_play:set_game_data(self(), continue_ability, NewAllCont),
	activate_continuous_ability().

% เลือกเอาแต่ละ Ability มา เช็คแล้วเลือกเอาเฉพาะ Ability ที่ Active แล้ว
renew_continuous_activate([]) -> 
			% %RemoveFX = get(remove_effect),
			% {ok, RemoveFX} = stack_pool:get_last_stack(self(), remove_effect), 
			% %AddFX = get(add_effect),
			% {ok, AddFX} = stack_pool:get_last_stack(self(), add_effect),
			% %CheckFx = get(check_value),
			% {ok, CheckFX} = stack_pool:get_last_stack(self(), check_value),
			% revise_effect_put([{remove_effect, RemoveFX}, {add_effect, AddFX}, {check_value, CheckFx}]), 
	[];
renew_continuous_activate([{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, inactive, Receiver}|Tail]) -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, inactive, Receiver}] ++ renew_continuous_activate(Tail);
% % เลือกเอาแต่ละ Ability มา เช็คแล้วเลือกเอาเฉพาะ Ability ที่ Active แล้ว
% Ability ที่ check_value แล้ว จะต้องเข้าไปทำงาน ละเอียดใน check_id_target แล้ว set_stack_option(self(), check_value  หรือ remove_effect ในนั้น
renew_continuous_activate([{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, check_value, Receiver}|Tail]) ->
	cheK_id_target({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Receiver}, check_value) ++ renew_continuous_activate(Tail);
% % เลือกเอาแต่ละ Ability มา เช็คแล้วเลือกเอาเฉพาะ Ability ที่ Active แล้ว
% Ability ที่ Active แล้ว จะต้องเข้าไปทำงาน ละเอียดใน check_id_target แล้ว set_stack_option(self(), active หรือ remove ในนั้น
renew_continuous_activate([{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, active, Receiver}|Tail]) ->
	cheK_id_target({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Receiver}, active) ++renew_continuous_activate(Tail);
renew_continuous_activate([{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, check_condition_value, Receiver}|Tail]) ->
	cheK_id_target({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Receiver}, check_condition_value) ++ renew_continuous_activate(Tail).


% revise_effect_put(Fx) ->
	% lists:foreach(
								% fun({FxType, FxValue}) ->
									% case FxValue of
										% undefined -> stack_pool:set_stack_option(self(), FxType, []),;
										% _ -> ""
									% end
								% end, Fx).

% เช็คว่า มีการ์ดใบไหนบ้าง ที่เป็น Target ของแต่ละ Ability Id
cheK_id_target([], _) ->
	case stack_pool:get_last_stack(self(), effect_change) of
		{ok, _} -> do_nothing;
		_ -> stack_pool:set_stack_option(self(), effect_change, no)
	end,
	[];
	% case get(effect_change) of
		% undefined -> put(effect_change, no);
		% _ -> ""
	% end,	[];
% ReceivedTarget = [{4, Target}, {5, BeyondTarget}]
cheK_id_target({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}, Check) ->
	OppID = mnesia_play:get_opponent_pid(CardOwner),
	% ตรวจสอบว่า Condition ของ Ability ถูกต้องหรือไม่
	CondCheck = check_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID),
	%smo_logger:fmsg("condition checked of {~p, ~p, ~p, ~p, ~p} is ~p~n", [CardOwner, CardOrder, CardID, AbilityNo, AbilityID, CondCheck]),
	case CondCheck of
		[ok, ok, ok, ok] -> % ถ้า condition ถูกต้อง
			% ตรวจสอบต่อว่า ได่รับ Effect สูญเสีย Ability หรือไม่
			FxRequire =  [{ability, {loss, [all]}}],
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			case function_utility:is_contain(FxRequire, CardFx) of
				[] ->
					% check เป้าหมายทั้งหมดที่มีความน่าจะเป็นที่จะได้รับ Effect 
					CheckReceive = 
					case mnesia_odbc:is_seal_card(CardID) of 
						is_seal -> check_seal_terget({CardOwner, CardOrder, CardID}, OppID, AbilityID);
						is_not_seal -> 
							CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
							check_mystic_target({CardZone, {CardOwner, CardOrder, CardID}}, OppID, AbilityID, ReceivedTarget)
					end,
					WilReceive = function_utility:check_target_cancel_any({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, CheckReceive),
					%io:format("card receive effect ~p~n", [WilReceive]),
					% อาจมีการ์ดบางใบถูก นำ Effect ออกจากการ์ดให้ เก็บไว้แล้วจะถูก ดำเนิินการต่อไป
					RemoveEffect =
					case WilReceive of
						[] -> compare_target(ReceivedTarget, remove_effect);
						_ -> compare_target(ReceivedTarget, WilReceive, remove_effect)
					end,
					put_ability_change(ReceivedTarget, WilReceive),
					stack_pool:set_stack_option(self(), remove_effect, get_effect(remove_effect) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, RemoveEffect}]),
					
					% อาจมีการ์ดบางใบจะได้รับ Effect ให้ เก็บไว้แล้วจะถูก ดำเนิินการต่อไป
					AddEffect = 
					case WilReceive of
						[] -> compare_target(ReceivedTarget, add_effect);
						_ -> compare_target(ReceivedTarget, WilReceive, add_effect)
					end,
					
					case Check of
						% กรณี active คือ การได้รับ Effect ธรรมดา คือการดได้รับไปแล้ว Effect นั้น จะไม่มีการเปลี่ยนแปลง 
						% ทำได้แค่เพียงลบ Effect ออกเท่านั้น เมื่อ Condition เมื่อ CondCheck = check_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID), ไม่เท่ากับ [ok, ok, ok, ok] 
						active ->
								put_ability_change(ReceivedTarget, WilReceive),
								stack_pool:set_stack_option(self(), add_effect, get_effect(add_effect) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, AddEffect}]);

						% กรณี check_value คือ การได้รับ Effect แบบที่จะมีการเปลี่ยนแปลงค่าได้ คือการใดได้รับไปแล้ว Effect นั้น จะเปลี่ยนแปลงค่าได้ 
						% และจะลบ Effect ออก เมื่อ Condition เมื่อ CondCheck = check_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID), ไม่เท่ากับ [ok, ok, ok, ok]
						check_value ->
							CheckValue =
							case WilReceive of
								[] -> compare_target(ReceivedTarget, check_value);
								_ -> compare_target(ReceivedTarget, WilReceive, check_value)
							end,							
							put_ability_change(ReceivedTarget, WilReceive),
							stack_pool:set_stack_option(self(), check_value, get_effect(check_value) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, CheckValue}]);
							
						% กรณี check_condition_value คือ การที่การ์ดที่เป็นเป้าหมาย จะ เปลี่ยนแปลงค่าพลัง ก็ต่อเมื่อ 
						% CondCheck = check_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID), ไม่เท่ากับ [ok, ok, ok, ok] ไม่เท่ากับ [ok, ok, ok, ok]
						% แต่ ถ้า ยังเป็น [ok, ok, ok, ok] อยู่ จะไม่มีการเปลี่ยนแปลง
						check_condition_value -> 
							%stack_pool:set_stack_option(self(), add_effect, get_effect(add_effect)),
								%io:format("receive target [{~p, ~p, ~p, ~p, ~p}, ~p]~n", [CardOwner, CardOrder, CardID, AbilityNo, AbilityID, WilReceive]),
							stack_pool:set_stack_option(self(), check_value, get_effect(check_value) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, WilReceive}])
					end,
					% ส่วนใบที่ไม่ได้อยู่ในการ Remove หรือ Add คือใบที่ ได้รับ Effect ไปแล้วและยังคงได้รับอยู่ พวกนี้ไม่ต้องทำอะไร
					[{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Check, WilReceive}];
				% กรณี Check ได้ว่ามีการได้รับ Effect ให้สูญเสีย Ability 
				_ ->
				% Target ที่เคยได้รับ Effect ไป จะต้องถูก Remove Effect ออก
				stack_pool:set_stack_option(self(), remove_effect, get_effect(remove_effect) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]),
				% stack_pool:set_stack_option(self(), add_effect, get_effect(add_effect)),
				% stack_pool:set_stack_option(self(), check_value, get_effect(check_value)),
				put_ability_change(ReceivedTarget, []),
				OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
				ActiveCon = active_condition_check({CardOwner, CardOrder, CardID}, OpponentPid, AbilityID),
				case ActiveCon of
					[ok] -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Check, []}];
					_ -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, inactive, []}]
				end
			end;
		_Other -> %กรณี ไม่ถูก ไม่เป็น [ok, ok, ok, ok]
			%stack_pool:set_stack_option(self(), add_effect, get_effect(add_effect)),
			% มีการเปลี่ยนแปลงของ Effect ดังนั้น ต้องทำต่อไปจนกว่า Effect จะไม่มีการเปลี่ยนแปลง
			%io:format("condition checked of {~p, ~p, ~p, ~p, ~p} is ~p~n", [CardOwner, CardOrder, CardID, AbilityNo, AbilityID, _Other]),
			case Check of
				% Target ที่เคยได้รับ Effect ไป จะต้องถูก Remove Effect ออก
				active ->
					put_ability_change(ReceivedTarget, []);
				% Target ที่เคยได้รับ Effect ไป จะต้องถูก Remove Effect ออก
				check_value ->
					put_ability_change(ReceivedTarget, []);
				% Target ที่เคยได้รับ Effect ไป จะต้องถูก Check Effect อีกครั้ง
				check_condition_value -> 
					stack_pool:set_stack_option(self(), check_value, get_effect(check_value) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}])
					% ไม่ต้่อง Remove Effect ที่เคยได้รับออก แต่
					% ไป check Condition ของ Effect ที่จะได้รับ ใน check_condition แทน ถ้า Condition ผิด จึงเปลี่ยน แปลง Effect ที่จะได้รับ เป็น {mismatch_condition, effect} แทน
					% ไม่ต้องระบุว่า Effect มีการเปลี่ยนแปลงหรือไม่ ใน put_ability_change(ReceivedTarget, []), แต่ถ้่าไม่การเปลี่ยนแปลงจึงไปทำใน ช่วงการ renew_effect
			end,
			
			% คือการ Check ว่า สถานะของ condition ยังคงจะเป็น active อยู่หรือไม่
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			ActiveCon = active_condition_check({CardOwner, CardOrder, CardID}, OpponentPid, AbilityID),
			%io:format("ActiveCon condition checked of {~p, ~p, ~p, ~p, ~p} is ~p~n", [CardOwner, CardOrder, CardID, AbilityNo, AbilityID, ActiveCon]),
			case ActiveCon of
				[ok] -> 
					case Check of
						check_condition_value -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Check, ReceivedTarget}];
						_ -> 
							stack_pool:set_stack_option(self(), remove_effect, get_effect(remove_effect) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]),
							[{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Check, []}]
					end;
				_ -> 
					stack_pool:set_stack_option(self(), remove_effect, get_effect(remove_effect) ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]),
					[{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, inactive, []}]
			end
	end.

compare_target(Received, ToDo) ->
	case ToDo of
		remove_effect -> Received;
		add_effect -> [];
		check_value -> []
	end.
	
compare_target(_, [], _) -> [];
compare_target(Received, [{TargetType, WillRecHead}|Tail], ToDo) ->
	case ToDo of
		remove_effect ->
			% เป่าหมายก่อนหน้าที่เคยได้รับ Effect ไปลบด้วย เป้าหมายใหม่ ที่ check ได้ การ์ดที่เหลือ คือ การ์ดที่จะต้องถูกลบ effect ออก
			case lists:keysearch(TargetType, 1, Received) of
				{value, {TargetType, HadReceive}} -> [{TargetType, HadReceive -- WillRecHead}] ++ compare_target(Received, Tail, ToDo);
				_ -> compare_target(Received, Tail, ToDo)
			end;
		add_effect ->
			% เป้าหมายใหม่ ที่ check ได้ ลบด้วย เป้าหมายก่อนหน้าที่เคยได้รับ Effect ไป การ์ดที่เหลือ คือ การ์ดที่จะได้รับ effect เพิ่ม
			case lists:keysearch(TargetType, 1, Received) of
				{value, {TargetType, HadReceive}} -> [{TargetType, WillRecHead -- HadReceive}] ++ compare_target(Received, Tail, ToDo);
				_ -> [{TargetType,WillRecHead}] ++ compare_target(Received, Tail, ToDo)
			end;
		check_value ->
			case lists:keysearch(TargetType, 1, Received) of
				{value, {TargetType, HadReceive}} -> [{TargetType, WillRecHead -- (WillRecHead -- HadReceive)}] ++ compare_target(Received, Tail, ToDo);
				_ -> [{TargetType,WillRecHead}] ++ compare_target(Received, Tail, ToDo)
			end
	end.

get_effect(Todo) ->
	case stack_pool:get_last_stack(self(), Todo) of
		{ok, Result} -> 
			%io:format("get_effect ~p:~p~n", [Todo, Result]),
			Result;
		_ -> []
	end.
	
put_ability_change(Received, WillReceive) ->
	Receiver = Received -- WillReceive,
	Remover = WillReceive -- Received,
	if
		Receiver =:= [], Remover =:= [] -> 
			case stack_pool:get_last_stack(self(), effect_change) of
				{ok, yes} -> do_nothing;
				_ -> stack_pool:set_stack_option(self(), effect_change, no)
			end;
		true -> 
			%smo_logger:msg("put_ability_change:effect change"),
			stack_pool:set_stack_option(self(), effect_change, yes)
	end.
	
check_condition({PlayerOwnID, CardOrder,  CardID}, OppPid, AbilityID) ->
	AllFx = card_utility:get_all_card_effect(PlayerOwnID, CardOrder,  CardID),
	AllInterest = [{ability, {loss, [all]}}],
	case function_utility:is_contain(AllInterest, AllFx) of
		[{ability, {loss, [all]}}] -> [];
		[] ->
			[{Owner, Player, Self, Other}] = 
				do(qlc:q( [ {
											X#continue_condition.playerown_must_check,  
											X#continue_condition.player_must_check, 
											X#continue_condition.owner_must_check, 
											X#continue_condition.other_must_check} || X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID ])),
			check_loop([{1, Owner}, {2, Player}, {3, Self}, {4, Other}], {PlayerOwnID, CardOrder,  CardID}, OppPid, AbilityID);
		_ -> []
	end.
	
check_loop([], _, _, _) -> [];
check_loop([{What, ToCheck}|Check], {PlayerOwnID, CardOrder,  CardID}, PlayerOppID, AbilityID) -> 
	case ToCheck of
		n -> [ok]++check_loop(Check, {PlayerOwnID, CardOrder,  CardID}, PlayerOppID, AbilityID);
		_ ->
			CheckedResult =
			case What of
				1 -> playerown_condition_check(PlayerOwnID, AbilityID);
				2 -> player_condition_check({PlayerOwnID, CardOrder,  CardID}, PlayerOppID, AbilityID);
				3 -> self_condition_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, AbilityID);
				4 -> list_condition_check({PlayerOwnID, CardOrder, CardID}, AbilityID, PlayerOppID)
			end,
			case CheckedResult of
				[ok] -> [ok]++check_loop(Check, {PlayerOwnID, CardOrder,  CardID}, PlayerOppID, AbilityID);
				Any -> Any %smo_logger:fmsg("Result is not ok case ~p~n", [Any]), 
			end
	end.
%===============================================================================
playerown_condition_check(PlayerOwnID, AbilityID) ->
	[PlayerOwnCondition] =  do(qlc:q([X#continue_condition.playerown_check|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	check_all_playerown_require_attribute(PlayerOwnID, PlayerOwnCondition).
%===============================================================================
check_all_playerown_require_attribute(PlayerOwnID, [ConditionHead|Tail]) ->
	case check_each_playerown_require_attribute(PlayerOwnID, ConditionHead) of
		true -> check_all_playerown_require_attribute(PlayerOwnID, Tail);
		Any -> [Any]
	end;
check_all_playerown_require_attribute(_, []) -> [ok].
%===============================================================================
check_each_playerown_require_attribute(PlayerOwnID, {Attribute, Value}) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(PlayerOwnID), Value);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(), Value);
		mp -> attribute_check:player_mp_check(game_info:player_mp(PlayerOwnID), Value);
		action -> attribute_check:player_action_check(game_info:player_action(PlayerOwnID), Value)
	end.
%===============================================================================
%===============================================================================
player_condition_check({PlayerOwnID, CardOrder,  CardID}, PlayerOppID, AbilityID) -> 
	[{PlayerCondition, RequirePlayer}] =  do(qlc:q([{X#continue_condition.player_check, 
																											X#continue_condition.player_side_check
																										  }|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	{ControlPid, UnconPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, RequirePlayer),
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
self_condition_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, AbilityID) ->
	[SelfConditionRequire] = do(qlc:q([X#continue_condition.owner_check|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	Zone = card_utility:check_card_zone(PlayerOwnID, CardOrder, CardID),
	check_all_self_require_attribute({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, SelfConditionRequire, AbilityID).
%===============================================================================
check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, [ConditionHead|Tail], AbilityID) ->
	case check_self:check_each_self_require_attribute(AbilOwnCardData, PlayerOppID, ConditionHead, AbilityID) of
		true -> check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, Tail, AbilityID);
		Any -> [Any]
	end;
check_all_self_require_attribute(_, _, [], _) -> [ok].
%===============================================================================
list_condition_check({PlayerOwnID, AbilOwnCardOrder, CardID}, AbilityID, PlayerOppID) ->
	CardZone = card_utility:check_card_zone(PlayerOwnID, AbilOwnCardOrder, CardID),
	[{RequirePlayer, ReqZone}] = do(qlc:q([{X#continue_condition.other_player_check,
																							 X#continue_condition.other_present_check
																						   }|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	PlayerZoneList1 = card_list:player_zone_list({ReqPlayer, ReqZone}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#continue_condition.other_self_include_check || X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		true -> PlayerZoneList = PlayerZoneList1
	end,
	[ConditionNeed] = do(qlc:q([X#continue_condition.other_check|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	[MatchCount] = do(qlc:q([X#continue_condition.other_match_count || X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	check_all({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, ConditionNeed, PlayerZoneList, MatchCount).
%===============================================================================
check_all(_, _, [], MatchCount) ->
	CardMatch = stack_pool:get_last_stack(self(), card_match_ability),
	stack_pool:remove_stack_option(self(), card_match_ability),
	case MatchCount of	
		{less_than, X} -> 
			case CardMatch of
				{ok, []} -> [ok];
				{ok, AllMatch} ->
					case length(AllMatch) < X of
						true -> [ok];
						false -> [card_match_more_than_require]
					end;
				_ -> [ok]
			end;				
		{equal_to , X} -> 
			case CardMatch of
				{ok, AllMatch} -> 
					case length(AllMatch) =:= X of
						true -> [ok];
						false -> [card_match_not_equal_to_require]
					end;
				_ -> 
					case X of
						0 -> [ok];
						_ -> [card_match_not_equal_to_require]
					end
			end;		
		_ -> 		 
			case CardMatch of
				{ok, _ } -> [card_require_not_enough];
				_ -> [card_require_not_enough]
			end
	end;

check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, [PlayerZoneListHead|Tail], MatchCount) ->
	CardMatch =  check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed),
	io:format("card match continuous ability {~p~p~p} are ~p~n", [AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID, CardMatch]),
	stack_pool:add_stack_option_field(self(), card_match_ability, CardMatch),
	{ok, AllCardMatch} = stack_pool:get_last_stack(self(), card_match_ability),
	case MatchCount of
		{less_than, _} ->
			check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount);
		{equal_to, _} ->
			check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount);
		_ ->
		case attribute_check:check_count(AllCardMatch, MatchCount) of
			true -> 
				stack_pool:remove_stack_option(self(), card_match_ability), 
				[ok];
			_ -> check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount)
		end
	end.
%===============================================================================
active_condition_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, AbilityID) ->
	[ConditionRequire] = do(qlc:q([X#continue_condition.active_condition|| X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	Zone = card_utility:check_card_zone(PlayerOwnID, CardOrder, CardID),
	check_all_self_require_attribute({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, ConditionRequire, AbilityID).
%===============================================================================
check_seal_terget({CardOwner, CardOrder, CardID}, OppPid, AbilityID) ->
	OwnerTargetChecked = s_ability_target:playerown_target_check(AbilityID),
	Owner =
	if
		OwnerTargetChecked =:= [] -> [];
		true -> [{1, [CardOwner]}]
	end,
	PlayerTarget = s_ability_target:playeropp_target_check({CardOwner, CardOrder, CardID}, OppPid, AbilityID),
	AnyPlayer =
	if
		PlayerTarget =:= [] -> [];
		true -> [{2, [PlayerTarget]}]
	end,
	SelfChecked = s_ability_target:owner_target_check(AbilityID), % if in the present condition this card can use ability then check which card will take this effects
	SelfCard =
	if
		SelfChecked =:= [] -> [];
		true -> [{3, [{CardOwner, CardOrder, CardID}]}]
	end,
	OtherChecked = s_ability_target:other_targets_check({CardOwner, CardOrder, CardID}, AbilityID, OppPid),
	OtherCard =
	if
		OtherChecked =:= [] -> [];
		true -> [{4, OtherChecked}]
	end,
	Owner ++ AnyPlayer ++ SelfCard ++OtherCard.
%------------------------------------------------------------------
check_mystic_target({CardZone, {CardOwner, CardOrder, CardID}}, OpponentPid, MAbilityID, ReceivedTarget) ->
	FxOwner = do(qlc:q( [X#mystic_ability.have_fx_to_owner|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Owner =
	case FxOwner of
		[n]  -> [];
		_ ->[{1, [CardOwner]}]
	end,
	FxPlayer = do(qlc:q( [X#mystic_ability.have_fx_to_any_player|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Player =
	case FxPlayer of
		[n] -> [];
		_ -> 
			PlayerTarget = new_mystic_check:player_got_effect({CardOwner, CardOrder, CardID}, OpponentPid, MAbilityID),
			[{2, PlayerTarget}]
	end,
	FxThis = do(qlc:q( [X#mystic_ability.have_fx_to_this|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	This =
	case FxThis of
		[n] -> [];
		_ -> 
			case mystic_effect:check_target_cancel_mystic(CardOwner, CardOrder, CardID, [{CardOwner, CardOrder, CardID}]) of
				[] -> [];
				_ -> [{3, [{CardOwner, CardOrder, CardID}]}]
			end
	end,
	FxTarget = do(qlc:q( [X#mystic_ability.have_fx_to_target|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Target =
	case FxTarget of
		[n] -> [];
		_ ->
			TargetFxSelect =	do(qlc:q( [X#mystic_ability.target_fx_select|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
			case TargetFxSelect of
				{do_not_need_select, 0} -> 
					OCardTarget = new_mystic_check:target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid),
					case mystic_effect:check_target_cancel_mystic(CardOwner, CardOrder, CardID, OCardTarget) of
						[] -> [];
						_ ->[{4, OCardTarget}]
					end;
				_ -> ReceivedTarget
			end
	end,
	FxBeyond = do(qlc:q( [X#mystic_ability.have_fx_to_beyond_target|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	Beyond =
	case FxBeyond of
		[n] -> [];
		_ -> 
			BeyondTarget = new_mystic_check:beyond_target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid),
			case mystic_effect:check_target_cancel_mystic(CardOwner, CardOrder, CardID, BeyondTarget) of
				[] -> [];
				_ -> [{5, BeyondTarget}]
			end
	end,
	Owner ++ Player ++ This ++ Target ++ Beyond.
%------------------------------------------------------------------
activate_continuous_ability() ->
	% Remove = get(remove_effect),
	% PlayerPid =
	% case stack_pool:get_last_stack(self(), card_player) of
		% {ok, CardPlayer} -> CardPlayer;
		% _ ->
			% case stack_pool:get_last_stack(self()) of
				% {ok, {CardPlayer, _, _, _}} -> CardPlayer;
				% _ -> 
					% {ok, PlayerTurn} = mnesia_play:get_game_data(self(), player_turn),
					% PlayerTurn
			% end
	% end,
	% เริ่มต้นด้วยการ check_value ก่อนเสมอ เพราะ Renew เป็นการทำ แบบ Loop แล้วมักไม่กระทบต่อการ เปลี่ยน สถานะจาก Active เป็น Inactive
	{ok, ChangeValue} = stack_pool:get_last_stack(self(), check_value),
			%io:format("All Renew Ability ~p~n", [ChangeValue]),
	renew_continuous_ability(ChangeValue),
	{ok, Remove} = stack_pool:get_last_stack(self(), remove_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	InactiveFx =
	case stack_pool:get_last_stack(self(), effect_inactive) of
		{ok, InacFx} -> InacFx;
		_ -> []
	end,
	% push_stack เพื่อ ทำ remove_effect
	stack_pool:push_stack(PlayerPid, 0, 0, [{remain_remove_effect, Remove}, {effect_inactive, InactiveFx}]),
	%stack_pool:set_stack_option(self(), remain_remove_effect, Remove),
	%remove_continuous_ability(Remove),
	remove_continuous_ability().
	% Add = get(add_effect),
	% stack_pool:set_stack_option(self(), remain_add_effect, Add),
	% add_continuous_ability().

remove_continuous_ability() ->
	RemainFx = stack_pool:get_last_stack(self(), remain_remove_effect),
	case RemainFx of
		% ไม่มี Effect ที่จะ Remove แล้ว
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			{ok, Add} = stack_pool:get_last_stack(self(), add_effect),
			% pop_stack ของการ remove_effect ออก
			%renew_continuous_ability(ChangeValue),
			stack_pool:set_stack_option(self(), remain_add_effect, Add),
			% ทำ add_effect ต่อไป
			add_continuous_ability();
		% ไม่มี Target ที่จะรับ Effect
		{ok, [{{_CardOwner, _CardOrder, _CardID, _AbilityNo, _AbilityID}, []}|Tail]} -> 
			stack_pool:set_stack_option(self(), remain_remove_effect, Tail), 
			remove_continuous_ability();
		% มี Target ที่จะรับ Effect
		{ok, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}|Tail]} ->
			stack_pool:set_stack_option(self(), remain_remove_effect, Tail),
			stack_pool:set_stack_option(self(), this_remove_effect, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]),
			remove_effect_from_target()
	end.
	
remove_effect_from_target() ->
	ThisRmFx = stack_pool:get_last_stack(self(), this_remove_effect),
	case ThisRmFx of
		{ok, [{_, []}]} -> remove_continuous_ability();
		{ok, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]} ->
			[{TargetType, TargetList}|Target] = ReceivedTarget,
			stack_pool:set_stack_option(self(), this_remove_effect_this_target, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, {TargetType, TargetList}}]),
			stack_pool:set_stack_option(self(), this_remove_effect, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Target}]),
			stack_pool:set_stack_option(self(), play, remove_continuous_effect_from_target),
			if 
				TargetType =:= 1; TargetType =:= 2 -> remove_effect_from_player({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetList);%add_effect_to_player();
				TargetType =:= 3; TargetType =:= 4; TargetType =:= 5-> remove_effect_from_card({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetList)
			end
	end.
% remove_continuous_ability(RemoveAbility) ->
	% lists:foreach(
								% fun({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}) ->
									% remove_effect_from_target({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget)
								% end, RemoveAbility).
	
% remove_effect_from_target({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget) ->
	% lists:foreach(	
								% fun({TargetType, TargetList}) ->
									% if
										% TargetType =:= 3; TargetType =:= 4; TargetType =:= 5 -> remove_effect_from_card({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetList);
										% TargetType =:= 1; TargetType =:= 2 -> remove_effect_from_player({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetList)
									% end
								% end, ReceivedTarget).	
remove_effect_from_card(_, []) -> card_inactive_destroy();
remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, [{TCardOwner, TCardOrder, TCardID}|TargetList]) ->
	% เนื่องจาก ตอนที่ Add Effect ทำโดย ถ้า Effect = [{at, 5}, {df, 6}]
	% จะแยกเป็นสอง Tuple คือ [{Give, [{at, 5}], Duration}, {Give, [{Df, 6}], Duration}]
	% ดังนั้นตอนลบจึง ต้่้องลบทีละ Tuple
	remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, {TCardOwner, TCardOrder, TCardID}, TargetList).
								
remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, {TCardOwner, TCardOrder, TCardID}, TargetList) ->
	{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
	%FxDelete =
	case function_utility:check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
	  % ถ้าพบว่าการ์ด มี Effect นี้ ให้ ลบออกแล้ว วน Check เรื่อยจนกว่าจะไม่เจอ Effect ของ {CardOwner, CardOrder, CardID, _AbilityNo, AbilityID} แลัว
	  % ถ้่าไม่พบแล้ว ไปทำ remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, TargetList) ใน Case อื่น
		{value, {GiveFx, Fx, Duration}} ->
			% check ว่า มี Effect ใดที่เป็น inactive และ Duration เป็น depend_on_s หรือไม่
			% ถ้าใช่หมายความว่า Effect นั้นได้รับจาก Mystic และ การ์ดใบนั้นต้องตก Shrine ไป
			case Duration of
				% Effect ที่ีมี Duration เป็น depend_on_s
				depend_on_s ->
					% Check ต่อว่า เป็น Ability ที่ เก็บไว้ใน stack_pool:get_last_stack(self(), effect_inactive) หรือไม่ ถ้าใช่
					EffectInactive = stack_pool:get_last_stack(self(), effect_inactive),
					% EffectInactive คือ Effect ที่เคย Active แต่ กลายเป็น Inactive 
					% ถ้า Duration เป็น depend_on_s แล้ว GiveFx จะเป็น Mystic และ GiveFx ต้องตก Shrine ไป
					case check_mystic_effect_inactive(EffectInactive, {CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}) of
						y ->
							{GOwner, GOrder, GID, _} = GiveFx,
							stack_pool:add_stack_option_field(self(), depend_on_s_destroy, [{GOwner, GOrder, GID}]);
						_ ->
					% ถ้่าไม่ใช่ depend_on_s ให้ remove_effect ออกเท่านั้น
						RemainFx = check_remain([{GiveFx, Fx, Duration}], TFX),
						
						%smo_logger:fmsg("Effect which card remain are ~p~n", [RemainFx]),
						%effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update),
						effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [{GiveFx, Fx}], remove),
						card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, RemainFx),
						effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update),
						remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, {TCardOwner, TCardOrder, TCardID}, TargetList)
					end;
				_ -> 
					RemainFx = check_remain([{GiveFx, Fx, Duration}], TFX),
					%card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, RemainFx),
					%smo_logger:fmsg("Effect which card remain are ~p~n", [RemainFx]),
					effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [{GiveFx, Fx}], remove),
					card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, RemainFx),
					effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update),
					remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, {TCardOwner, TCardOrder, TCardID}, TargetList)
			end;
			%effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [{GiveFx, Fx}], remove),
		_ -> remove_effect_from_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, TargetList)
	end.
								
card_inactive_destroy() ->
	case stack_pool:get_last_stack(self(), depend_on_s_destroy) of
		{ok, DestroyList} -> 
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			destroy:check_card_destroyed(PlayerPid, DestroyList, depend_on_s_destroy);
		_ -> interfere_step:return_play(check_play_step)
	end.
	
check_mystic_effect_inactive(EffectInactive, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}) ->
	case EffectInactive of
		{ok, EffectInac} -> 
			case lists:keysearch({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, 1, EffectInac) of
				{value, _} -> y;
				_ -> n
			end;
		_ -> n
	end.
	
remove_effect_from_player({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, TargetList) ->
	lists:foreach(	
								fun(PlayerPid) ->
									%{ok, TFX} = get_player_effect(PlayerPid, player_effect),
									TFX = get_player_effect(PlayerPid, player_effect),
									%smo_logger:fmsg("Effect which card received are ~p~n", [TFX]),
									FxDelete =
									case function_utility:check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
										{value, {GiveFx, Fx, Duration}} ->	[{GiveFx, Fx, Duration}];
										_ -> []
									end,
									%smo_logger:fmsg("Effect which will remove from received effect is ~p~n", [FxDelete]),
									RemainFx = check_remain(FxDelete, TFX),
									update_player_effect(PlayerPid, RemainFx)
									%smo_logger:fmsg("Effect which card remain are ~p~n", [RemainFx])
								end, TargetList),
	interfere_step:return_play(check_play_step).

check_remain([], ReceivedFx) -> ReceivedFx;
check_remain(_, []) -> [];
check_remain([{GiveFx, Fx, Duration}], [{GiveFx, _Fx, _Duratoin}|Tail]) -> check_remain([{GiveFx, Fx, Duration}], Tail);
check_remain([{GiveFx, Fx, Duration}], [{_GiveFx, _Fx, _Duratoin}|Tail]) -> [{_GiveFx, _Fx, _Duratoin}] ++ check_remain([{GiveFx, Fx, Duration}], Tail).
	
% เรียกจาก ....activate_continuous_ability()
add_continuous_ability() ->
	RemainFx = stack_pool:get_last_stack(self(), remain_add_effect),
	case RemainFx of
		{ok, []} -> 
			FxChange = stack_pool:get_last_stack(self(), effect_change),
			case FxChange of
				{ok, yes} -> 
					check_continuous_target();
				{ok, _} -> 
					stack_pool:set_stack_option(self(), add_effect, []),
					stack_pool:set_stack_option(self(), remove_effect, []),
					stack_pool:set_stack_option(self(), check_value, []),
					interfere_step:return_play(check_play_step)
			end;
		{ok, [{{_CardOwner, _CardOrder, _CardID, _AbilityNo, _AbilityID}, []}|Tail]} -> 
			stack_pool:set_stack_option(self(), remain_add_effect, Tail), 
			add_continuous_ability();
		{ok, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}|Tail]} ->
			stack_pool:set_stack_option(self(), remain_add_effect, Tail),
			stack_pool:set_stack_option(self(), this_add_effect, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]),
			add_effect_to_target()
	end.
	
add_effect_to_target() ->
	ThisAddFx = stack_pool:get_last_stack(self(), this_add_effect),
	case ThisAddFx of
		{ok, [{_, []}]} -> add_continuous_ability();
		{ok, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}]} ->
			[{TargetType, TargetList}|Target] = ReceivedTarget,
			stack_pool:set_stack_option(self(), this_add_effect_this_target, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, {TargetType, TargetList}}]),
			stack_pool:set_stack_option(self(), this_add_effect, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Target}]),
			if 
				TargetType =:= 1; TargetType =:= 2 -> add_effect_to_card();%add_effect_to_player();
				TargetType =:= 3; TargetType =:= 4; TargetType =:= 5-> add_effect_to_card()
			end
	end.

%add_effect_to_player() -> [].
add_effect_to_player() -> interfere_step:return_play(check_play_step).
	
add_effect_to_card() ->
	{ok, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, {TargetType, TargetList}}]} = stack_pool:get_last_stack(self(), this_add_effect_this_target),
	case TargetList of
		[] -> add_effect_to_target();
		_ ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal ->
					case TargetType of
						1 -> 
							[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.playerown_effect, X#card_ability.owner_duration_effect}  || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
						2 -> 
							[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.player_effect, X#card_ability.owner_duration_effect}  || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
						3 -> 
							[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.owner_effect, X#card_ability.owner_duration_effect}  || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
						4 -> 
							[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.other_effect, X#card_ability.other_duration_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]))
					end;
				is_not_seal ->
					case TargetType of
						1 ->
							[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_to_owner, X#mystic_ability.this_fx_duration}  || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]));
						2 ->
							[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_to_player, X#mystic_ability.this_fx_duration}  || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]));
						3 -> 
							[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_to_this, X#mystic_ability.this_fx_duration}  || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]));
						4 -> 
							[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_target_receive, X#mystic_ability.target_fx_duration} || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]));
						5 -> 
							[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_beyond_target_receive, X#mystic_ability.beyond_target_fx_duration} || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]))
					end
			end,
			StackOption = [{card_give_effect, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}}, {effect_list, NFx}, {duration, Duration}, {target, {TargetType, TargetList}}],
			stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, StackOption),
			%interfere_step:return_play(check_play_step).
			check_effect_to_card()
	end.

check_effect_to_card() ->
	FxList = stack_pool:get_last_stack(self(), effect_list),
	case FxList of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			add_effect_to_target();
			%interfere_step:return_play(check_play_step);
		{ok, [Fx | FxRemain]} ->
			stack_pool:set_stack_option(self(), play, affect_card_ability_effect),
			stack_pool:set_stack_option(self(), effect_list, FxRemain),
			stack_pool:set_stack_option(self(), current_effect, Fx),
			interfere_step:return_play(check_play_step)
	end.

add_current_effect() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), current_effect),
	stack_pool:set_stack_option(self(), play, return_to_check_effect_to_card),
	check_effect_affect(Fx).
	
set_msytic_give_effect(CardOwner, CardOrder, CardID, [{AbilityID, MysticTarget, Duration}]) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> ok;
		_ -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{AbilityID, MysticTarget, Duration}])
	end.
	
check_effect_affect({EffectType, EffectToDo}) ->
	case EffectType of
		% Effect to Player
		%{check, {player_target_effect, s1_wind_no75_a1a, {less_than, 1}, {card_on_hand, -1}}}
		check ->
			case EffectToDo of
				%{player_target_effect, s1_wind_no75_a1a, {less_than, 1}, {card_on_hand, -1}}
				{player_target_effect, AbilIDCheck, CheckMatch, CheckedThenDo} ->
					{ok, {_TType, [Target]}} = stack_pool:get_last_stack(self(), target),
					case check_player_got_effect(Target, player_effect, AbilIDCheck, CheckMatch) of
						true -> check_effect_affect(CheckedThenDo);
						_ -> interfere_step:return_play(check_play_step)
					end;
				{card_target_effect, AbilIDCheck, CheckMatch, CheckedThenDo} ->
					{ok, {TType, Target}} = stack_pool:get_last_stack(self(), target),
					case check_card_got_effect(Target, AbilIDCheck, CheckMatch) of
						[] -> interfere_step:return_play(check_play_step);
						MatchTarget -> 
							stack_pool:set_stack_option(self(), target, {TType, MatchTarget}),
							check_effect_affect(CheckedThenDo)
					end;
				_ -> interfere_step:return_play(check_play_step)
			end;
		player_assign_atk ->
			{ok, {_TType, [Target]}} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{AbilityID, [Target], Duration}]),
			{ok, PlayerFx} = mnesia_play:get_player_data(Target, player_effect),
			mnesia_play:set_player_data(Target, player_effect, PlayerFx ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{player_assign_atk, EffectToDo}], Duration}]),
			interfere_step:return_play(check_play_step);
		card_on_hand ->
			{ok, {_TType, [Target]}} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{AbilityID, [Target], Duration}]),
			{ok, PlayerFx} = mnesia_play:get_player_data(Target, player_effect),
			mnesia_play:set_player_data(Target, player_effect, PlayerFx ++ [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{card_hand_max, EffectToDo}], Duration}]),
			PlayerHandFx = continuous_ability:get_player_effect(Target, card_hand_max),
			HandSize = 7,
			OpposePid = mnesia_play:get_opponent_pid(Target),
			gen_server:cast(OpposePid, {send, [16#88, 16#76, 1, 0, HandSize + PlayerHandFx]}),
			interfere_step:return_play(check_play_step);
		% Effect to Card
		cancel_mystic ->
			{ok, {_TType, Target}} = stack_pool:get_last_stack(self(), target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_card_effect_and_active(Target, [{CardGive, [{EffectType, EffectToDo}], Duration}]),
			%remove_mystic_remain_effect(Target, EffectToDo),
			DestroyMystic =
			case EffectToDo of
				[all] -> 
					mystic_card:destroy(Target, all_mystic);
				[all_opponent] -> mystic_card:destroy(Target, all_opponent);
				[opponent_relic] -> mystic_card:destroy(Target, opponent_relic);
				[all_tarot] -> mystic_card:destroy(Target, all_tarot)
			end,
			remove_mystic_remain_effect(Target, DestroyMystic, EffectToDo),
			case DestroyMystic of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, PlayerPid} = mnesia_play:get_game_data (self(), player_turn),
					shrine_zone:card_to_shrine(PlayerPid, DestroyMystic)
			end;
		cancel_skill ->
			{ok, {_TType, Target}} = stack_pool:get_last_stack(self(), target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_card_effect_and_active(Target, [{CardGive, [{EffectType, EffectToDo}], Duration}]),
			remove_skill_remain_effect(Target, EffectToDo),
			interfere_step:return_play(check_play_step);
		cancel_ability ->
			{ok, {_TType, Target}} = stack_pool:get_last_stack(self(), target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_card_effect_and_active(Target, [{CardGive, [{EffectType, EffectToDo}], Duration}]),
			remove_ability_remain_effect(Target, EffectToDo),
			interfere_step:return_play(check_play_step);
		check_condition ->
			{ok, Target} = stack_pool:get_last_stack(self(), target),
			case Target of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					case EffectToDo of
						{Condition, SubFx} ->
							CardMatch = function_utility:card_match_condition(Target, Condition),
							case CardMatch of
								[] -> check_effect_affect({mismatch_condtion, effect});%interfere_step:return_play(check_play_step);
								_ -> check_effect_affect(SubFx)
							end
					end
			end;
		elem ->
			io:format("continuous elemt change ~n"),
			{ok, Target} = stack_pool:get_last_stack(self(), target),
			lists:foreach(fun({TPid, TOrder, TID}) ->
				seal_card:change_card_element(TPid, TOrder, TID, EffectToDo) end, Target),
				%effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], update) end, Target),
			{ok, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			activate_effect_to_target(Target, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{EffectType, EffectToDo}], Duration}]);
		action ->
			case EffectToDo of
				sacrifice ->
					{ok, {CardOwner, _CardOrder, _CardID, _AbilityNo, _AbilityID}} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, {_TType, Target}} = stack_pool:get_last_stack(self(), target),
					destroy:check_card_destroyed(CardOwner, Target, sacrifice)
			end;
		Other ->
			{ok, {CardOwner, CardOrder, CardID, AbilityNo, AbilityID}} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, {_TType, Target}} = stack_pool:get_last_stack(self(), target),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			if 
				Other =:= at; Other =:= df; Other =:= sp; Other =:= ma; Other =:= ms; Other =:= mc ->
					activate_power_effect_to_target(Target, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{EffectType, EffectToDo}], Duration}]);
				true -> activate_effect_to_target(Target, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{EffectType, EffectToDo}], Duration}])
			end
	end.

remove_mystic_remain_effect(Target, DestroyMystic, WhichRemove) ->
	lists:foreach(
									fun({TCardOwner, TCardOrder, TCardID}) ->
										{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
										FxRemain = effect_remain(TFX, {TCardOwner, TCardOrder, TCardID}, {mystic, DestroyMystic}, WhichRemove),
										RemoveFx = TFX -- FxRemain,
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, RemoveFx, remove),
										card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, FxRemain),
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update)
									end, Target).

remove_ability_remain_effect(Target, WhichRemove) ->
	lists:foreach(
									fun({TCardOwner, TCardOrder, TCardID}) ->
										{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
										FxRemain = effect_remain(TFX, {TCardOwner, TCardOrder, TCardID}, ability, WhichRemove),
										RemoveFx = TFX -- FxRemain,
										%io:format("REMOVE Effect ~p~n", [RemoveFx]),
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, RemoveFx, remove),
										card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, FxRemain),
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update)
									end, Target).

remove_skill_remain_effect(Target, WhichRemove) ->
	lists:foreach(
									fun({TCardOwner, TCardOrder, TCardID}) ->
										{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
										FxRemain = effect_remain(TFX, {TCardOwner, TCardOrder, TCardID}, skill, WhichRemove),
										RemoveFx = TFX -- FxRemain,
										%io:format("REMOVE Effect ~p~n", [RemoveFx]),
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, RemoveFx, remove),
										card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, FxRemain),
										effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update)
									end, Target).
									
effect_remain([], _, _, _) -> [];
effect_remain([Fx|TFX], {TCardOwner, TCardOrder, TCardID}, RemoveType, WhichRemove) ->
	check_effect(Fx, {TCardOwner, TCardOrder, TCardID}, RemoveType, WhichRemove) ++ effect_remain(TFX, {TCardOwner, TCardOrder, TCardID}, RemoveType, WhichRemove).

% check_effect({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}, {TCardOwner, TCardOrder, TCardID}, RemoveType, WhichRemove) ->
	% case RemoveType  of
		% mystic ->
			% case mnesia_odbc:is_seal_card(CardID) of
				% is_not_seal -> 
					% case check_to_remove(CardID) of
						% remove -> [];
						% _ -> 
							% case WhichRemove of
								% [all_opponent] ->
									% case TCardOwner of
										% CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}];
										% _ -> []
									% end;
								% _ -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
							% end
					% end;
				% _ -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
			% end;
		% skill ->
			% case mnesia_odbc:is_skill(AbilityID) of
				% true ->
					% case WhichRemove of
						% [all_opponent] ->
							% case TCardOwner of
								% CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}];
								% _ -> []
							% end;
						% _ -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
					% end;
				% false -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
			% end;
		% ability ->
			% RemainFx = 
			% case mnesia_odbc:is_skill(AbilityID) of
				% false ->
					% IsAbility = true,
					% case WhichRemove of
						% [all_opponent] ->
							% case TCardOwner of
								% CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}];
								% _ -> []
							% end;
						% _ -> [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
					% end;
				% true -> 
					% IsAbility = false,
					% [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Fx, Duration}]
			% end,
			% case IsAbility of
				% true -> % Check ด้วยว่า Ability นั้น เป็น Continuous หรือไม่ ถ้่าใช้่ต้องไป ลบ Target ออก จาก AbilityID นั้นด้วย
					% io:format("remove target {~p, ~p, ~p} from {~p, ~p, ~p, ~p, ~p}~n", [TCardOwner, TCardOrder, TCardID, CardOwner, CardOrder, CardID, AbilityNo, AbilityID]);
				% _ -> do_nothing
			% end,
			% RemainFx
	% end;
check_effect({{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}, {TCardOwner, TCardOrder, TCardID}, RemoveType, WhichRemove) ->
	% กรณี ที่จะ Remove Effect ของ Mystic
	case RemoveType of
		{mystic, DestroyMystic} ->
			% Check แล้วว่า GiveFx เป็น Mystic
			case mnesia_odbc:is_seal_card(CardID) of
				is_not_seal ->
					% % Check ว่า GiveFx เป็น PS 0 Turn หรือ PA
					% case check_to_remove(CardID) of
						% % ถ้าเป็น เป็น PS 0 Turn หรือ PA
						% remove -> [];
						% _ -> 
							case WhichRemove of
								[all_opponent] ->
									case TCardOwner of
										CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
										_ -> 
											case [{CardOwner, CardOrder, CardID}] -- DestroyMystic of
												[] -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
												_ -> []
											end
									end;
								_ -> 
									case [{CardOwner, CardOrder, CardID}] -- DestroyMystic of
										[] -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
										_ -> []
									end
									%[{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
							end;
					%end;
				_ -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
			end;
		skill ->
			case mnesia_odbc:is_skill(AbilityID) of
				true ->
					case WhichRemove of
						[all_opponent] ->
							case TCardOwner of
								CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
								_ -> []
							end;
						[{elem, Elem}] ->
							case function_utility:card_match_condition([{CardOwner, CardOrder, CardID}], [{elem, Elem}]) of
								[] -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
								_ -> []
							end;
						_ -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
					end;
				false -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
			end;
		ability ->
			RemainFx = 
			case mnesia_odbc:is_skill(AbilityID) of
				false ->
					IsAbility = true,
					case WhichRemove of
						[all_opponent] ->
							case TCardOwner of
								CardOwner -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
								_ -> []
							end;
						[{elem, Elem}] ->
							case function_utility:card_match_condition([{CardOwner, CardOrder, CardID}], [{elem, Elem}]) of
								[] -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}];
								_ -> []
							end;
						_ -> [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
					end;
				true -> 
					IsAbility = false,
					[{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}]
			end,
			case IsAbility of
				true -> % Check ด้วยว่า Ability นั้น เป็น Continuous หรือไม่ ถ้่าใช้่ต้องไป ลบ Target ออก จาก AbilityID นั้นด้วย
					%io:format("remove target {~p, ~p, ~p} from ~n", [TCardOwner, TCardOrder, TCardID]),
					case RemainFx of
						[] -> 	exclude_target_from_active({CardOwner, CardOrder, CardID, AbilityID}, TCardOwner, TCardOrder, TCardID);
						_ -> do_nothing
					end;
				_ -> do_nothing
			end,
			RemainFx
	end.
% check_effect({{CardOwner, CardOrder, CardID}, Fx, Duration}, TCardOwner, WhichRemove) ->
	% case mnesia_odbc:is_seal_card(CardID) of
		% is_not_seal -> 
			% case check_to_remove(CardID) of
				% remove -> [];
				% _ -> 
					% case WhichRemove of
						% [all_opponent] ->
							% case TCardOwner of
								% CardOwner -> [{{CardOwner, CardOrder, CardID}, Fx, Duration}];
								% _ -> []
							% end;
						% _ -> [{{CardOwner, CardOrder, CardID}, Fx, Duration}]
					% end
			% end;
		% _ -> [{{CardOwner, CardOrder, CardID}, Fx, Duration}]
	% end.
	
check_to_remove(CardID) ->
	[{UsingType, Duration}] = do(qlc:q([{X#mystic_card.paste_type, X#mystic_card.duration} || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID])),
	case {UsingType, Duration} of
		{2, _} -> remove;
		{1, 0} -> remove;
		_ -> remain
	end.
	
add_card_effect_and_active(Target, [{CardGive, NFx, Duration}]) ->
	lists:foreach(fun({TCardOwner, TCardOrder, TCardID}) ->
									{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
									{CardOwner, CardOrder, CardID, _AbilityNo, AbilityID} = CardGive,
									{RemoveFX, ReplaceFX} =
									case function_utility:check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
										{value, {GiveFx, Fx, _Duration}} -> {[{GiveFx, Fx, _Duration}], [{GiveFx, Fx ++ NFx, Duration}]};
										_ -> {[], [{{CardOwner, CardOrder, CardID, AbilityID}, NFx, Duration}]}
									end,
									ResultFx = (TFX -- RemoveFX) ++ ReplaceFX, 
									card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, ResultFx)
								end, Target).
	
activate_effect_to_target(Target, [{CardGive, NFx, Duration}]) ->
	lists:foreach(fun({TCardOwner, TCardOrder, TCardID}) ->
		{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
		{CardOwner, CardOrder, CardID, _AbilityNo, AbilityID} = CardGive,
		{RemoveFX, ReplaceFX} =
		case function_utility:check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
			{value, {GiveFx, Fx, _Duration}} -> {[{GiveFx, Fx, _Duration}], [{GiveFx, Fx ++ NFx, Duration}]};
			_ -> {[], [{{CardOwner, CardOrder, CardID, AbilityID}, NFx, Duration}]}
		end,
		ResultFx = (TFX -- RemoveFX) ++ ReplaceFX, 
		card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, ResultFx),
		effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update) end, Target),
		interfere_step:return_play(check_play_step).
	
activate_power_effect_to_target(Target, [{CardGive, [{EffectType, EffectToDo}], Duration}]) ->
	lists:foreach(fun({TCardOwner, TCardOrder, TCardID}) ->
		{CardOwner, CardOrder, CardID, _AbilityNo, AbilityID} = CardGive,
		NFx = effect_value:check_value(CardOwner, CardOrder, CardID, {EffectType, EffectToDo}, {TCardOwner, TCardOrder, TCardID}),
		{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
		{RemoveFX, ReplaceFX} =
		case function_utility:check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
			{value, {GiveFx, Fx, _Duration}} -> {[{GiveFx, Fx, _Duration}], [{GiveFx, Fx ++ NFx, Duration}]};
			_ -> {[], [{{CardOwner, CardOrder, CardID, AbilityID}, NFx, Duration}]}
		end,
		ResultFx = (TFX -- RemoveFX) ++ ReplaceFX, 
		card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, ResultFx),
		effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update) end, Target),
		interfere_step:return_play(check_play_step).
		
%renew_continuous_ability(undefined) -> interfere_step:return_play();
renew_continuous_ability(RenewAbility) ->
	lists:foreach(
								fun({{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget}) ->
									renew_effect_to_target({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget)
								end, RenewAbility).

renew_effect_to_target({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ReceivedTarget) ->
	lists:foreach(	
								fun({TargetType, TargetList}) ->
									if
										TargetType =:= 3; TargetType =:= 4; TargetType =:= 5 -> renew_effect_to_card({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetType,TargetList);
										TargetType =:= 1; TargetType =:= 2 -> renew_effect_to_player({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, TargetType, TargetList)
									end
								end, ReceivedTarget).

renew_effect_to_card({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, TargetType, TargetList) ->
	%io:format("renew effect of {~p, ~p, ~p, ~p, ~p} to Target ~p ~n", [CardOwner, CardOrder, CardID, _AbilityNo, AbilityID, TargetList]),
	lists:foreach(
								fun({TCardOwner, TCardOrder, TCardID}) ->
									%CheckedValue = [{at, 2}, {df, 4}], โดยรูปแบบ
									CheckedValue =
									case mnesia_odbc:is_seal_card(CardID) of
										is_seal ->
											case TargetType of
												3 -> 
													[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.owner_effect, owner_duration_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID])),
													check_effect_value({TCardOwner, TCardOrder, TCardID}, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList);
												4 -> 
													[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.other_effect, other_duration_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID])),
													check_effect_value({TCardOwner, TCardOrder, TCardID}, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList)
											end;
										is_not_seal ->
											case TargetType of
												3 -> 
													[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_to_this, X#mystic_ability.this_fx_duration}  || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID])),
													check_effect_value({TCardOwner, TCardOrder, TCardID}, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList);
												4 -> 
													[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_target_receive, X#mystic_ability.target_fx_duration} || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID])),
													check_effect_value({TCardOwner, TCardOrder, TCardID}, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList);
												5 -> 
													[{NFx, Duration}] =	do(qlc:q( [{X#mystic_ability.fx_beyond_target_receive, X#mystic_ability.beyond_target_fx_duration} || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID])),
													check_effect_value({TCardOwner, TCardOrder, TCardID}, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList)
											end
									end,
									%io:format("checked value ~p~n", [CheckedValue]),
									{ok, TFX} = card_utility:get_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect),
									% ContainFx = [{{CardOwner, CardOrder, CardID, AbilityID}, Fx1, Duration}}, {{CardOwner, CardOrder, CardID, AbilityID}, Fx2, Duration}}]
									ContainFx = function_utility:list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX),
									
									%io:format("ContainFx ~p~n", [ContainFx]),
									% ReFx = [{{CardOwner, CardOrder, CardID, AbilityID}, Fx1, Duration}}, {{CardOwner, CardOrder, CardID, AbilityID}, Fx2, Duration}}]
									ReFx = function_utility:replace_effect({CardOwner, CardOrder, CardID, AbilityID}, CheckedValue, Duration),
									
									%io:format("Replace Effect ~p~n", [ReFx]),
									{RemoveFX, ReplaceFX} =
									case ContainFx -- ReFx of
										[] -> 
											case ContainFx of
												[] -> {[], ReFx};
												_ -> {[], []}
											end;
										_ -> {ContainFx, ReFx}
									end,
										% {value, {GiveFx, Fx, _Duration}} -> 
											% % ถ้าค่าเดิมเท่ากับค่าที่เช็ค 
											% smo_logger:fmsg("check_value ~p and FX ~p ~n", [CheckedValue, Fx]),
											% case CheckedValue =:= Fx of
												% % ถ้าค่าเดิมเท่ากับค่าที่เช็ค
												% true -> {[], []};
												% % ถ้าค่าเดิมไม่เท่ากับค่าที่เช็ค
												% _ -> {[{GiveFx, Fx, _Duration}], [{GiveFx, CheckedValue, Duration}]}
											% end;
									ResultFx = (TFX -- RemoveFX) ++ ReplaceFX,
									case {RemoveFX, ReplaceFX} of
										{[], []} -> 
											case stack_pool:get_last_stack(self(), effect_change) of
												{ok, _} -> do_nothing;
												_ -> stack_pool:set_stack_option(self(), effect_change, no)
											end;
										_ -> 
											%smo_logger:msg("renew_effect_to_card:effect_change"),
											stack_pool:set_stack_option(self(), effect_change, yes)
									end,											
									 card_utility:update_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, ResultFx),
									 effect_activate:send_update_activate_effect(TCardOwner, TCardOrder, TCardID, [], update)
								end, TargetList).

renew_effect_to_player({CardOwner, CardOrder, CardID, _AbilityNo, AbilityID}, TargetType, TargetList) -> 
	lists:foreach(
								fun({PlayerPid}) ->
									CheckedValue =
									case TargetType of
										1 -> 
											[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.owner_effect, owner_duration_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID])),
											check_effect_value(PlayerPid, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList);
										2 -> 
											[{{_, _, NFx}, Duration}] =	do(qlc:q( [{X#card_ability.other_effect, other_duration_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID])),
											check_effect_value(PlayerPid, CardOwner, CardOrder, CardID, AbilityID, NFx, TargetList)
									end,
									{ok, TFX} = get_player_effect(PlayerPid, player_effect),
									Contain = function_utility:list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX),
									ReFx = function_utility:replace_effect({CardOwner, CardOrder, CardID, AbilityID}, CheckedValue, Duration),
									{RemoveFX, ReplaceFX} =
									case Contain -- ReFx of
										[] -> {[], []};
										_ -> {Contain, ReFx}
									end,
									%case function_utility:list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, TFX) of
										% {value, {GiveFx, Fx, _Duration}} -> 
											% % ถ้าค่าเดิมเท่ากับค่าที่เช็ค 
											% case CheckedValue =:= Fx of
												% % ถ้าค่าเดิมเท่ากับค่าที่เช็ค
												% true -> {[], []};
												% % ถ้าค่าเดิมไม่เท่ากับค่าที่เช็ค
												% _ -> {[{GiveFx, Fx, _Duration}], [{GiveFx, CheckedValue, Duration}]}
											% end;
										% _ -> {[], [{{CardOwner, CardOrder, CardID, AbilityID}, CheckedValue, Duration}]}
									%end,
									ResultFx = (TFX -- RemoveFX) ++ ReplaceFX,
									case {RemoveFX, ReplaceFX} of
										{[], []} -> 
											case stack_pool:get_last_stack(self(), effect_change) of
												{ok, _} -> do_nithing;
												_ -> stack_pool:set_stack_option(self(), effect_change, no)
											end;
										_ -> stack_pool:set_stack_option(self(), effect_change, yes)
									end,											
									 update_player_effect(PlayerPid, ResultFx)
								end, TargetList).
%-------------------------------------------------------------------
exclude_target_from_active({CardOwner, CardOrder, CardID, AbilityID}, TCardOwner, TCardOrder, TCardID) ->
	[AbilityNo] = 
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> do(qlc:q( [X#card_ability.ability_no || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
		is_not_seal -> do(qlc:q( [X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]))
	end,
	{ok, AllConID} = mnesia_play:get_game_data(self(), continue_ability),
	A = list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, AllConID),
	case A of
		{value, {{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}} ->
			RemainTarget = remove_target_from_active(Receiver, TCardOwner, TCardOrder, TCardID),
			NewConID = (AllConID -- [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}]) ++  [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, RemainTarget}],
			mnesia_play:set_game_data(self(), continue_ability, NewConID);
		_ -> do_nothing
	end.
%-------------------------------------------------------------------
remove_target_from_active([], _TCardOwner, _TCardOrder, _TCardID) -> [];
remove_target_from_active([{WhoseRec, Receiver}|Receive], TCardOwner, TCardOrder, TCardID) ->
	%io:format("receiver ~p~n", [Receiver]),
	case Receiver -- [{TCardOwner, TCardOrder, TCardID}] of
		[] -> remove_target_from_active(Receive, TCardOwner, TCardOrder, TCardID);
		Remain -> [{WhoseRec, Remain}] ++ remove_target_from_active(Receive, TCardOwner, TCardOrder, TCardID)
	end.
	
			
set_ability_active({CardOwner, CardOrder, CardID, AbilityID}) ->
	%smo_logger:fmsg("set_ability_active {~p, ~p, ~p, ~p}~n", [CardOwner, CardOrder, CardID, AbilityID]),
	[AbilityNo] = 
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> do(qlc:q( [X#card_ability.ability_no || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
		is_not_seal -> do(qlc:q( [X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]))
	end,
	{ok, AllConID} = mnesia_play:get_game_data(self(), continue_ability),
	A = list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, AllConID),
	case A of
		{value, {{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}} ->
			ActiveCase = check_special_active(AbilityID),
			Active = [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ActiveCase, Receiver}],
			NewConID = (AllConID -- [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}]) ++  Active,
			mnesia_play:set_game_data(self(), continue_ability, NewConID);
		_ -> ""
	end.
	
set_ability_active({CardOwner, CardOrder, CardID, AbilityID}, [{TargetType, Target}]) ->
	%smo_logger:fmsg("set_ability_active {~p, ~p, ~p, ~p} : ~p ~n", [CardOwner, CardOrder, CardID, AbilityID, Target]),
	[AbilityNo] = 
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> do(qlc:q( [X#card_ability.ability_no || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID]));
		is_not_seal -> do(qlc:q( [X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityID]))
	end,
	{ok, AllConID} = mnesia_play:get_game_data(self(), continue_ability),
	A = list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, AllConID),
	case A of
		{value, {{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}} ->
			ActiveCase = check_special_active(AbilityID),
			NTargetType =
			case TargetType of
				target -> 4;
				_ -> 5
			end,
			Active = [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, ActiveCase, Receiver ++ [{NTargetType, Target}]}],
			NewConID = (AllConID -- [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}]) ++  Active,
			mnesia_play:set_game_data(self(), continue_ability, NewConID);
		_ -> ""
	end.
	
% set_ability_inactive({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}) ->
	% AllConID = get(continuous_id),
	% A = lists:keysearch({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, 1, AllConID),
	% case A of
		% {value, {{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}} ->
			% Active = [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, inactive, Receiver}],
			% NewConID = (AllConID -- [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}]) ++  Active,
			% put(continuous_id, NewConID);
		% _ -> ""
	% end.
	
list_search(_, []) -> [];
list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}|_]) -> {value, {{CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Status, Receiver}};
list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{{_CardOwner, _CardOrder, _CardID, _AbilityNo, _AbilityID}, _Status, _Receiver}|Tail]) -> list_search({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, Tail).

check_special_active(AbilityID) ->
	Case = do(qlc:q( [X#continue_condition.need_check_value || X <- mnesia:table(continue_condition), X#continue_condition.ability_id =:= AbilityID])),
	case Case of
		[n] -> active;
		[e] -> check_value;
		_ -> check_condition_value
	end.

%check_effect_value(_CardOwner, _CardOrder, _CardID, _AbilityID, [], _TargetList) -> [].
check_effect_value(_, _CardOwner, _CardOrder, _CardID, _AbilityID, [], _TargetList) -> [];
check_effect_value({TOwner, TOrder, TID}, CardOwner, CardOrder, CardID, AbilityID, [FxHead|Fx], TargetList) ->
	case FxHead of
		{check_condition, {Condition, FxTypeValue}} ->
			%io:format("check co ~p target ~p~n", [Condition, TargetList]),
			CardMatch = function_utility:card_match_condition(TargetList, Condition),
			case CardMatch of
				[] -> [{mismatch_condtion, effect}];
				_ -> 
					OppID = mnesia_play:get_opponent_pid(CardOwner),
					Value = 
					case check_effect_then_active_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID) of
						effect_active -> [FxTypeValue];
						condition_active -> [{mismatch_condtion, effect}];
						_ -> []
					end,
					Value ++ check_effect_value({TOwner, TOrder, TID}, CardOwner, CardOrder, CardID, AbilityID, Fx, TargetList)
			end;
		{check_environment, {Condition, FxTypeValue}} ->
			case function_utility:check_environment(TargetList, Condition) of
				true -> 
					OppID = mnesia_play:get_opponent_pid(CardOwner),
					case check_effect_then_active_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID) of
						effect_active -> [FxTypeValue];
						condition_active -> [{mismatch_condtion, effect}];
						_ -> []
					end;
				_ -> [{mismatch_condtion, effect}]
			end;
		{FxType,{Operator, Atom}} -> effect_value:check_value(CardOwner, CardOrder, CardID, {FxType, {Operator, Atom}}, {TOwner, TOrder, TID}) ++ check_effect_value({TOwner, TOrder, TID}, CardOwner, CardOrder, CardID, AbilityID, Fx, TargetList);
		_ -> [FxHead] ++ check_effect_value({TOwner, TOrder, TID}, CardOwner, CardOrder, CardID, AbilityID, Fx, TargetList)
	end;
check_effect_value(TOwner, CardOwner, CardOrder, CardID, AbilityID, [FxHead|Fx], TargetList) ->
	case FxHead of
		{check_condition, {Condition, FxTypeValue}} ->
			%io:format("check co ~p target ~p~n", [Condition, TargetList]),
			CardMatch = function_utility:card_match_condition(TargetList, Condition),
			case CardMatch of
				[] -> [{mismatch_condtion, effect}];
				_ -> 
					OppID = mnesia_play:get_opponent_pid(CardOwner),
					Value = 
					case check_effect_then_active_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID) of
						effect_active -> [FxTypeValue];
						condition_active -> [{mismatch_condtion, effect}];
						_ -> []
					end,
					Value ++ check_effect_value(TOwner, CardOwner, CardOrder, CardID, AbilityID, Fx, TargetList)
			end;
		{check_environment, {Condition, FxTypeValue}} ->
			case function_utility:check_environment(TargetList, Condition) of
				true -> 
					OppID = mnesia_play:get_opponent_pid(CardOwner),
					case check_effect_then_active_condition({CardOwner, CardOrder, CardID}, OppID, AbilityID) of
						effect_active -> [FxTypeValue];
						condition_active -> [{mismatch_condtion, effect}];
						_ -> []
					end;
				_ -> [{mismatch_condtion, effect}]
			end
	end.
		
check_effect_then_active_condition({CardOwner, CardOrder, CardID}, OppPid, AbilityID) ->
	case check_condition({CardOwner, CardOrder, CardID}, OppPid, AbilityID) of
		[ok, ok, ok, ok] -> effect_active;
		_ -> 
			case active_condition_check({CardOwner, CardOrder, CardID}, OppPid, AbilityID) of
				[ok] -> condition_active;
				_ -> condition_inactive
			end
	end.
	
	
get_player_effect(PlayerPid, Field) ->
	case Field of
		mp -> mnesia_play:get_player_data(PlayerPid, mp_rest);
		Field ->
			{ok, PlayerFx} = mnesia_play:get_player_data(PlayerPid, player_effect),
			get_player_effect_field(Field, PlayerFx)
	end.
	
update_player_effect(PlayerPid, Fx) ->
	mnesia_play:set_player_data(PlayerPid, player_effect, Fx).

get_player_effect_field(Field, []) ->
	case Field of
		card_hand_max -> 0;
		mp_max -> 0;
		_ -> []
	end;
get_player_effect_field(Field, [{_GFx, Fx, _Duration}|PlayerFx]) ->
	case Field of
		card_hand_max -> get_field(Field, Fx) + get_player_effect_field(Field, PlayerFx);
		mp_max -> get_field(Field, Fx) + get_player_effect_field(Field, PlayerFx);
		_ -> get_field(Field, Fx) ++ get_player_effect_field(Field, PlayerFx)
	end.

get_field(Field, []) ->
	case Field of
		card_hand_max -> 0;
		mp_max -> 0;
		_ -> []
	end;
get_field(Field, [{Field, Fx}|Effect]) ->
	case Field of
		card_hand_max ->
			Fx + get_field(Field, Effect);
		mp_max ->
			Fx + get_field(Field, Effect);
		_ ->
			[Fx] ++ get_field(Field, Effect)
	end;
get_field(Field, [_|Effect]) ->	get_field(Field, Effect).

check_player_got_effect(PlayerPid, player_effect, AbilIDCheck, CheckMatch) ->
	{ok, PlayerFx} = mnesia_play:get_player_data(PlayerPid, player_effect),
	Result = check_effect_id_got(PlayerFx, AbilIDCheck),
	attribute_check:check_count(Result, CheckMatch).

check_card_got_effect([], _, _) -> [];
check_card_got_effect([{CardOwner, CardOrder, CardID}|Target], AbilIDCheck, CheckMatch) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Result = check_effect_id_got(CardFx, AbilIDCheck),
	case attribute_check:check_count(Result, CheckMatch) of
		true -> [{CardOwner, CardOrder, CardID}] ++ check_card_got_effect(Target, AbilIDCheck, CheckMatch);
		_ -> check_card_got_effect(Target, AbilIDCheck, CheckMatch)
	end.
	
check_player_got_effect(PlayerPid, FxField, FxInterest) ->
	{ok, PlayerFx} = mnesia_play:get_player_data(PlayerPid, FxField),
	check_player_effect(PlayerFx, FxInterest).
	
check_player_effect(_PlayerFx, []) -> have_no_effect;
check_player_effect(PlayerFx, [InterestFx|Interest]) -> 
	case check_interest(PlayerFx, [InterestFx]) of
		have_effect -> have_effect;
		_ -> check_player_effect(PlayerFx, Interest)
	end.

check_interest([], _) -> have_no_effect;
check_interest([{{_, _, _, _}, InterestFx, _}|_], InterestFx) -> have_effect;
check_interest([{{_, _, _, _}, _, _}|Tail], InterestFx) -> check_interest(Tail, InterestFx);
check_interest([{{_, _, _, _, _}, InterestFx, _}|_], InterestFx) -> have_effect;
check_interest([{{_, _, _, _, _}, _, _}|Tail], InterestFx) -> check_interest(Tail, InterestFx).
	


check_effect_id_got([], _) -> [];
check_effect_id_got([{{_, _, _, AbilIDCheck}, _ReceiveFx, _}|Tail], AbilIDCheck) -> [AbilIDCheck] ++ check_effect_id_got(Tail, AbilIDCheck);
check_effect_id_got([{{_, _, _, _}, _, _}|Tail], AbilIDCheck) -> check_effect_id_got(Tail, AbilIDCheck);
check_effect_id_got([{{_, _, _, _, AbilIDCheck}, _ReceiveFx, _}|Tail], AbilIDCheck) -> [AbilIDCheck] ++ check_effect_id_got(Tail, AbilIDCheck);
check_effect_id_got([{{_, _, _, _, _}, _, _}|Tail], AbilIDCheck) -> check_effect_id_got(Tail, AbilIDCheck).
	