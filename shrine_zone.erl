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
-module (shrine_zone).

-import (lists, [append/2, flatlength/1, split/2]).

-export ([req_shrine_info/2, get_shrine_level/1, card_to_shrine/2, card_to_shrine/3, add_status_to_cards/1]).
-export ([get_shrine_data/2, update_shrine_level/2, get_shrine_msg/3, move_to_shrine_complete/0]).
-export ([check_update_shrine_level/0, count_shrine_level/0, card_to_shrine_change_number/0]).
%-export ([activate_select_target_ability/0]).
-export ([activate_shrine_interfere_2/0, shrine_level_update/0]).
-export([
						check_into_shrine_ability/1,
						check_in_shrine_ability/0,
						check_remove_from_arena_ability/1,
						interfere_card_to_shrine/0,
						verify_into_shrine_ability/0,
						verify_in_shrine_ability/0,
						verify_remove_from_arena_ability/0,
						remove_from_zone_card_status/0,
						activate_into_shrine_effect/0,
						activate_in_shrine_effect/0,
						activate_remove_from_arena_effect/0,
						activate_as_in_shrine_effect/0,
						pasted_mystic_to_shrine/1,
						decide_to_shrine/2
					]).

					
get_shrine_level (PlayerPid) ->
	{ok, Shrine} = mnesia_play:get_player_data(PlayerPid, shrine_cards),
	sum_shrine_level (Shrine).

sum_shrine_level ([]) -> 0;
sum_shrine_level ([{{_, _, CardID}, _} | T]) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{ok, Level} = mnesia_odbc:get_seal_data (CardID, level),
			Level + sum_shrine_level (T);
		is_not_seal ->
			sum_shrine_level (T)
	end.

get_shrine_data (ShrineCard, Field) ->
	{PlayerPid, CardOrder, CardID} = ShrineCard,
	case Field of
		player_pid -> {ok, PlayerPid};
		card_order -> {ok, CardOrder};
		card_id -> {ok, CardID}
	end.

get_data_reply (List, DataReply, CheckType) ->
	get_data_reply (List, DataReply, CheckType, 0).

get_data_reply ([], DataReply, _, NumCard) -> {DataReply, NumCard};
get_data_reply ([{{_, CardOrder, CardID}, _} | T], DataReply, CheckType, NumCard) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CheckType ->
			get_data_reply (T, DataReply ++ [CardOrder, <<CardID:16>>], CheckType, NumCard + 1);
		_ ->
			get_data_reply (T, DataReply, CheckType, NumCard)
	end.

get_test_data_reply (Type) ->
	case Type of
		seal ->
			[{{0, 1, 257}, 0}, {{0, 2, 258}, 0}, {{0, 3, 259}, 0}, {{0, 4, 260}, 0}, {{0, 5, 261}, 0}, 
			{{0, 6, 262}, 0}, {{0, 7, 263}, 0}, {{0, 8, 264}, 0}, {{0, 9, 265}, 0}, {{0, 10, 266}, 0}];
		mystic ->
			[{{0, 1, 356}, 0}, {{0, 2, 356}, 0}, {{0, 3, 357}, 0}, {{0, 4, 357}, 0}, {{0, 5, 358}, 0}, 
			{{0, 6, 358}, 0}, {{0, 7, 359}, 0}, {{0, 8, 359}, 0}, {{0, 9, 360}, 0}, {{0, 10, 360}, 0},
			{{0, 11, 361}, 0}, {{0, 12, 361}, 0}, {{0, 13, 362}, 0}]
	end.

req_shrine_info (ReplyPid, PlayerPid) ->
	case 1 of
		1 ->	{ok, ShrineCards} = mnesia_play:get_player_data(PlayerPid, shrine_cards),
			response_shrine_info (ReplyPid, ShrineCards);
		0 ->	ShrineCards = get_test_data_reply (seal) ++ get_test_data_reply (mystic),
			response_shrine_info (ReplyPid, ShrineCards)
	end.

response_shrine_info (ReplyPid, ShrineCards) ->
	{SealDR, SSize} = get_data_reply (ShrineCards, [], is_seal),
	{MysticDR, MSize} = get_data_reply (ShrineCards, [], is_not_seal),
	SealReply = [SSize] ++ SealDR,
	MysticReply = [MSize] ++ MysticDR,
	ReplyData = [16#8b, 16#00] ++ SealReply ++ MysticReply,
	io:format("Reply data ~p~n", [ReplyData]),
	gen_server:cast(self(), {res_shrine_info, ReplyPid, ReplyData}).

% --------------------------- Move to shrine Zone -------------------------------
% get_support_seal ([]) -> [];
% get_support_seal ([{PlayerPid, CardOrder, CardID} | T]) ->
	% case mnesia_odbc:is_seal_card (CardID) of
		% is_seal ->
			% case card_utility:check_card_zone(PlayerPid, CardOrder, CardID) of
				% arena_zone ->
					% {ok, Support, _MysticSupport} = arena_zone:break_support_seal(PlayerPid, CardOrder, CardID),
					% %smo_logger:fmsg("###get supprot seal then Support main seal and support seal are ~p~n", [Support]),
					% SupportNoData = exclude_support_data(Support),
					% %smo_logger:fmsg("####support with main are ~p~n", [SupportNoData]),
					% SupportNoMain = remove_main_seal(SupportNoData, PlayerPid, CardOrder, CardID),
					% %smo_logger:fmsg("Support without main seal is ~p~n", [SupportNoMain]),
					% SupportNoMain++ get_support_seal(T);
				% _ ->	get_support_seal (T)
			% end;
		% is_not_seal ->	get_support_seal(T)
	% end.
% 
% remove_main_seal ([], _, _, _) -> [];
% remove_main_seal ([{PlayerPid, CardOrder, CardID} | Support], PlayerPid, CardOrder, CardID) ->
	% remove_main_seal (Support, PlayerPid, CardOrder, CardID);
% % remove_main_seal([{{PlayerPid, CardOrder, CardID}, _} | Support], PlayerPid, CardOrder, CardID) ->
	% % remove_main_seal(Support, PlayerPid, CardOrder, CardID);
% remove_main_seal ([Card | Support], PlayerPid, CardOrder, CardID) ->
	% [Card] ++ remove_main_seal (Support, PlayerPid, CardOrder, CardID).

% exclude_support_data([]) -> [];
% exclude_support_data([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	% [{PlayerPid, CardOrder, CardID}] ++ exclude_support_data(T);
% exclude_support_data([{PlayerPid, CardOrder, CardID} | T]) ->
	% [{PlayerPid, CardOrder, CardID}] ++ exclude_support_data(T).

% add_status_to_cards ([]) -> [];
% add_status_to_cards ([{CardOwner, CardOrder, CardID} | T]) ->
	% CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	% smo_logger:fmsg("{~p~p~p} is on Zone ~p~n", [CardOwner, CardOrder, CardID, CardZone]),
	% case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, paste_to) of
		% {ok, Pasted} when is_list (Pasted) ->
			% card_utility:remove_mystic_effect_from_target ({CardOwner, CardOrder, CardID}, Pasted);
		% _ ->	no_action
	% end,
	% move_to_shrine_zone (CardOwner, CardOrder, CardID, CardZone),
	% [{CardOwner, CardOrder, CardID}] ++ add_status_to_cards (T).
	
	
add_status_to_cards(Cards) ->
	%io:format("card move to shrine ~p~n", [Cards]),
	lists:foreach(
								fun({CardOwner, CardOrder, CardID}) ->
									CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
									%smo_logger:fmsg("card destroy check card zone is ~p~n", [CardZone]),
									%card_utility:add_card_status(CardOwner, CardOrder, CardID, destroyed, CardZone),
											% case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to) of
												% {ok, Pasted} when is_list (Pasted) ->
													% card_utility:remove_mystic_effect_from_target({CardOwner, CardOrder, CardID}, Pasted);
												% _ ->	no_action
											% end,
									case mystic_effect:mystic_give_effect_to(CardOwner, CardOrder, CardID) of
										[] -> do_nothing;%io:format("Mystic {~p, ~p, ~p} doesnot give effect to ANY CARD ~n", [CardOwner, CardOrder, CardID]);
										ReceiveFx -> %io:format("card receive effect from mystic are ~p~n", [ReceiveFx]),
											AllPid = check_receive_pid(ReceiveFx),
											case AllPid  of
												[] -> lists:foreach(fun({SPid, SOrder, Sid}) -> end_of_subturn:remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid) end, ReceiveFx);
												_ -> lists:foreach(fun(ReceivePid) -> end_of_subturn:remove_player_mystic_effect(CardOwner, CardOrder, CardID, ReceivePid) end, AllPid)
											end
									end,
									move_to_shrine_zone(CardOwner, CardOrder, CardID, CardZone)
								end, Cards	).

check_receive_pid([]) -> [];
check_receive_pid([Receive|R]) ->
	case Receive of
		{_Pid, _Order, _ID} -> check_receive_pid(R);
		Pid -> [Pid] ++ check_receive_pid(R)
	end.
								
move_to_shrine_zone(CardOwner, CardOrder, CardID, CardZone) ->
	%OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	MoveFrom = from_zone_status(CardZone),
	smo_logger:fmsg("{~p, ~p, ~p} is add from zone ~p~n", [CardOwner, CardOrder, CardID, MoveFrom]),
	%ability_affect:remove_all_give_effect_target(CardOwner, CardOrder, CardID),
	%card_utility:add_card_status(CardOwner, CardOrder, CardID, destroyed, CardZone),
	{ok, RFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect),
	%{ok, SFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, skill_effect),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> {ok, Growth_Option} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, growth);
		is_not_seal -> Growth_Option = 0
	end,
	card_utility:remove_card(CardOwner, CardOrder, CardID, CardZone, CardOwner),
	case CardZone of
		hand_cards -> hand_zone:update_hand_data();
		_ -> do_noting
	end,
	%card_utility:remove_card(CardOwner, CardOrder, CardID, CardZone, OpponentPid),
	add_to_shrine(CardOwner, CardOrder, CardID, MoveFrom, RFx, Growth_Option).

from_zone_status (FromZone) ->
	case FromZone of
		hand_cards -> hand_to_shrine;
		arena_zone -> arena_to_shrine;
		support_cards -> arena_to_shrine;
		seal_deck -> deck_to_shrine;
		mystic_deck -> deck_to_shrine;
		_ -> io:format ("From zone out of range ~p~n", [FromZone])
	end.

add_to_shrine(CardOwner, CardOrder, CardID, MoveFrom, RFx, Growth_Option) ->
	{ok, Shrine} = mnesia_play:get_player_data(CardOwner, shrine_cards),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			CardOption = seal_card:set_create_option(CardID, [{card_status, moving_to_shrine}, {card_status, MoveFrom}]);
		is_not_seal ->
			CardOption = mystic_card:set_create_option(CardID, [{card_status, moving_to_shrine}, {card_status, MoveFrom}])
	end,
	ShrineUpdate = Shrine ++ [{{CardOwner, CardOrder, CardID}, CardOption}],
	mnesia_play:set_player_data(CardOwner, shrine_cards, ShrineUpdate),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, growth, Growth_Option);
		is_not_seal -> ""
	end,
	RFxUpdate = check_effect_though_in_shrine(CardOwner, CardOrder, CardID, RFx),
	card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RFxUpdate).

% effect ที่ส่งผลแม้การ์ดอยู่ใน shrine จะถูกใส่เพิ่มให้การ์ดที่ตกลง shrine ที่ฟังค์ชั่นนี้ -
check_effect_though_in_shrine (_, _, _, []) -> [];
check_effect_though_in_shrine (CardOwner, CardOrder, CardID, [{GFx, Fx, when_to_shrine} | CardFx]) ->
	case Fx of
		[{move, to_remove_zone}] ->
			io:format ("Add cards efffect to remove~n"),
			FxToTarget = [{GFx, Fx, when_to_shrine}],
			ActivateFx = [{GFx, [{target_ability, [{CardOwner, CardOrder, CardID}]}, {fx_target, to_remove_zone}, {fx_to_target, FxToTarget}], 0}],
			card_utility:set_card_option_field (CardOwner, CardOrder, CardID, receive_effect, ActivateFx),
			card_utility:add_card_status (CardOwner, CardOrder, CardID, have_ability_target),			
			card_utility:add_card_status (CardOwner, CardOrder, CardID, ability_activation);
		_ ->	io:format ("Other though to shrine effect ~p~n", [Fx])
	end,
	check_effect_though_in_shrine (CardOwner, CardOrder, CardID, CardFx);
check_effect_though_in_shrine (CardOwner, CardOrder, CardID, [_ | CardFx]) ->
	check_effect_though_in_shrine (CardOwner, CardOrder, CardID, CardFx).

pasted_mystic_to_shrine ([]) -> [];
pasted_mystic_to_shrine ([{CardOwner, CardOrder, CardID} | T]) ->
	MysticPaste = arena_zone:get_mystic_pasted(CardOwner, CardOrder, CardID),
	smo_logger:fmsg("Mystic pasted ~p~n", [MysticPaste]),
	case MysticPaste of
		[] ->	pasted_mystic_to_shrine (T);
		_ ->	MysticPaste ++ pasted_mystic_to_shrine (T)
	end.
% -----------------------------------------------------------------
decide_to_shrine(PlayerPid, ToShrineList) ->
	gen_server:cast(self(), {player_decide_change_zone, PlayerPid, shrine_cards, ToShrineList}).

% 703. ขั้นตอนการตก Shrine
card_to_shrine(PlayerPid, ToShrineList) -> card_to_shrine(PlayerPid, ToShrineList, other_destroy).
card_to_shrine(PlayerPid, ToShrineList, DestroyType) ->
	PastedMystic = pasted_mystic_to_shrine(ToShrineList),
	SupportSeal = card_utility:get_support_seal(ToShrineList),
	CardToShrine = ToShrineList ++ SupportSeal ++ PastedMystic,
	%CardToShrine = ToShrineList ++ PastedMystic,
	%CardToShrine = add_status_to_cards(ToShrineList ++ SupportSeal ++ PastedMystic),
	OtherZone = change_zone:check_card_to_other_zone(CardToShrine, shrine_cards),
	smo_logger:fmsg("card to shrine are ~p card to other zone are ~p~n", [CardToShrine, OtherZone]),
	case OtherZone of
		[] -> do_card_to_shrine(PlayerPid, CardToShrine, DestroyType);
		ToOtherZone -> 
			DestroyList = CardToShrine -- ToOtherZone,
			%smo_logger:fmsg("card destroy to shrine are ~p card to other zone are ~p~n", [DestroyList, ToOtherZone]),
			{ok, LastPlay} = stack_pool:get_last_stack(self(), play),
			put(last_play, LastPlay),
			stack_pool:set_stack_option(self(), play, from_shrine_to_oher_zone),
			stack_pool:set_stack_option(self(), card_to_other_zone, ToOtherZone),
			case DestroyList of
				[] -> interfere_step:return_play(check_play_step);
				_ -> 
					do_card_to_shrine(PlayerPid, DestroyList, DestroyType)
			end
	end.
	
do_card_to_shrine(PlayerPid, DestroyList, DestroyType) ->
	OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	StackOption = [{play, move_to_shrine}, {player, PlayerPid}, {opponent, OpponentPid}, {card_destroy, DestroyList}, {destroy_type, DestroyType}],
	stack_pool:push_stack(self(), PlayerPid, 0, 0, StackOption),
% 703.1 เมื่อ Seal หรือ Mysic Card ที่ถูกนำลง Shrine ด้วย Effect ต่างๆ จะเรียก Seal หรือ Mystic Card ที่ถูกนำลง Shrine ว่า การ์ดที่กำลังตก Shrine
	add_status_to_cards(DestroyList),
	% PastedMystic = pasted_mystic_to_shrine(DestroyList),
	% SupportSeal = get_support_seal (DestroyList),
	% CardDestroy = add_status_to_cards(DestroyList ++ SupportSeal ++ PastedMystic),
	
% 703.2 นำการ์ดที่ถูกนำลง Shrine ด้วย Effect ต่างๆ ลง Shrine โดยถือว่าการ์ดอยู่ใน Shrine ใน Phase นี้ -
% โดยเจ้าของการ์ดที่ถูกนำลง Shrine เป็นผู้จัดเรียงการ์ดลงบนด้านบนสุดของ Shrine
% หากการ์ดที่ถูกทำลายมี Mystic Card ติดอยู่ให้ทำการนำ Mystic Card ที่ติดอยู่ลง Shrine ใน Phase นี้ -
% การ์ดที่กำลังตก Shrine ทุกใบจะลืม Effect ทั้งหมดที่กระทำกับตัวมันนอกจาก Effect นั้นจะระบุไว้ว่ายังคงส่งผล แม้การ์ดนั้นอยู่ใน Shrine
	{ok, PlayerList} = mnesia_play:get_game_data(self(), player_list),
	%stack_pool:set_stack_option(self(), card_destroy, DestroyList),
	%hand_zone:update_hand_data(),
	case DestroyList of
		[] ->	interfere_step:return_play (check_play_step);
		_ ->	send_destroy_msg(PlayerList, DestroyList, DestroyType)
	end.
	
send_destroy_msg(PlayerList, DestroyList, DestroyType) ->
	case DestroyType of
		sacrifice ->
			lists:foreach(fun({PlayerPid, _}) ->
											ReplyMsg = function_utility:get_card_to_client(PlayerPid, DestroyList, 0),
											io:format("Send update sacrifice ~p~n", [ReplyMsg]),
											%smo_logger:fmsg("Send update destroy ~w", [ReplyMsg]),
											case mnesia_play:get_game_data(self(), game_step) of
												{ok, eos} ->
													gen_server:cast(self(), {update_change_zone, shrine_cards, PlayerPid, [16#88, 16#29, 204],ReplyMsg});
												_ ->
													gen_server:cast(self(), {update_change_zone, shrine_cards, PlayerPid, [16#88, 16#29],ReplyMsg})
												end
										end, PlayerList);
		_ ->
			lists:foreach(fun({PlayerPid, _}) ->
											ReplyMsg = function_utility:get_card_to_client(PlayerPid, DestroyList, 0),
											io:format("Send update destroy ~p~n", [ReplyMsg]),
											%smo_logger:fmsg("Send update destroy ~w", [ReplyMsg]),
											case mnesia_play:get_game_data(self(), game_step) of
												{ok, eos} ->
													gen_server:cast(self(), {update_change_zone, shrine_cards, PlayerPid, [16#88, 16#1e, 204],ReplyMsg});
												_ ->
													gen_server:cast(self(), {update_change_zone, shrine_cards, PlayerPid, [16#88, 16#1e],ReplyMsg})
												end
										end, PlayerList)
	end.
	
% send_destroy_msg ([], _) -> all_send;
% send_destroy_msg ([{PlayerPid, _} | T], DestroyList) ->
	% ReplyMsg = get_card_to_client (PlayerPid, DestroyList, 0),
	% %io:format("Send update destroy ~p~n", [ReplyMsg]),
	% smo_logger:fmsg("Send update destroy ~w", [ReplyMsg]),
	% case mnesia_play:get_game_data(self(), game_step) of
		% {ok, eos} ->
			% gen_server:cast(self(), {update_change_zone, PlayerPid, [16#88, 16#1e, 204],ReplyMsg});
		% _ ->
			% gen_server:cast(self(), {update_change_zone, PlayerPid, [16#88, 16#1e],ReplyMsg})
		% end,
	% send_destroy_msg (T, DestroyList).

card_to_shrine_change_number () ->
	stack_pool:set_stack_option (self(), play, play_card_to_shrine_change_number),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play (check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

% 703.3 หากเป็น Seal ให้นับ Level ใน Shrine
check_update_shrine_level () ->
	stack_pool:set_stack_option(self(), play, play_check_update_shrine_level),
	gen_server:cast(self(), {update_shrine_level}).

count_shrine_level() ->
	{ok, PlayerList} = mnesia_play:get_game_data(self(), player_list),
	ShrineList = get_player_shrine (PlayerList),
	case ShrineList of
		[{_, continues}, {_, continues}] -> interfere_step:return_play(check_play_step);
		[{X, lost}, {_, lost}] ->
			io:format("Send game end with result (GAME DRAW)~n"),
			stack_pool:remove_stack (self()),
			gen_server:cast(self(), {update_game_end, X, game_draw});
		[{LoserPid, lost}, {_, continues}] ->
			stack_pool:remove_stack (self()),
			gen_server:cast(self(), {update_game_end, LoserPid, player_loss});
		[{_, continues}, {LoserPid, lost}] ->
			stack_pool:remove_stack (self()),
			gen_server:cast(self(), {update_game_end, LoserPid, player_loss})
	end.

get_player_shrine ([]) -> [];
get_player_shrine ([{PlayerPid, _} | T]) ->
	PlayerShLv = get_shrine_level (PlayerPid),
	{ok, ShrineMax} = mnesia_play:get_game_data (self(), max_shrine),
	if
		%PlayerShLv >= ShrineMax -> [{PlayerPid, lost}] ++ get_player_shrine (T);
		PlayerShLv >= 15 -> [{PlayerPid, lost}] ++ get_player_shrine (T);
		%PlayerShLv >= 150 -> [{PlayerPid, lost}] ++ get_player_shrine (T);
		true -> [{PlayerPid, continues}] ++ get_player_shrine (T)
	end.

% 703.4 Ability ของ Seal หรือ Mystic Card ที่จะทำงานเมื่อตก Shrine หรือ เมื่อออกจากสนามถ้ามีการเลือกการทำงานให้เลือกใน Phase นี้ --
% และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ -
% 703.5 หากต้องทำการเลือกเป้าหมาย ให้เลือกเป้าหมายใน Phase นี้ --
check_into_shrine_ability(PlayerPid) ->
	stack_pool:set_stack_option (self(), play, check_into_shrine_ability),
	mod_ability_activate:check_any_ability_activate(into_shrine, PlayerPid).
	
check_remove_from_arena_ability(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, check_remove_from_arena_ability),
	mod_ability_activate:check_any_ability_activate(remove_from_arena, PlayerPid).
% ------------------ จบ 703.4, 703.5------------------------------

%703.6.  Interfere Step  
interfere_card_to_shrine() ->
	stack_pool:set_stack_option(self(), play, interfere_card_to_shrine),
	interfere_step:into_sub_interfere().
% ------------------ จบ 703.6------------------------------
	


																														%check_ability_select_target () ->
																															%stack_pool:set_stack_option(self(), play, activate_ability_select_target),
																															%{ok, DestroyedCards} = stack_pool:get_last_stack (self(), card_destroy),
																															%remove_from_zone_status (DestroyedCards),
																															%{ok, AllArena} = card_utility:get_all_card (arena_zone),
																															%{ok, AllShrine} = card_utility:get_all_card (shrine_cards),
																															%{ok, AllRemove} = card_utility:get_all_card (remove_cards),
																															%Cards = card_utility:get_cards_have_status (AllArena ++ AllShrine ++ AllRemove, ability_select_target),
																															%stack_pool:set_stack_option (self(), select_ability_target, Cards),
																															%interfere_step:return_play (check_play_step).

																														% activate_select_target_ability () ->
																															% case stack_pool:get_last_stack (self(), select_ability_target) of
																																% {ok, []} ->
																														% % 703.6 Interfere Step
																																	% {ok, AllArena} = card_utility:get_all_card (arena_zone),
																																	% {ok, AllShrine} = card_utility:get_all_card (shrine_cards),
																																	% {ok, AllRemove} = card_utility:get_all_card (remove_cards),
																																	% Cards = card_utility:get_cards_have_status (AllArena ++ AllShrine ++ AllRemove, have_ability_target),
																																	% remove_card_status (Cards, have_ability_target),
																																	% stack_pool:add_stack_option_field (self(), ability_target_selected_cards, Cards),
																																	% stack_pool:set_stack_option(self(), play, move_to_shrine_interfere),
																																	% interfere_step:into_sub_interfere();
																																% {ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
																																	% stack_pool:set_stack_option (self(), select_ability_target, Cards),
																																	% case card_utility:check_card_status (CardOwner, CardOrder, CardID, ability_activation) of
																																		% {ok, have_no_status} ->
																																			% interfere_step:return_play (check_play_step);
																																		% {ok, have_status} ->
																																			% card_utility:remove_card_status (CardOwner, CardOrder, CardID, ability_select_target),
																																			% % ใส่ลงไปในการ์ดที่เลือกเป้าหมายเรียบร้อยแล้วเพื่อรอตรวจสอบเป้าหมายและแสดงผล
																																			% stack_pool:add_stack_option_field (self(), ability_target_selected_cards, [{CardOwner, CardOrder, CardID}]),
																																			% ability_utility:check_ability_select_target (CardOwner, CardOrder, CardID)
																																	% end
																															% end.

																													% remove_card_status ([], _) -> ok;
																													% remove_card_status ([{PlayerPid, CardOrder, CardID} | Cards], StatusRemove) ->
																														% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
																														% card_utility:remove_card_status (PlayerPid, CardOrder, CardID, StatusRemove, Zone),
																														% remove_card_status (Cards, StatusRemove).

% 703.7 ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ 
% เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 703.5 ใหม่ หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ 
% สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
verify_into_shrine_ability() ->
	stack_pool:set_stack_option(self(), play, verify_into_shrine_ability),
	mod_ability_activate:verify_ability_condition(into_shrine).
	
verify_remove_from_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_remove_from_arena_ability),
	mod_ability_activate:verify_ability_condition(remove_from_arena).
	
% 703.8 Ability เมื่อตก Shrine, เมื่อออกจากสนาม, Ability ที่ทำงานใน Shrine หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดใน Shrine ตรงตามที่ กำหนดจะทำงานทันที --
activate_into_shrine_effect() ->
	stack_pool:set_stack_option(self(), play, activate_into_shrine_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_effect:check_any_ability_activate(into_shrine, PlayerPid).

activate_remove_from_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_remove_from_arena_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_effect:check_any_ability_activate(remove_from_arena, PlayerPid).
%------------------------จบ 703.7------------------------------
remove_from_zone_card_status() ->
	stack_pool:set_stack_option(self(), play, remove_from_zone_card_status),
	{ok, DestroyedCards} = stack_pool:get_last_stack(self(), card_destroy),
	lists:foreach(fun({PlayerPid, CardOrder, CardID}) -> remove_from_zone_status(PlayerPid, CardOrder, CardID) end, DestroyedCards),
	interfere_step:return_play(check_play_step).

remove_from_zone_status(PlayerPid, CardOrder, CardID) ->
	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	card_utility:remove_card_status (PlayerPid, CardOrder, CardID, hand_to_shrine, Zone),
	card_utility:remove_card_status (PlayerPid, CardOrder, CardID, arena_to_shrine, Zone),
	card_utility:remove_card_status (PlayerPid, CardOrder, CardID, deck_to_shrine, Zone).
%------------------------จบ 703.8-------------------------------
	

	
																													%check_card_ability_target () ->
																														%stack_pool:set_stack_option(self(), play, activate_ability_target_verify),
																														%case stack_pool:get_last_stack (self(), ability_target_selected_cards) of
																															%{ok, Cards} ->
																																%case ability_affect:ability_target_verify (Cards) of
																																	%[] ->	interfere_step:return_play(check_play_step);
																																	%_ ->	check_ability_select_target ()
																																%end;
																															%_ -> interfere_step:return_play (check_play_step)
																														%end.

																													% 703.8 Ability เมื่อตก Shrine, เมื่อออกจากสนาม, Ability ที่ทำงานใน Shrine หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดใน Shrine ตรงตามที่ กำหนดจะทำงานทันที --
																													%all_ability_affact (PlayerPid) ->
																														%stack_pool:set_stack_option (self(), play, activate_all_affect),
																														%case stack_pool:get_last_stack (self(), ability_activate) of
																															%{ok, Cards} ->
																																%CardsActivate = Cards;
																															%{error, _} ->
																																%CardsActivate = []
																														%end,
																														%{ok, DestroyedCards} = stack_pool:get_last_stack (self(), card_destroy),
																														%AddOnCard = card_utility:check_card_effect_duration (DestroyedCards, [{move, to_remove_zone}], when_to_shrine),
																													%	io:format ("Destroyed ~p Cards activate ability ~p~n", [DestroyedCards, CardsActivate ++ AddOnCard]),
																														%case CardsActivate ++ AddOnCard of
																															%[] ->	interfere_step:return_play (check_play_step);
																															%_ ->	ability_affect:check_have_to_arrange_ability_affect (PlayerPid, CardsActivate ++ AddOnCard, [], [])
																														%end.

% 703.9 Interfere Step
activate_shrine_interfere_2() ->
	stack_pool:set_stack_option (self(), play, shrine_interfere_2),
	interfere_step:into_sub_interfere ().

% 703.10 หากเป็น Seal ให้นับ Level ใน Shrine
shrine_level_update() ->
	stack_pool:set_stack_option (self(), play, check_count_shrine_level),
	gen_server:cast(self(), {update_shrine_level}).
	
check_in_shrine_ability() ->
	{ok, DestroyedCards} = stack_pool:get_last_stack(self(), card_destroy),
	lists:foreach(fun({CardOwner, CardOrder, CardID}) -> card_utility:add_card_status(CardOwner, CardOrder, CardID, move_to_shrine, shrine_cards) end, DestroyedCards),
	stack_pool:set_stack_option(self(), play, check_in_shrine_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_activate:check_any_ability_activate(in_shrine, PlayerPid).
	
verify_in_shrine_ability() ->
	stack_pool:set_stack_option(self(), play, verify_in_shrine_ability),
	mod_ability_activate:verify_ability_condition(in_shrine).
	
activate_in_shrine_effect() ->
	stack_pool:set_stack_option(self(), play, activate_in_shrine_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_effect:check_any_ability_activate(in_shrine, PlayerPid).
	
activate_as_in_shrine_effect() ->
	stack_pool:set_stack_option (self(), play, activate_as_in_shrine_effect),
	% case ability_effect:check_all_ability_affect() of
		% 0 -> interfere_step:return_play(check_play_step);
		% _ -> 	card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

% 703.11 การ์ดที่กำลังตก Shrine จะสูญเสียสภาพการ์ดที่กำลังตก Shrine ใน Phase นี้ --
move_to_shrine_complete() ->
	{ok, DestroyedCards} = stack_pool:get_last_stack (self(), card_destroy),
	lists:foreach(fun({PlayerPid, CardOrder, CardID}) -> remove_cards_status(PlayerPid, CardOrder, CardID) end, DestroyedCards),
	io:format("-- Move to shrine completed-~n"),
	interfere_step:return_play().
	
remove_cards_status(CardOwner, CardOrder, CardID) ->
	Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	io:format ("Remove move-to-shrine status~n"),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_shrine, Zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, move_to_shrine, Zone),
	effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [], update).

% 703.12 หากมี Effect ใดๆ ทำให้การ์ดที่กำลังตก Shrine เปลี่ยนไปยัง Zone อื่นได้สำเร็จ ให้การ์ดที่กำลังตก Shrine สูญเสียสภาพการ์ดที่กำลังตก Shrine ทันที --

% plyaer_list, reply_data
update_shrine_level ([], ReplyShrineLevel) -> ReplyShrineLevel;
update_shrine_level ([{PlayerPid, _} | T], ReplyShrineLevel) ->
	ShrineLevel = get_shrine_level (PlayerPid),
	update_shrine_level (T, ReplyShrineLevel ++ [{PlayerPid, ShrineLevel}]).

get_shrine_msg (_, [], ShrineMsg) -> ShrineMsg;
get_shrine_msg (PlayerPid, [{PlayerPid, ShrineLevel}|T], ShrineMsg) ->
	get_shrine_msg (PlayerPid, T, [ShrineLevel] ++ ShrineMsg);
get_shrine_msg (PlayerPid, [{_, ShrineLevel}|T], ShrineMsg) ->
	get_shrine_msg (PlayerPid, T, ShrineMsg ++ [ShrineLevel]).
