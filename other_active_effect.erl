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
-module(other_active_effect).
-export(
					[
						paste_mystic_to_other_target/2,
						move_mystic_to_other_target/3,
						move_remain_to_arena/0,
						verify_move_mystic_to_arena/3,
						activate_mystic_to_target/3,
						swap_card/2
					]).
					
paste_mystic_to_other_target(PlayerPid, MysticList) ->
	[{CardOwner, CardOrder, CardID}|List] = MysticList,
	stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{card_player, PlayerPid}, {play, active_remove_mystic_effect}, {other_mystic_remain, List}]),
	{ok, [{PasteOwner, PasteOrder, PasteID}]} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to),
	stack_pool:set_stack_option(self(), pasted_target, {PasteOwner, PasteOrder, PasteID}),
	ReceiveFx = mystic_give_effect_to(CardOwner, CardOrder, CardID),
	lists:foreach(
								fun({SPid, SOrder, Sid}) -> 
									remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid),
									stack_pool:add_stack_option_field(self(), except_target, [{SPid, SOrder, Sid}])
								end, ReceiveFx),
	continuous_ability:check_continuous_target().
	
move_mystic_to_other_target(CardOwner, CardOrder, CardID) ->
	%smo_logger:fmsg("all remove mystic ability id are ~p~n", [stack_pool:get_last_stack(self(), all_mystic_abilityid)]),
	case stack_pool:get_last_stack(self(), all_mystic_abilityid) of
		{ok, MysticID} ->
			[RemainID|_] = function_utility:del_duplicate(MysticID),
			[AbilityNo] = new_mystic_check:check_id_number(RemainID), % AbilityNo is List
			%smo_logger:fmsg("Ability Number ~p~n", [AbilityNo]),
			check_mystic_move_condition(CardOwner, CardOrder, CardID, AbilityNo);
		_ -> move_remain_to_arena()
	end.
	
check_mystic_move_condition(CardOwner, CardOrder, CardID, AbilityNumber) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	MstZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	case new_mystic_check:check_casting_condition({MstZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) of
		{ok, can_cast} -> select_mystic_target(CardOwner, CardOrder, CardID, AbilityNumber);
		_ -> 
			stack_pool:set_stack_option(self(), play, cannot_move_mystic_then_to_shrine),
			shrine_zone:card_to_shrine(CardOwner, [{CardOwner, CardOrder, CardID}]) 
	end.
	
select_mystic_target(CardOwner, CardOrder, CardID, AbilityNumber) ->
	stack_pool:set_stack_option(self(), mystic_ability_number, AbilityNumber),
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	% [{TargetType, MAbilityID, {SelectType, SelectAmount, Target}}|_]  = IDHaveFx
	MstZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	IDHaveFx = new_mystic_check:check_mystic_ability_target({MstZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid),
	%smo_logger:fmsg("all potential Ability Id Are ~p~n", [IDHaveFx]),
	stack_pool:set_stack_option(self(), all_potential_mystic_ability, IDHaveFx),
	stack_pool:set_stack_option(self(), play, mystic_to_other_select_target),
	new_mystic_check:select_mystic_target().
	
verify_move_mystic_to_arena(CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		arena_zone ->
			{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			MstZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
			case new_mystic_check:check_casting_condition({MstZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) of
				{ok, can_cast} -> verify_mystic_target(CardOwner, CardOrder, CardID);
				_ -> move_remain_to_arena()
			end;
		_ -> move_remain_to_arena()
	end.
	
verify_mystic_target(CardOwner, CardOrder, CardID) ->
	%stack_pool:set_stack_option(self(), play, verify_mystic_target),
	%{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
	stack_pool:set_stack_option(self(), play, verify_mystic_to_other_target),
	new_mystic_check:verify_mystic_target(CardOwner, CardOrder, CardID).
	
activate_mystic_to_target(CardOwner, CardOrder, CardID) ->
	%mystic_effect:update_ability_continues (),
	{ok, MAbility} = stack_pool:get_last_stack (self(), result_target),
%	io:format ("Mystic ability effect ~p~n", [MAbility]),
	case MAbility of
		[] ->
			%stack_pool:remove_stack_option (self(), target_selected),
			check_send_fx_and_duration_mystic(CardOwner, CardOrder, CardID);
		[MAbilityTuple| MAbilityRest] ->
			stack_pool:set_stack_option(self(), result_target, MAbilityRest),
			%stack_pool:add_stack_option_field(self(), mystic_ability_activated, [MAbilityTuple]),
			stack_pool:set_stack_option(self(), play, mystic_to_other_activate_mystic_effect),			
			mystic_effect:set_activate_mystic_to_target(CardOwner, CardOrder, CardID, MAbilityTuple)
	end.	
	
check_send_fx_and_duration_mystic(CardOwner, CardOrder, CardID) ->
	DurationMystic = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, duration, arena_zone),
	case DurationMystic of
		{ok, 0} ->	
			stack_pool:set_stack_option(self(), play, move_mystic_arena_zero_turn),
			%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			shrine_zone:card_to_shrine(CardOwner, [{CardOwner, CardOrder, CardID}]);
		{ok, _} -> 
			stack_pool:set_stack_option(self(), play, move_mystic_to_other_target),
			send_move_mystic_to_target(CardOwner, CardOrder, CardID)%move_remain_to_arena()
	end.
	
send_move_mystic_to_target(CardOwner, CardOrder, CardID) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	{ok, {PreOwner, PreOrder, PreID}} = stack_pool:get_last_stack(self(), pasted_target),
	{ok, [{TOwner, TOrder, TID}]} = stack_pool:get_last_stack(self(), {target_selected, target}),
	case mystic_effect:check_target_cancel_mystic(CardOwner, CardOrder, CardID, [{TOwner, TOrder, TID}]) of
		[] -> shrine_zone:card_to_shrine(CardOwner, [{CardOwner, CardOrder, CardID}]);
		_ ->
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, paste_to, [{TOwner, TOrder, TID}]),
			card_utility:set_card_option_field(TOwner, TOrder, TID, mystic, [{CardOwner, CardOrder, CardID}]),
			gen_server:cast(self(), {move_msytic_to_other_target, PlayerPid, {CardOwner, CardOrder, CardID}, {PreOwner, PreOrder, PreID}, {TOwner, TOrder, TID}})
	end.
	
move_remain_to_arena() ->
	case stack_pool:get_last_stack(self(), other_mystic_remain) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, Remain} ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			paste_mystic_to_other_target(PlayerPid, Remain);
		_ ->
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step)
	end.
	
mystic_give_effect_to(CardOwner, CardOrder, CardID) ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, give_effect) of
		{ok, GFx} -> extract_target_give_effect(GFx);
		_ -> []
	end.
	
extract_target_give_effect([]) -> [];
extract_target_give_effect([{_MAbilityID, Target, _Duration}|Tail]) -> Target ++ extract_target_give_effect(Tail);
extract_target_give_effect([_|Tail]) -> extract_target_give_effect(Tail).	

% remove_player_mystic_effect(CardOwner, CardOrder, CardID, PlayerPid) ->
	% {ok, PlayerFx} = mnesia_play:get_player_data(PlayerPid, player_effect),
	% RFxRemain = remove_mystic_effect(PlayerPid, PlayerFx, CardOwner, CardOrder, CardID),
	% mnesia_play:set_player_data(PlayerPid, player_effect, RFxRemain).
	
% remove_mystic_effect(_, [], _, _, _) -> [];
% remove_mystic_effect(PlayerPid, [PlayerFx|PFx], CardOwner, CardOrder, CardID) ->
	% case PlayerFx of
		% {{CardOwner, CardOrder, CardID, MAbilityID}, _Fx, depend_on_s} ->
			% stack_pool:add_stack_option_field(self(), all_mystic_abilityid, [MAbilityID]),
			% remove_mystic_effect(PlayerPid, PFx, CardOwner, CardOrder, CardID);
		% _ ->	[PlayerFx] ++ remove_mystic_effect(PlayerPid, PFx, CardOwner, CardOrder, CardID)
	% end.
	
remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid) ->
	case card_utility:get_card_option_field(SPid, SOrder, Sid, receive_effect) of
		{ok, RFx} ->
			RFxUpdate = remove_mystic_effect(SPid, SOrder, Sid, RFx, CardOwner, CardOrder, CardID),
			card_utility:update_card_option_field(SPid, SOrder, Sid, receive_effect, RFxUpdate, arena_zone);
		_ -> card_not_on_zone
	end,
	case card_utility:get_card_option_field(SPid, SOrder, Sid, mystic, arena_zone) of
		{ok, Mystic} ->
			PastedUpdate = remove_mystic_from_seal(Mystic, {CardOwner, CardOrder, CardID}),
			card_utility:update_card_option_field(SPid, SOrder, Sid, mystic, PastedUpdate, arena_zone);
		_ -> card_not_on_zone
	end,
	effect_activate:send_update_activate_effect(SPid, SOrder, Sid, [], update).
	
remove_mystic_effect(_, _, _, [], _, _, _) -> [];
remove_mystic_effect(SPid, SOrder, Sid, [CardFx | CardFxRest], CardOwner, CardOrder, CardID) ->
	case CardFx of
		{{CardOwner, CardOrder, CardID, MAbilityID}, Fx, _} ->
			% Check ว่า MAbilityID นั้นเป็น continuous หรือเปล่า ถ้่าใช้ให้ไปลบ
			[IsContinuous] = new_mystic_check:is_continuous(MAbilityID),
			case IsContinuous of
				y -> continuous_ability:exclude_target_from_active({CardOwner, CardOrder, CardID, MAbilityID}, SPid, SOrder, Sid);
				_ -> do_nothing
			end,
			effect_activate:send_update_activate_effect(SPid, SOrder, Sid, [{{CardOwner, CardOrder, CardID, MAbilityID}, Fx}], remove),
			stack_pool:add_stack_option_field(self(), all_mystic_abilityid, [MAbilityID]),
			remove_mystic_effect(SPid, SOrder, Sid, CardFxRest, CardOwner, CardOrder, CardID);
		_ ->	[CardFx] ++ remove_mystic_effect(SPid, SOrder, Sid, CardFxRest, CardOwner, CardOrder, CardID)
	end.
	
remove_mystic_from_seal ([], _) -> [];
remove_mystic_from_seal ([RemovePasted | T], RemovePasted) -> T;
remove_mystic_from_seal ([H | T], RemovePasted) -> [H] ++ remove_mystic_from_seal (T, RemovePasted).

swap_card(PlayerPid, {SwapCase, Deck}) ->
	case SwapCase of
		head_to_tail -> 
			{ok, [ZoneHead|Other]} = mnesia_play:get_player_data(PlayerPid, Deck),
			mnesia_play:set_player_data(PlayerPid, Deck, Other ++ [ZoneHead]);
		tail_to_head ->
			{ok, [ZoneHead|Other]} = mnesia_play:get_player_data(PlayerPid, Deck),
			Last = lists:last([ZoneHead|Other]),
			mnesia_play:set_player_data(PlayerPid, Deck, [Last] ++ Other)
	end.
	
	
	
