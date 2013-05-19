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
-module(special_effect).
-export(
				[
					check_effect/3,
					activate_effect/0,
					activate_second_effect/0,
					set_second_effect_target/3,
					effect_activated/0,
					get_target_as_require/3
				]).
				
check_effect(PlayerPid, Target, {FstEffect, FstTargetDo, CheckNext, SndEffect, SndTargetDo}) ->
	case CheckNext of 
		[{card_type, all}] ->
			PlayerOppPid = mnesia_play:get_opponent_pid(PlayerPid),
			SealDeck = card_list:player_zone_list({owner, [seal_deck]}, {PlayerPid, PlayerOppPid}),
			MysticDeck  = card_list:player_zone_list({owner, [mystic_deck]}, {PlayerPid, PlayerOppPid}),
			GetSeal = get_target_as_require(FstTargetDo, SealDeck, []),
			GetMystic = get_target_as_require(FstTargetDo, MysticDeck, []),
			FstTargetGot = GetSeal++GetMystic,
			case function_utility:card_match_condition(FstTargetGot, CheckNext) of
				[] -> just_first_effect(FstTargetGot, PlayerPid, FstEffect);
				Contain -> do_first_then_second(PlayerPid, FstEffect, FstTargetGot, Contain, SndEffect, SndTargetDo)
			end;	
		_ ->
			FstTargetGot = get_target_as_require(FstTargetDo, Target, []),
			smo_logger:fmsg("target as require for ~p are ~p ~n", [FstTargetDo, FstTargetGot]),
			case function_utility:card_match_condition(FstTargetGot, CheckNext) of
				[] -> just_first_effect(FstTargetGot, PlayerPid, FstEffect);
				Contain -> do_first_then_second(PlayerPid, FstEffect, FstTargetGot, Contain, SndEffect, SndTargetDo)
			end
	end.

get_target_as_require(all, Cards, _) -> Cards;
get_target_as_require(0, _, ResTarget) -> ResTarget;
get_target_as_require(_, [], ResTarget) -> ResTarget;
%get_target_as_require(TargetDo, [Target|Remain], ResTarget) -> get_target_as_require(TargetDo - 1, Remain, ResTarget ++ [Target]);
get_target_as_require(TargetDo, [{seal_deck, {CardOwner, CardOrder, CardID}}|Remain], ResTarget) -> get_target_as_require(TargetDo - 1, Remain, ResTarget ++ [{CardOwner, CardOrder, CardID}]);
get_target_as_require(TargetDo, [{mystic_deck, {CardOwner, CardOrder, CardID}}|Remain], ResTarget) -> get_target_as_require(TargetDo - 1, Remain, ResTarget ++ [{CardOwner, CardOrder, CardID}]);
get_target_as_require(TargetDo, [Target|Remain], ResTarget) -> get_target_as_require(TargetDo - 1, Remain, ResTarget ++ [Target]).
	
just_first_effect(FstTargetGot, PlayerPid, FstEffect) ->
	case FstEffect of
		show -> reveal_controller:activate_player_reveal(FstTargetGot, PlayerPid);
		_ -> interfere_step:return_play(check_play_step)
	end.
	
do_first_then_second(PlayerPid, FstEffect, FstTargetGot, Contain, SndEffect, SndTargetDo) ->
	{ok, GFX} = stack_pool:get_last_stack(self(), card_give_effect),
	StackOption = [{play, activate_first_effect}, {card_give_effect, GFX}, {first_effect, FstEffect}, {first_target, FstTargetGot}, {player, PlayerPid}, {second_effect, SndEffect}, {second_target_amount, SndTargetDo}, {second_target, Contain}],
	stack_pool:push_stack(self(), PlayerPid, 0, 0, StackOption),
	just_first_effect(FstTargetGot, PlayerPid, FstEffect).
	
activate_second_effect() ->
	{ok, Contain} = stack_pool:get_last_stack(self(), second_target),
	{ok, SndTargetDo} = stack_pool:get_last_stack(self(), second_target_amount),
	case SndTargetDo < length(Contain) of
		true -> 
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
			{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self(), card_give_effect),
			stack_pool:set_stack_option(self(), play, activate_second_effect),
			stack_pool:set_stack_option(self(), reselect_target, case_then_select),
			gen_server:cast(self(), {select_ability_target, PlayerPid, CardOwner, CardOrder, CardID, 1, SndTargetDo, 99, Contain});
		_ -> 
			stack_pool:set_stack_option(self(), play, activate_second_effect),
			stack_pool:set_stack_option(self(), last_target, Contain),
			activate_effect()
	end.
	
set_second_effect_target(_PlayerPid, _, [_|Data]) ->
	{ok, {CardOwner, _, _, _}} = stack_pool:get_last_stack(self(), card_give_effect),
	ReverseTarget = mod_ability_activate:reverse_data_to_card(CardOwner, Data),
	stack_pool:set_stack_option(self(), last_target, ReverseTarget),
	activate_effect().

activate_effect() ->
	{ok, Effect} = stack_pool:get_last_stack(self(), second_effect),
	case Effect of
		move_to_hand ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
			{ok, Target} = stack_pool:get_last_stack(self(), last_target),
			move_to_hand:move_card_to_hand(PlayerPid, Target);
		discard ->
			{ok, CardToShrine} = stack_pool:get_last_stack(self(), last_target),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
			shrine_zone:card_to_shrine(PlayerPid, CardToShrine);
		cast_without_paying_cost ->
			{ok, [{CardOwner, CardOrder, CardID} | _]} = stack_pool:get_last_stack(self(), last_target),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
			put(paying_cost, no),
			casting_card:check_card_cast (PlayerPid, CardOwner, CardOrder, CardID);
		{owner_mp, X} ->
			{ok,  PlayerPid} = stack_pool:get_last_stack(self(), player),
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			mod_ability_effect:activate_effect_to_player({mp, X}, PlayerPid, OpponentPid);
		{opponent_mp, X} ->
			{ok,  PlayerPid} = stack_pool:get_last_stack(self(), player),
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			mod_ability_effect:activate_effect_to_player({mp, X}, OpponentPid, PlayerPid);
		_ ->
			interfere_step:return_play(check_play_step)
	end.
	
effect_activated() ->
	stack_pool:pop_stack_out(self()),
	interfere_step:return_play(check_play_step).
