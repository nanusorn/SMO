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
-module (assign_atk_controller).

-import (lists, [foreach/2]).

-compile (export_all).

rejected_attack(_PlayerPid, _CardOrder, _CardID, AtomicReason) ->
	Reason = 
	case AtomicReason of
		no_target_attack -> 0;
		seal_not_ready -> 1;
		not_enough_mp -> 2;
		already_atk_to_hand -> 3;
		no_hand_card -> 4;
		disallow_assign_attack -> 5;
		hand_target_mismatch -> 6;
		cancel_attack -> 7;
		controller_not_allow -> 8;
		_Other -> -1
	end,
	gen_server:cast(self(), {cancel_attack, Reason}),
	case stack_pool:get_last_stack(self(), play) of
		 {ok, PlayStep} -> interfere_step:return_play(PlayStep);
		 _ -> gen_server:cast(self(), {act_next_command})
	end.

assign_atk_check(PlayerPid, Data) ->
	case list_to_binary(Data) of
		<<WhoCard:8, CardOrder, CardID:16>> ->
			CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
			check_attack_condition(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->	io:format("Data error from ~p~n", [Data])
	end.

check_attack_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	case new_assign_atk:seal_status(PlayerPid, CardOwner, CardOrder, CardID, pre_interfere) of
		{attack} ->
			new_assign_atk:request_assign_attack(PlayerPid, CardOwner, CardOrder, CardID);
		{cancel_attack, Reason} ->
			rejected_attack(PlayerPid, CardOrder, CardID, Reason);
		Other ->
			rejected_attack(PlayerPid, CardOrder, CardID, unknow)
	end.

activate_select_attack_action(PlayerLists, PlayPid, CardOwner, CardOrder, CardID) ->
	foreach(fun ({PlayerPid, _}) ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			{ATChange, DFChange, SPChange, MAChange} = get_power_change(CardOwner, CardOrder, CardID, CardFx, {0, 0, 0, 0}),
			%{ATChange, DFChange, SPChange, MAChange} = {0,0,0,0},
			case PlayerPid of
				CardOwner ->
					WhoCard = 1;
				_ ->	WhoCard = 0
			end,
			case PlayerPid of
				PlayPid ->
					gen_server:cast(PlayerPid, {send, [16#88, 16#17, WhoCard, CardOrder, <<CardID:16>>, ATChange + 10, DFChange + 10, SPChange + 10, MAChange + 10]});
				_ ->	
					gen_server:cast(PlayerPid, {send, [16#88, 16#17, 16#cc]})
			end
		end, PlayerLists).

player_select_attack_action(PlayPid, PlayerPid, Data) ->
	case PlayerPid of
		PlayPid -> 
			case Data of
				[AttackAction] ->
					new_assign_atk:player_select_attack_action(PlayerPid, AttackAction);
				_ ->	io:format("Select attack action data error : ~p~n", [Data])
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_attack_action)
	end.

get_power_change(_, _, _, [], Result) -> Result;
get_power_change(OwnerPid, CardOrder, CardID, [{_, Fx, _} | CardFx], {IAt, IDf, ISp, IMa}) ->
	{A, D, S, M} = check_fx_power(OwnerPid, CardOrder, CardID, Fx, {0, 0, 0, 0}),
	get_power_change(OwnerPid, CardOrder, CardID, CardFx, {IAt + A, IDf + D, ISp + S, IMa + M}).	

check_fx_power(_, _, _, [], Result) -> Result;
check_fx_power(PlayerPid, CardOrder, CardID, [{PowerType, Value} | Fx], {IA, ID, IS, IM}) ->
	%Power = ability_activate:check_value_data (PlayerPid, CardOrder, CardID, Value),
	{RA, RD, RS, RM} = 
	case PowerType of
		at -> {Value, 0, 0, 0};
		df -> {0, Value, 0, 0};
		sp -> {0, 0, Value, 0};
		ma -> {0, 0, 0, Value};
		_ ->	io:format ("Power type ~p value ~p~n", [PowerType, Value]),
			{0, 0, 0, 0}
	end,
	check_fx_power(PlayerPid, CardOrder, CardID, Fx, {IA + RA, ID + RD, IS + RS, IM + RM}).

activate_attacker (PlayerLists, PlayPid, PlayerMp, CardOwner, Card) ->
%	CardOwner = check_card_controller(CardOwner, Card),
	foreach (	fun ({PlayerPid, _}) ->
		case PlayPid of
			PlayerPid ->
				MpData = [1, PlayerMp],
				CardData = [1] ++ Card;
			_ ->	
				MpData = [0, PlayerMp],
				CardData = [0] ++ Card
		end,
%		case CardOwner of
%			PlayerPid ->
%				CardData = [1] ++ Card;
%			_ ->	CardData = [0] ++ Card
%		end,
		gen_server:cast(PlayerPid, {send, [16#88, 16#10] ++ CardData ++ MpData})
	end, PlayerLists).

%check_card_controller(CardOwner, Card) ->
%	[ CardOrder, CardSN ] = Card,
%	<<CardID:16>> = list_to_binary([CardSN]),
%		case card_utility:check_card_controller(CardOwner, CardOrder, CardID) of
%			controller_allow -> CardOwner;
%			controller_not_allow -> OpponentPid
%		end.

player_select_zone_attack(PlayerLists, PlayPid) ->
	foreach(
						fun ({PlayerPid, _}) ->
							case PlayPid of
								PlayerPid ->gen_server:cast(PlayPid, {send, [16#88, 16#19]})	;
								_ ->	""
							end
						end, PlayerLists).

activate_select_atk_target (PlayerLists, PlayPid, TargetMsg) ->
	foreach (	fun ({PlayerPid, _}) ->
		case PlayPid of
			PlayerPid -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#13] ++ TargetMsg});
			_ -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#13]})
		end
	end, PlayerLists).

player_select_attack_target(PlayerPid, PlayPid, Data) ->
	case PlayerPid of
		PlayPid ->
			case list_to_binary(Data) of
				<<WhoCard:8, CardOrder, CardID:16>> ->
					CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
					new_assign_atk:assign_attack_target(CardOwner, CardOrder, CardID);
				_ ->	io:format("Player select attack target data error ~p~n", [Data])
			end;
		_ ->	play_utility:out_of_turn(PlayerPid, player_select_attack_target)
	end.

update_attack_target (PlayerLists, CardOwner, CardOrder, CardID) ->
	foreach (	fun ({PlayerPid, _}) ->
		case PlayerPid of
			CardOwner -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#14, 1, CardOrder, <<CardID:16>>]});
			_ -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#14, 0, CardOrder, <<CardID:16>>]})
		end
	end, PlayerLists).
	
% cancel_atk_check (PlayerPid) ->
	% case smo_arena:is_your_turn (PlayerPid) of
		% 1 ->	assign_atk:rejected_attack (cancel_attack);
		% 0 ->	play_utility:out_of_turn (PlayerPid, cancel_attack)
	% end.