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
-module (discard_step).

-import (lists, [append/2, flatlength/1]).

-export ([into_step/0, check_player_discard/2, check_to_discard/1]).
%-export ([calculate_ability_player_effect/6]).

into_step () ->
	case mnesia_play:get_game_data(self(), player_turn) of
		{ok, PlayerPid} ->
			stack_pool:push_stack(self(), PlayerPid, 0, 0, [{play, player_discard}, {card_player, PlayerPid}]),
			ability_into_discard(),
			check_to_discard (PlayerPid);
		{error, Reason} ->
			io:format("Error ~p~n", [Reason])
	end.

ability_into_discard() ->
	ok.

check_to_discard (PlayerPid) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	HandSize = lists:flatlength(Hand),
	check_hand_size (PlayerPid, HandSize).

check_player_hand_max (PlayerPid) ->
	%OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	%PlayerFx = get_max_hand_effect (PlayerPid, player),
	PlayerFx = continuous_ability:get_player_effect(PlayerPid, card_hand_max),
	%OpponentFx = get_max_hand_effect (OpponentPid, opponent),
	7 + PlayerFx.

check_hand_size (PlayerPid, HandSize) ->
	HandMax = check_player_hand_max (PlayerPid),
	io:format("Player ~p HandMax ~p~n", [PlayerPid, HandMax]),
	if 
		%% แก้ไขให้ตรวจสอบจาก Max Hand Size
		HandSize > HandMax ->
			gen_server:cast(self(), {act_player_discard});
		true ->
			stack_pool:pop_stack_out (self()),
			play_utility:into_next_step ()
	end.

check_player_discard (PlayerPid, Data) ->
	case list_to_binary(Data) of
		<<CardOrder:8, CardID:16>> ->
			discard:player_discard(PlayerPid, [{PlayerPid, CardOrder, CardID}]);
		_ ->	io:format("!! Discard data error~n")
	end.

% หาจำนวนสูงสุดของการ์ดในมือผู้เล่นจาก Ability Fx ของการ์ด
% get_max_hand_effect (PlayerPid, TargetType) ->
	% Seals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
	% Mystics = card_utility:get_all_card (PlayerPid, mystic_card, arena_zone),
	% check_hand_max_effect (Seals ++ Mystics, TargetType).

% check_hand_max_effect ([], _) -> 0;
% check_hand_max_effect ([{{PlayerPid, CardOrder, CardID}, _} | T], TargetType) ->
	% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% {ok, Ability} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Zone),
	% FxValue = calculate_ability_player_effect (PlayerPid, CardOrder, CardID, Ability, hand_max, TargetType),
	% FxValue + check_hand_max_effect (T, TargetType).

% calculate_ability_player_effect (_, _, _, [], _, _) -> 0;
% calculate_ability_player_effect (PlayerPid, CardOrder, CardID, [{_, _, TargetType, _, _} | T], AbilityCheck, TargetType) ->
	% {ok, RFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, arena_zone),
	% %{ok, SFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, arena_zone),
	% FxValue = check_have_effect (PlayerPid, CardOrder, CardID, RFx ++ SFx, AbilityCheck),
	% FxValue + calculate_ability_player_effect (PlayerPid, CardOrder, CardID, T, AbilityCheck, TargetType);
% calculate_ability_player_effect (PlayerPid, CardOrder, CardID, [_ | T], AbilityCheck, TargetType) ->
	% calculate_ability_player_effect (PlayerPid, CardOrder, CardID, T, AbilityCheck, TargetType).

% check_have_effect (_, _, _, [], _) -> 0;
% check_have_effect (PlayerPid, CardOrder, CardID, [{_, Effect, _} | T], AbilityCheck) ->
% %	io:format ("Fx ~p Ability check ~p~n", [Effect, AbilityCheck]),
	% FxValue = check_effect (PlayerPid, CardOrder, CardID, Effect, AbilityCheck),
	% FxValue + check_have_effect (PlayerPid, CardOrder, CardID, T, AbilityCheck).	

% check_effect (_, _, _, [], _) -> 0;
% check_effect (PlayerPid, CardOrder, CardID, [{AbilityCheck, {_, Value}} | T], AbilityCheck) ->
	% FxValue = ability_activate:check_value_data (PlayerPid, CardOrder, CardID, Value),
	% FxValue + check_effect (PlayerPid, CardOrder, CardID, T, AbilityCheck);
% check_effect (PlayerPid, CardOrder, CardID, [_ | T], AbilityCheck) -> check_effect (PlayerPid, CardOrder, CardID, T, AbilityCheck).