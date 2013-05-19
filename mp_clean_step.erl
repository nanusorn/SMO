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
-module (mp_clean_step).

-compile (export_all).

into_step() ->
	case mnesia_play:get_game_data (self(), max_mp) of
		{ok, MaxMp} ->
			refill_mp (MaxMp);
		{error, Reason} ->
			io:format("Query command error : ~p~n", [Reason])
	end.

refill_mp (MaxMp) ->
	case mnesia_play:get_game_data (self(), player_turn) of
		{ok, PlayerTurn} ->
			MpMaxChange = continuous_ability:get_player_effect(PlayerTurn, mp_max),
			%MpMaxChange = continuous_ability:get_player_effect(PlayerTurn, mp_max),
			mnesia_play:set_mp_rest(PlayerTurn, MaxMp + MpMaxChange),
			gen_server:cast(self(), {update_clean_mp, PlayerTurn, MaxMp + MpMaxChange});
		{error, Reason} ->
			io:format("Query command error : ~p~n", [Reason])
	end.

% get_max_mp_change (PlayerPid, TargetType) ->
	% Seals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
	% Mystics = card_utility:get_all_card (PlayerPid, mystic_card, arena_zone),
	% check_mp_max_effect (Seals ++ Mystics, TargetType).

% check_mp_max_effect ([], _) -> 0;
% check_mp_max_effect ([{{PlayerPid, CardOrder, CardID}, _} | T], TargetType) ->
	% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% {ok, Ability} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Zone),
	% FxValue = discard_step:calculate_ability_player_effect (PlayerPid, CardOrder, CardID, Ability, mp_max, TargetType),
	% FxValue + check_mp_max_effect (T, TargetType).