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
-module(special_server_function).
-compile(export_all).

require_to_hand(PlayerPid, CardType) ->
	Card =
	case CardType of
		[0] ->
			{ok, PlayerSeal} = mnesia_play:get_player_data(PlayerPid, seal_deck),
			PlayerSeal;
		[1] ->
			{ok, Mystic} = mnesia_play:get_player_data(PlayerPid, mystic_deck),
			Mystic
	end,
	TargetReply = get_reply(Card, PlayerPid, 0),
	gen_server:cast(self(), {server_select_skill_target, PlayerPid, 0, 99, 0, 1, TargetReply}).
	
	
get_reply([], _, TargetSize) -> [TargetSize];
get_reply([{{PlayerPid, CardOrder, CardID}, _}| T], PlayerPid, TargetSize) ->
	get_reply(T, PlayerPid, TargetSize + 1) ++ [1, CardOrder, <<CardID:16>>, 4, 0];
get_reply([{{_, CardOrder, CardID, _}, _} | T], PlayerPid, TargetSize) ->
	get_reply(T, PlayerPid, TargetSize + 1) ++ [0, CardOrder, <<CardID:16>>, 4, 0].

server_select_target(PlayerLists, SkillUser, TargetType, DialogCode, AmountType, Amount, TargetReply) ->
	lists:foreach(	fun ({PlayerPid, _}) ->
							case PlayerPid of
								SkillUser ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#ef, TargetType, DialogCode, AmountType, Amount] ++ TargetReply});
								_ -> ""
							end
						end, PlayerLists).

						
select_seal_to_hand(PlayerPid, TargetSelect) ->
	move_to_hand:move_card_to_hand(PlayerPid, TargetSelect). 

% do_card_to_hand(PlayerPid, CardOrder, CardID) ->
	% card_utility:change_card_zone(PlayerPid, CardOrder, CardID, seal_deck, hand_cards),
	% {ok, Hands} = mnesia_play:get_player_data(PlayerPid, hand_cards),
	% gen_server:cast(self(), {hand_card_add, lists:flatlength(Hands), [{PlayerPid, CardOrder, CardID}]}),
	% gen_server:cast(self(), {act_next_command}).