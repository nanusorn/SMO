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
-module (mystic_effect_activate_set1).

-import (lists, [flatlength/1]).

-compile (export_all).

% ------------------- 96 - Cosmocyclic ผู้เล่นทุกคนทิ้ง Mystic Card ทุกใบในมือ จากนั้นผู้เล่นทุกคนจั่ว Mystic Card อย่างน้อย 1 ใบแต่ไม่เกินจำนวน Mystic Card ที่ตนทิ้งไป ----
check_cards_draw (PlayerPid) ->
	case stack_pool:get_last_stack (self(), play) of
		{ok, attack_subturn_draw} ->
			stack_pool:set_stack_option (self(), play, activate_attack_subturn_draw);
		{ok, defend_subturn_draw} ->
			stack_pool:set_stack_option (self(), play, activate_defend_subturn_draw)
	end,
	{ok, DrawType} = stack_pool:get_last_stack (self(), draw_type),
	case DrawType of
		mystic_card_upto ->
			check_player_draw (PlayerPid);
		_ ->	io:format ("Other draw type ~p~n", [DrawType])
	end.

check_player_draw (PlayerPid) ->
	stack_pool:set_stack_option (self(), mystic_effect, cosmo_cyclic),
	{ok, Current} = stack_pool:get_last_stack (self(), current_draw),
	{ok, Max} = stack_pool:get_last_stack (self(), max_draw),
	case Current of
		0 ->	player_select_draw (PlayerPid, 1);
		_ ->	if Current >= Max ->
				interfere_step:return_play (check_play_step);
			   true ->
				gen_server:cast(self(), {activate_select_mystic_option, PlayerPid, 16#02, []})
			end
	end.

player_select_draw (PlayerPid, Data) ->
	case Data of
		0 -> interfere_step:return_play ();
		1 -> player_select_draw_card (PlayerPid)
	end.

player_select_draw_card (PlayerPid) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	{ok, SDeck} = mnesia_play:get_player_data (PlayerPid, seal_deck),
	{ok, MDeck} = mnesia_play:get_player_data (PlayerPid, mystic_deck),
	case stack_pool:get_last_stack (self(), draw_card) of
		{ok, mystic_card} ->
			MysticDeckSize = flatlength(MDeck),
			if
				MysticDeckSize > 0 ->					
					{CardDraw, HandUpdate, MysticDeck} = draw_card (Hand, MDeck),
					response_draw (PlayerPid, CardDraw, HandUpdate, SDeck, MysticDeck);
				true -> check_lost_by_fx ()
			end;
		{ok, seal_card} ->
			SealDeckSize = flatlength(SDeck),
			if
				SealDeckSize > 0 ->
					{CardDraw, HandUpdate, SealDeck} = draw_card (Hand, SDeck),
					response_draw (PlayerPid, CardDraw, HandUpdate, SealDeck, MDeck);
				true -> check_lost_by_fx ()
			end;
		_ -> io:format("Draw msg data error ~n")
	end.

draw_card (Hand, Deck) ->
	{CardDraw, RestDeck} = lists:split (1, Deck),
	{CD, _} = hand_zone:get_hand_data (CardDraw, []),
	{CardDraw, Hand ++ CD, RestDeck}.

check_lost_by_fx () ->
	case stack_pool:get_last_stack (self(), current_draw) of
		{ok, 0} ->	io:format ("Must set player lost~n");
		{ok, _} -> interfere_step:return_play (check_play_step)
	end.

response_draw (PlayerPid, [{{_, CardOrder, CardID}, _}], HandUpdate, SealDeck, MysticDeck) -> 
	mnesia_play:set_player_data (PlayerPid, hand_cards, HandUpdate),
	mnesia_play:set_player_data (PlayerPid, seal_deck, SealDeck),
	mnesia_play:set_player_data (PlayerPid, mystic_deck, MysticDeck),
	
	%ability_activate:check_update_ability (on_hand, PlayerPid),

	NumHandCards = flatlength(HandUpdate),
	NumSealDeck = flatlength(SealDeck),
	NumMysticDeck = flatlength(MysticDeck),

	{ok, Current} = stack_pool:get_last_stack (self(), current_draw),
	{ok, Max} = stack_pool:get_last_stack (self(), max_draw),

	if Current < Max ->
		Play = stack_pool:get_last_stack (self(), play),
		case Play of
			{ok, activate_attack_subturn_draw} ->
				stack_pool:set_stack_option (self(), play, attack_subturn_draw);
			{ok, activate_defend_subturn_draw} ->
				stack_pool:set_stack_option (self(), play, defend_subturn_draw)
		end;
	   true -> limit_draw
	end,
	
	stack_pool:set_stack_option (self(), current_draw, Current + 1),

	gen_server:cast(self(), {update_select_mystic_fx, PlayerPid, 16#02, [NumHandCards, NumSealDeck, NumMysticDeck, CardOrder, <<CardID:16>>]}).