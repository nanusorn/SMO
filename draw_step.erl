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
-module (draw_step).

-import (lists, [append/2, split/2, flatlength/1]).

-compile (export_all).

% 401.3.1. ใน Draw Step ผู้เล่นเจ้าของ Subturn โจมตี จะต้องทำการจั่วการ์ด จากกองใดก็ได้ของตนเองโดยให้จั่วการ์ดได้ไม่ต่ำกว่า 1 ใบ
% แต่ไม่เกิน 2 ใบ ยกเว้นจะมี Effect ระบุให้กระทำนอกเหนือจากที่กล่าวไว้ --
% 401.3.3. ใน Draw Step ผู้เล่นทั้งสองฝ่ายไม่สามารถร่ายหรือสั่งการ Seal Card หรือ Mystic Card ใดๆทั้งที่เป็นและไม่เป็น Interfere
% แม้ว่าจะมี Interfere Step แทรกขึ้นมาก็ตาม ยกเว้นจะมี Effect ระบุให้ร่ายหรือสั่งการได้ใน Step นี้ --
into_step() ->
	{ok, PlayerPid} = mnesia_play:get_game_data(self(), player_turn),
% 401.3.2. ใน Subturn โจมตีของ Turn แรกของผู้เล่นทั้งสองฝ่ายจะข้าม Step นี้ไป --
	case mnesia_play:get_player_data(PlayerPid, first_turn) of
		{ok, first} ->
			%stack_pool:pop_stack_out (self()),
			play_utility:into_next_step();
		{ok, not_first} ->
			stack_pool:push_stack(self(), PlayerPid, 0, 0, [{play, play_draw_step}, {draw_card, 0}, {card_player, PlayerPid}]),
			interfere_step:return_play (check_play_step)
	end.

% 401.3.4. Ability เมื่อเข้าสู่ Draw Step ทำงาน
draw_step_4(PlayerPid) ->
	stack_pool:set_stack_option (self(), play, play_draw_step_4),
	interfere_step:return_play (check_play_step).

% 401.3.5. ทำการจั่วการ์ดในมือตามเงื่อนไขที่กำหนดในข้อ 401.3.1
draw_step_5 (PlayerPid) ->
	stack_pool:set_stack_option (self(), play, play_draw_step_5),
	MaxDraw = get_max_draw (PlayerPid),
	{ok, SealDeck} = mnesia_play:get_player_data (PlayerPid, seal_deck),
	{ok, MysticDeck} = mnesia_play:get_player_data (PlayerPid, mystic_deck),
	case SealDeck ++ MysticDeck of
		[] ->	
			case stack_pool:get_last_stack (self(), draw_card) of
				{ok, 0} ->
					gen_server:cast(self(), {update_game_end, PlayerPid, player_loss});
				{ok, _} ->
					gen_server:cast(self(), {force_skip_draw, PlayerPid}),
					stack_pool:pop_stack_out (self()),
					play_utility:into_next_step()
			end;
		_ ->	
			case stack_pool:get_last_stack (self(), draw_card) of
				{ok, MaxDraw} ->
					gen_server:cast(self(), {act_player_draw, 0}),
					stack_pool:pop_stack_out (self()),
					play_utility:into_next_step();
				% {ok, 0} ->
					% gen_server:cast(self(), {act_player_draw, MaxDraw});
				{ok, _Int} ->
					gen_server:cast(self(), {act_player_draw, 1})
			end
	end.

get_max_draw(PlayerPid) -> 2.

request_draw (PlayerPid, Data) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	{ok, SDeck} = mnesia_play:get_player_data (PlayerPid, seal_deck),
	{ok, MDeck} = mnesia_play:get_player_data (PlayerPid, mystic_deck),
	case Data of
		[0] ->
			SealDeckSize = flatlength(SDeck),
			if
				SealDeckSize > 0 ->
					{CardDraw, HandUpdate, SealDeck} = draw_card (Hand, SDeck),
					response_draw (PlayerPid, CardDraw, HandUpdate, SealDeck, MDeck);
				true ->
					response_redraw (PlayerPid)
			end;
		[1] ->
			MysticDeckSize = flatlength(MDeck),
			if
				MysticDeckSize > 0 ->
					{CardDraw, HandUpdate, MysticDeck} = draw_card (Hand, MDeck),
					response_draw (PlayerPid, CardDraw, HandUpdate, SDeck, MysticDeck);
				true ->
					response_redraw (PlayerPid)
			end;
		_ -> io:format("Draw msg data error ~n")
	end.

draw_card (Hand, [CardDraw | Deck]) ->
	{[CardDraw], Hand ++ [CardDraw], Deck}.

response_redraw (PlayerPid) ->
	MaxDraw = get_max_draw(PlayerPid),
	case stack_pool:get_last_stack (self(), draw_card) of
		{ok, MaxDraw} ->
			stack_pool:pop_stack_out (self()),
			play_utility:into_next_step();
		{ok, 0} ->
			gen_server:cast(self(), {redraw, 1});
		{ok, _} ->
			gen_server:cast(self(), {redraw, 0})
	end.

response_draw (PlayerPid, CardDraw, HandUpdate, SealDeck, MysticDeck) -> 
	mnesia_play:set_player_data (PlayerPid, hand_cards, HandUpdate),
	mnesia_play:set_player_data (PlayerPid, seal_deck, SealDeck),
	mnesia_play:set_player_data (PlayerPid, mystic_deck, MysticDeck),

	NumHandCards = flatlength(HandUpdate),
	NumSealDeck = flatlength(SealDeck),
	NumMysticDeck = flatlength(MysticDeck), 

	{ok, DrawCard} = stack_pool:get_last_stack (self(), draw_card),
	stack_pool:set_stack_option (self(), draw_card, DrawCard + 1),

	ResponseData = [16#85, 16#03, NumSealDeck, NumMysticDeck, NumHandCards],
	DrawPlayerOption = lib_arena_play:cards_id_two_byte(CardDraw),
	hand_zone:update_hand_data(),
	gen_server:cast(self(), {response_draw, ResponseData, DrawPlayerOption}).

draw_step_5_1 (PlayerPid) ->
	stack_pool:set_stack_option (self(), play, play_draw_step_5_1),	
	mod_ability_activate:check_any_ability_activate(on_hand, PlayerPid).
	
verify_onhand_draw_ability() ->
	stack_pool:set_stack_option (self(), play, verify_onhand_draw_ability),	
	mod_ability_activate:verify_ability_condition(on_hand).
	
activate_onhande_draw_effect() ->
	stack_pool:set_stack_option (self(), play, activate_onhand_draw_effect),	
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(on_hand, PlayerPid).

draw_step_5_2 (PlayerPid) ->
	stack_pool:set_stack_option (self(), play, play_draw_step_5_2),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play (check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().
