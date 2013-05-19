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
-module(draw_card).
-export([
						draw/4,
						response_select_deck/2,
						return_to_draw/1,
						check_another_draw/0
					]).
					
% PlayerDrawPid คือ ผู้เล่นที่จะต้องจั่ว
% DrawDeck คือ Deck ที่จะต้องจั่ว
% DrawAmount คือ จำนวนที่จะต้องจั่ว ถ้าระบุจำนวนเลย ก็ เป็น 1, 2, 3 ... แต่ถ้าบอกเป็นช่วง เช่น {1, 3} คือ จั่วได้ อย่างน้อย 1 แต่ไม่เกิน 3
% Draw คือ จำนวนที่จั่วไปแล้ว
draw(PlayerDrawPid, DrawDeck, DrawAmount, Drew) ->
	% CanDraw คือ จำนวนการ์ดที่ยังจั่วได้
	% ShowDisDraw คือ ระบุว่าจะให้ Client แสดงปุ่ม ไม่จั่วการ์ดหรือไม่
	{CanDraw, ShowDisDraw} =
	case DrawAmount of
		{InitDraw, MaxDraw} -> 
			if 
				Drew >= InitDraw -> {MaxDraw - Drew, 1};
				true -> {MaxDraw - Drew, 0}
			end;
		Amount -> {Amount - Drew, 0}
	end,
	% DeckCard เอาไว้่แค่ check ว่าการ์ด ในกองที่จะต้องจั่วหมดหรือยัง ถ้าหมดแล้ว คือ ผู้เล่นนั้นแพ้
	% DeckAssign บอกว่า เป็น กองการ์ดไหนที่จะต้องจั่ว ถ้า 0 Seal คือ 1 คือ Mystic
	{DeckCard, DeckAssign}=
	case DrawDeck of
		whatever -> % case จั่วโดยไม่ได้กำหนดว่าต้องจั่วจากกองไหน
			{ok, MysticDeck} = mnesia_play:get_player_data (PlayerDrawPid, mystic_deck),
			{ok, SealDeck} = mnesia_play:get_player_data (PlayerDrawPid, seal_deck),
			{SealDeck ++ MysticDeck, [1, 1, ShowDisDraw]};
		_ -> % case อื่นๆ ระบุชัดเจนว่าต้องจั่วจาก กอง Seal หรือ Mystic
			{ok, Deck} = mnesia_play:get_player_data(PlayerDrawPid, DrawDeck),
			DeckRequire =
			case DrawDeck of
				seal_deck -> [0, 1, ShowDisDraw];
				mystic_deck -> [1, 0, ShowDisDraw]
			end,
			{Deck, DeckRequire}
	end,
	case DeckCard of
		[] -> % You lose
			io:format("---------------------no card left-----------------------~n"),
			gen_server:cast(self(), {update_game_end, PlayerDrawPid, player_loss});
		_ -> 
			stack_pool:push_stack(self(), PlayerDrawPid, 0, 0, [{draw_deck, DrawDeck}, {draw_remain, CanDraw}, {can_draw, DrawAmount}, {already_draw, Drew + 1}]),
			start_draw(PlayerDrawPid, DeckAssign)
	end.
	
start_draw(PlayerDrawPid, DeckAssign) ->
	gen_server:cast(self(), {player_select_deck_to_draw, PlayerDrawPid, DeckAssign}).

response_select_deck(PlayerDrawPid, DrawDeck) ->
	smo_logger:fmsg("deck client select is ~p~n", [DrawDeck]),
	case DrawDeck of
		[0] -> 
			{ok, SealDeck} = mnesia_play:get_player_data(PlayerDrawPid, seal_deck),
			[{{PlayePid, CardOrder, CardID}, _}|_] = SealDeck,
			move_card_to_hand(DrawDeck, {PlayePid, CardOrder, CardID});
		[1] ->
			{ok, MysticDeck} = mnesia_play:get_player_data(PlayerDrawPid, mystic_deck),
			[{{PlayePid, CardOrder, CardID}, _}|_] = MysticDeck,
			move_card_to_hand(DrawDeck, {PlayePid, CardOrder, CardID});
		[2] -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step)
	end.
	
move_card_to_hand(DrawDeck, {PlayePid, CardOrder, CardID}) ->
	stack_pool:set_stack_option(self(), play, draw_card),
	stack_pool:set_stack_option(self(), deck_select, DrawDeck),
	stack_pool:set_stack_option(self(), card_draw, {PlayePid, CardOrder, CardID}),
	move_to_hand:move_card_to_hand(PlayePid, [{PlayePid, CardOrder, CardID}]).
	
return_to_draw(PlayerDrawPid) ->
	{ok, HandCard} = mnesia_play:get_player_data(PlayerDrawPid, hand_cards),
	{ok, SealDeck} = mnesia_play:get_player_data(PlayerDrawPid, seal_deck),
	{ok, MysticDeck} = mnesia_play:get_player_data(PlayerDrawPid, mystic_deck),
	NumHandCards = length(HandCard),
	NumSealDeck = length(SealDeck),
	NumMysticDeck = length(MysticDeck),
	stack_pool:set_stack_option(self(), play, check_another_draw),
	ResponseData = [NumSealDeck, NumMysticDeck, NumHandCards],
	gen_server:cast(self(), {update_player_collection, PlayerDrawPid, ResponseData}).

	
check_another_draw() ->
	case stack_pool:get_last_stack(self(), draw_remain) of
		{ok, 1} ->
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, DrawRemain} ->
			smo_logger:fmsg("card can draw is ~p~n", [DrawRemain]),
			{ok, DrawDeck} = stack_pool:get_last_stack(self(), draw_deck),
			{ok, {PlayerDrawPid, _, _, _}} = stack_pool:get_last_stack(self()),
			{ok, Drew} = stack_pool:get_last_stack(self(), already_draw),
			{ok, DrawAmount} = stack_pool:get_last_stack(self(), can_draw),
			stack_pool:pop_stack_out(self()),
			draw(PlayerDrawPid, DrawDeck, DrawAmount, Drew)
	end.
			
