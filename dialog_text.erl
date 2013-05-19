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
-module(dialog_text).

-export([text_on_panel/1,text_code/1]).

text_code(Fx) -> text_on_panel([{[], Fx, []}]).
	
text_on_panel([{_, Fx, _}]) ->
	case Fx of
		[{action, discard}] ->	1;
		[{action, destroy}] -> 2;
		[{action, move_to_hand}] -> 3;
		[{action, to_remove_zone}] -> 4;
		[{action, sacrifice}] -> 5;
		[{player_action, sacrifice}] -> 5;
		[{action, move_to_arena}] -> 6;
		[{action, assign_break_combine}] -> 7;
		[{action, change_line}] -> 8;
		[{action, move_to_at_line}] -> 9;
		[{action, attach_to_s}] -> 10;
		[{action, draw_1_card}] -> 11;
		[{action, break_combine}] -> 7;
		[{combat, {when_attack, {df, power}}}, {combat, {when_target, {power, df}}}] -> 12;
		%[{move, to_remove_zone}] -> 15;
		
		[{atk, once}] -> 20;
		%[{at, '=9'}, {atk, '=2'}] -> 21;
		%[{atk, once}, {atk, once}, {atk, once}] -> 22;

		
		[{heal_curse, _}] -> 25;
		[{heal_selected_curse, _}] -> 25;

		[{curse, stone_curse}, {curse, poison_curse}] -> 26;
		[{curse, last_dance_curse}, _] -> 26;
		[{curse, _}] -> 26;
		
		[{protect_curse, _}] -> 27;
				
		[{curse, {_, all}}] -> 28;
		
		[{attack, to_df}] -> 31;
		[{at, to_seal_being_combine}] -> 32;

		[{at, '+R'}] -> 30;
		[{at, '+T'}] -> 30;
		[{at, '+S'}] -> 30;
		[{at, _}] -> 30;
		
		[{df, '+Q'}] -> 35;
		%[{df, _}] -> 35;
		
		[{ms, 1}] -> 37;
		[{ms, 1}, {ma, 1}, {mc, 1}] -> 38;
		
		[{mp, '+T'}] -> 40;
		[{mp, _}] -> 40;
		
		[{mc, _}] -> 41;
		
		_ -> 0
	end.