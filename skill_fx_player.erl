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
-module('skill_fx_player').

-import(lists, [foreach/2]).

-export([set_player_effect/3]).
%-compile('export_all').

set_player_effect(Effect, ReceivePid, OpposePid) ->
	set_effect_to_player(Effect, ReceivePid, OpposePid).

set_effect_to_player(Effect, ReceivePid, OpposePid) ->
	foreach(fun({EffectType, EffectToDo}) -> 
						activate_effect_to_player({EffectType, EffectToDo}, ReceivePid, OpposePid)
					end, Effect).
		
activate_effect_to_player({EffectType, EffectToDo}, ReceivePid, OpposePid) ->
	case EffectType of
		mp ->
			Value = check_effect_todo(EffectToDo),
			set_effect(ReceivePid, mp_rest, Value),
			stack_pool:set_stack_option(self(), play, activate_effect_to_player),
			{ok, ReceiverMp} = mnesia_play:get_player_data(ReceivePid, mp_rest),
			{ok, OpposeMp} = mnesia_play:get_player_data(OpposePid, mp_rest),
			%io:format("Send update player mp ~p~n", [{ReceiverMp, OpposeMp}]),
			%%send(ReceivePid, [16#88, 16#76, 0, ReceiverMp, OpposeMp]),
			%%send(OpposePid, [16#88, 16#76, 0, OpposeMp, ReceiverMp]),
			gen_server:cast(ReceivePid, {send, [16#88, 16#76, 0, ReceiverMp, OpposeMp]}),
			gen_server:cast(OpposePid, {send, [16#88, 16#76, 0, OpposeMp, ReceiverMp]}),
			interfere_step:return_play();
		swap ->
			other_active_effect:swap_card(ReceivePid, EffectToDo),
			interfere_step:return_play()
	end.
		
set_effect(Receiver, PlayerField, Value) ->
	{ok, PlayerValue} = mnesia_play:get_player_data(Receiver, PlayerField),
	if
		PlayerValue + Value < 0 ->
			%io:format('set player data: 0 ~n'),
			mnesia_play:set_player_data(Receiver, PlayerField, 0);
		true ->
			%io:format('set player data: ~p~n', [PlayerValue + Value]),
			mnesia_play:set_player_data(Receiver, PlayerField, PlayerValue + Value)
	end.
	
check_effect_todo(EffectToDo) ->
	case is_integer(EffectToDo) of
		true -> EffectToDo;
		false -> 0
	end.