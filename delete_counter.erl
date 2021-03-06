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
-module(delete_counter).
-export([check_effect/3]).

check_effect(0, FxChecked, FxNeedCheck) -> {0, FxChecked++ FxNeedCheck};
check_effect(Delete, FxChecked, []) -> {Delete, FxChecked};
check_effect(Delete, FxChecked, [{GiveFx, Fx, Duration}|Tail]) ->
	case check_counter(Delete, [], Fx) of
	{DeleteRemain, []} ->
		case DeleteRemain of
			0 ->
				{0, FxChecked++Tail};
			_ ->
				check_effect(DeleteRemain, FxChecked, Tail)
		end;
	{DeleteRemain, EffectRemain} ->
		case DeleteRemain of
			0 ->
				{0, FxChecked++[{GiveFx, EffectRemain, Duration}]++ Tail};
			_ ->
				check_effect(DeleteRemain, FxChecked++[{GiveFx, EffectRemain, Duration}], Tail)
		end
	end.

check_counter(0, EffectChecked, EffectNeedCheck) -> {0, EffectChecked++EffectNeedCheck};
check_counter(Delete, EffectChecked, []) -> {Delete, EffectChecked};
check_counter(Delete, EffectChecked, [Effect|Tail]) ->
	case Effect of
		{counter, X} ->
			if
				X > Delete ->
					check_counter(0, EffectChecked++[{counter, X-Delete}]++[Effect]--[{counter, X}], Tail);
				X  =< Delete ->
					check_counter(Delete-X, EffectChecked, Tail)
			end;
		_ ->
			check_counter(Delete, EffectChecked++[Effect], Tail)
	end.

%delete_counter:check_effect(1, [], [{a, [{df, 4}, {counter,0}, {at, 3}], dura}, {b, [{at, 3}, {counter, 5}, {df, 12}], dura1}]).  

