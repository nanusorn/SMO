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
-module(query_ability).
-compile (export_all).
-import (mnesia_table, [do/1]).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("ability_list.hrl").

no_of_ability(AbilityID) ->
	[NoOfAbility] = do(qlc:q([ X#card_ability.ability_no || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityID])),
	NoOfAbility.
	
id_of_ability(AbilityNo, CardID) ->
	do(qlc:q([ X#card_ability.ability_id || X <- mnesia:table(card_ability), X#card_ability.ability_no =:= AbilityNo, X#card_ability.card_id =:= CardID])).