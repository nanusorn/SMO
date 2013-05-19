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
-module(m_skill_check).
-import (mnesia_table, [do/1]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("m_skill.hrl").

%===============================================================================
is_interfere(CardID) ->
	Result1 = do(qlc:q( [ X#mystic_skill.can_interfere|| X <- mnesia:table(mystic_skill), X#mystic_skill.card_id =:= CardID ])),
	%io:format('Result1 ~p~n', [Result1]),
	Result = function_utility:del(Result1),
	%io:format('Result ~p~n', [Result]),
	if
		Result =:= [y] -> lets_intefere;
		Result =:= [n] -> dont_think_about_it;
		true -> shit_something_wrong
	end.
%===============================================================================
seal_skill(CardID) ->
	io:format('CardID ~p~n', [CardID]),
	SskillList1 = do(qlc:q( [ X#mystic_skill.mskill_no|| X <- mnesia:table(mystic_skill), X#mystic_skill.card_id =:= CardID ])),
	SskillList =function_utility:qsort(SskillList1),
	DelDup = function_utility:del(SskillList),	
	SskillCount = length(DelDup),
	if
		SskillCount > 1 -> {need_to_select_one, DelDup};
		SskillCount =:= 1 -> {do_not_need_to_select, DelDup};
		true -> {error, this_is_seal_have_no_skill}
	end.
%===============================================================================
resource_use(CardID) ->
	Result1 = do(qlc:q( [ X#mystic_skill.skill_mp|| X <- mnesia:table(mystic_skill), X#mystic_skill.card_id =:= CardID ])),
	%io:format('Result1 ~p~n', [Result1]),
	Result = function_utility:del(Result1),
	if 
		Result =:= [] -> {error, 'this mystic have no skill'};
		Result =/= [] -> 
			case Result of
				[{Mp, Counter}] -> {ok, [{mp,Mp }, {counter, Counter}]};
				[{Counter}] -> {ok, [{counter, Counter}]};
				[Mp] -> {ok,[{mp, Mp}]}	
			end
	end.
			