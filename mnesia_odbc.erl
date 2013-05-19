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
-module (mnesia_odbc).

-include_lib ("stdlib/include/qlc.hrl").
-include ("record.hrl").
-include ("s_skill.hrl").

-import (mnesia_table, [do/1]).

-import (lists, [foreach/2, append/2, nth/2]).

-compile (export_all).

% store_card_data (Ref) ->
	% query_seal_card (Ref),
	% query_mystic_card(Ref),
	% query_combination(Ref).
% 
% query_seal_card (Ref) ->
	% SealCommand = "Select * From smno.data_seal Order by set_id, card_no",
	% case lib_database:sql_query(SealCommand) of
		% {selected, _, Result} ->
			% Table = [append_seal_data(X) || X <- Result],
			% insert_card_data(Table, seal_card);
		% {error, Reason} ->
			% io:format("Query command error : ~p~n", [Reason])
	% end.
% 
% query_mystic_card (Ref) ->
	% MysticCommand = "Select * From smno.data_mystic Order by set_id, card_no",
	% case lib_database:sql_query( MysticCommand) of
		% {selected, _, Result} ->
			% Table = [append_mystic_data(X) || X <- Result],
			% insert_card_data(Table, mystic_card);
		% {error, Reason} ->
			% io:format("Query command error : ~p~n", [Reason])
	% end.

% insert_card_data(Data, TableName) ->
% %%	io:format("Select From ~p : ~p~n", [TableName, Data]),
	% mnesia:clear_table(TableName),
	% F = fun () ->
			% foreach(fun mnesia:write/1, Data)
		% end,
	% mnesia:transaction(F).

% seperate_data (Data) ->
	% seperate_2_byte (Data, []).

% seperate_2_byte ([], SepData) -> SepData;
% seperate_2_byte (null, SepData) -> SepData;
% seperate_2_byte ([B1, B2 | T], SepData) ->
	% <<B:16>> = <<B1, B2>>,
	% seperate_2_byte(T, append(SepData, [B]));
% seperate_2_byte ([Data], _) ->
	% io:format("<mnesia_odbc> Data seperate 2 byte error is ~p~n", [Data]).

%% à¸—à¸³à¸?à¸²à¸?à¹?à¸?à¸? Hard Code à¹?à¸§à¹?à¸?à¹?à¸­à¸? à¹?à¸?à¹?à¹?à¸?à¹€à¸¡à¸·à¹?à¸­à¹?à¸”à¹?à¸£à¸±à¸? Store Proc à¸?à¸²à¸? PSql
%% {card_id, card_set, card_no, card_name, card_type, card_element, card_naming, mp_cast, mp_atk, level, attack, defend, speed, rarity, combine}
% append_seal_data (List) ->
	% {CardSet, CardNo, CardName, Level, Rarity, Element, Race, Type, Attack, Defend, Speed, Mp, Interfere, TextBox} = List,
	% <<CardID:16>> = <<CardSet, CardNo>>,
	% MpLen = string:len(Mp),
	% IndexSlash = string:rchr(Mp, $/),
	% MpCast = string:sub_string(Mp, 1, IndexSlash - 1),
	% MpAtk = string:sub_string(Mp, IndexSlash + 1, MpLen),
	% Ty = seperate_data(Type),
	% El = seperate_data(Element),
	% Nm = seperate_data(Race),
	% {seal_card, CardID, CardSet, CardNo, [CardName], Ty, El, Nm, list_to_integer(MpCast), list_to_integer(MpAtk), Level, Attack, Defend, Speed, Rarity, TextBox}.

%% {card_id, card_set, card_no, card_name, card_type, paste_type, rarity, is_interfere, mp_cast, p_turn, ability, exception}
% append_mystic_data (List) ->
	% {CardSet, CardNo, CardName, Rarity, UsingType, IsInterfere, MpCast, PTurn, CardType, Exception} = List,
	% <<CardID:16>> = <<CardSet, CardNo>>,
	% Duration = PTurn * 2,
	% Except = play_utility:separate_comma_data(Exception),
	%{mystic_card, CardID, CardSet, CardNo, CardName, CardType, UsingType, Rarity, IsInterfere, MpCast, Duration, [], Except, []}.

%% à¸ªà¸³à¸«à¸£à¸±à¸?à¸?à¸²à¸£à¹?à¸ªà¹?à¸?à¸·à¹?à¸­à¸?à¸­à¸? Table à¹€à¸?à¸·à¹?à¸­à¹?à¸«à¹? Mnesia à¸ªà¸²à¸¡à¸²à¸£à¸–à¸?à¸³à¹?à¸?à¹?à¸?à¹?à¸?à¸²à¸?à¹?à¸”à¹?
% append_table_name (TableName, Element) ->
	% L = tuple_to_list(Element),
	% list_to_tuple ([TableName] ++ L).

is_seal_card (CardID) ->
	Query = qlc:q([X || X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]),
	case do(Query) of
		[_] -> is_seal;
		[] -> is_not_seal
	end.

is_skill(ID) ->
	Query = qlc:q([X || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= ID]),
	case do(Query) of
		[_] -> true;
		[] -> false
	end.
	
	
%% {card_id, card_set, card_no, card_name, card_type, card_element, card_naming, mp_cast, mp_atk, level, attack, defend, speed, rarity, combine, ability}
get_seal_data (CardID) ->
	Query = qlc:q([{
				X#seal_card.card_set, 
				X#seal_card.card_no, 
				X#seal_card.card_name, 
				X#seal_card.card_type, 
				X#seal_card.card_element,
				X#seal_card.card_naming, 
				X#seal_card.mp_cast, 
				X#seal_card.mp_atk, 
				X#seal_card.level, 
				X#seal_card.attack, 
				X#seal_card.defend, 
				X#seal_card.speed, 
				X#seal_card.rarity}||X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_combat_data (CardID) ->
	Query = qlc:q([{X#seal_card.attack, X#seal_card.defend, X#seal_card.speed} ||
				X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_seal_data (CardID, Field) ->
	case get_seal_data(CardID) of
		{ok, Result} ->
			return_field_data (Result, Field);
		{error} ->
			{error, "<mnesia odbc> Get seal data Error!!!"}
	end.

%% {CardID, CardSet, CardNo, CardName, CardType, CardElement, CardNaming, MpCast, MpAtk, Level, Attack, Defend, Speed, Rarity, Combine, Ability}
%% ability = {AbilityType, MajorEffect, MinorEffect, SubMinorEffect}
return_field_data ({CardSet, CardNo, CardName, CardType, CardElement, CardNaming, MpCast, MpAtk, Level, Attack, Defend, Speed, Rarity}, FieldData) ->
	case FieldData of
		card_set -> {ok, CardSet};
		card_no -> {ok, CardNo};
		card_name -> {ok, CardName};
		card_type -> {ok, CardType};
		card_element -> {ok, CardElement};
		card_naming -> {ok, CardNaming};
		mp_cast -> {ok, MpCast};
		mp_atk -> {ok, MpAtk};
		level -> {ok, Level};
		attack -> {ok, Attack};
		defend -> {ok, Defend};
		speed -> {ok, Speed};
		rarity -> {ok, Rarity};
		_ ->	{error, "<mnesia odbc> Get " ++ [FieldData] ++ " not found"}
	end.

get_all_support_check (CardID) ->
	case is_seal_card (CardID) of
		is_seal ->	
			{ok, CardName} = get_seal_data (CardID, card_name),
			{ok, CardType} = get_seal_data (CardID, card_type),
			{ok, CardElement} = get_seal_data (CardID, card_element),
			{ok, CardNaming} = get_seal_data (CardID, card_naming),
			CardName ++ CardType ++ CardElement ++ CardNaming;
		is_not_seal -> []
	end.

get_mystic_data (CardID) ->	
	Query = qlc:q([{	X#mystic_card.card_id, 
										X#mystic_card.card_set, 
										X#mystic_card.card_no, 
										X#mystic_card.card_name, 
										X#mystic_card.card_type,
										X#mystic_card.card_subtype,
										X#mystic_card.paste_type, 
										X#mystic_card.card_naming, 
										X#mystic_card.is_interfere, 
										X#mystic_card.mp_cast, 
										X#mystic_card.duration} || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_mystic_data (CardID, Field) ->
	case get_mystic_data(CardID) of
		{ok, Result} ->
			get_mystic_field (Result, Field);
		{error} ->
			{error, "<mnesia odbc> Get mystic data error!!!"}
	end.

%% {CardID, CardSet, CardNo, CardName, CardType, UsingType, Rarity, Interfere, MpCast, PasteTurn, Ability, Exception}
get_mystic_field ({CardID, CardSet, CardNo, CardName, CardType, CardSubtype, UsingType, Rarity, Interfere, MpCast, Duration}, FieldData) ->
	case FieldData of
		card_id -> {ok, CardID};
		card_set -> {ok, CardSet};
		card_no -> {ok, CardNo};
		card_name -> {ok, CardName};
		card_type -> {ok, CardType};
		card_subtype -> {ok, CardSubtype};
		paste_type -> {ok, UsingType};
		rarity -> {ok, Rarity};
		interfere -> {ok, Interfere};
		mp_cast -> {ok, MpCast};
		duration -> {ok, Duration};
		_ -> {error, "<mnesia odbc> Get Mystic field not found"}
	end.

check_mp_cast (CardID) ->
	case is_seal_card (CardID) of
		is_seal -> get_seal_data (CardID, mp_cast);
		is_not_seal -> get_mystic_data (CardID, mp_cast)
	end.

% query_combination (Ref) ->
	% CombineCommand = "Select * From smno.data_combination Order by set_id, card_no, combine_order",
% %	CombineCommand = "Select * From smno.data_combination Order by set_id, card_no, option",
	% case lib_database:sql_query( CombineCommand) of
		% {selected, _, Result} ->
			% [append_combination_data(X) || X <- Result];
		% {error, Reason} ->
			% io:format("Query command error : ~p~n", [Reason])
	% end.

% combination = {card_id, option_list}
% option_list = {sub_1, sub_2, sub_3, sub_4, sub_5, atk, def, spd, mp, atk_type}
append_combination_data (Row) ->
	{CardSet, CardNo, OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType} = Row,
	<<CardID:16>> = <<CardSet, CardNo>>,
	case get_combination_data (CardID) of
		{ok, OptionList} ->
			insert_option (CardID, OptionList, OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType);
		{error} ->
			add_combination (CardID, OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType)
	end.

create_combination_row (CardID, OptionList) ->
	#combination{ card_id = CardID, option_list = OptionList}.

insert_option (CardID, OptionList, OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType) ->
	UpdateOption = OptionList ++ [{OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType}],
	Row = create_combination_row (CardID, UpdateOption),
	F =	fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).
	
add_combination (CardID, OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType) ->
	Row = create_combination_row (CardID, [{OptionNum, S1, S2, S3, S4, S5, Atk, Def, Spd, Mp, AtkType}]),
	F =	fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).

get_combination_data (CardID) ->
	Query = qlc:q([X#combination.option_list || X <- mnesia:table(combination), X#combination.card_id =:= CardID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_combination_support (CardID, OptionNumber) when is_integer (OptionNumber) ->
	case get_combination_data (CardID) of
		{ok, Result} -> get_option_support_seal (nth(OptionNumber, Result));
		{error} -> io:format("Get combination data error from card id ~p~n", [CardID])
	end;
get_combination_support (CardID, OptionListNumber) when is_list (OptionListNumber) ->
	[OptionNumber] = OptionListNumber,
	get_combination_support (CardID, OptionNumber).

get_option_support_seal ([{OptionNum, S1, S2, S3, S4, S5, A, B, C, D, E}]) ->
	get_option_support_seal ({OptionNum, S1, S2, S3, S4, S5, A, B, C, D, E});
get_option_support_seal ({OptionNum, S1, S2, S3, S4, S5, _, _, _, _, _}) ->
	get_sub ([S1, S2, S3, S4, S5], [], OptionNum).

get_option_support_growth ([{OptionNum, S1, S2, S3, S4, S5}]) ->
	get_option_support_growth ({OptionNum, S1, S2, S3, S4, S5});
get_option_support_growth ({OptionNum, S1, S2, S3, S4, S5}) ->
	get_sub ([S1, S2, S3, S4, S5], [], OptionNum).

get_sub ([], Lists, OptionNum) -> {OptionNum, Lists};
get_sub ([null|_], Lists, OptionNum) -> {OptionNum, Lists};
get_sub ([S|T], Lists, OptionNum) -> get_sub(T, append(Lists, [S]), OptionNum).

get_standard_power (CardID) ->
	{ok, Attack} = get_seal_data (CardID, attack),
	{ok, Defend} = get_seal_data (CardID, defend),
	{ok, Speed} = get_seal_data (CardID, speed),
	{ok, MAtk} = get_seal_data (CardID, mp_atk),
	[{at, Attack}, {df, Defend}, {sp, Speed}, {ma, MAtk}, {att, 0}].

% {power_change, [{at, 12}, {sp, 3}]}
get_power_change_data (CardID, CombineOption) ->
	case get_combination_data(CardID) of
		{ok, Result} ->
			case CombineOption of
				[Option] -> find_option_power (Result, Option);
				Option -> find_option_power (Result, Option)
			end;
		{error} ->
			io:format ("Get combination data from ~p error~n", [CardID]),
			{error, error}
	end.

find_option_power ([], _) ->
	{error, option_error};
find_option_power ([{CombineOption, _, _, _, _, _, Atk, Def, Spd, Mp, AtkType}|_], CombineOption) ->
	PowerOption = find_option ([{at, Atk}, {df, Def}, {sp, Spd}, {ma, Mp}, {att, AtkType}]),
	{ok, PowerOption};
find_option_power ([_|T], CombineOption) -> find_option_power (T, CombineOption).

find_option ([]) -> [];
find_option ([{_, 0} | PowerOption]) -> find_option (PowerOption);
find_option ([{_, null} | PowerOption]) -> find_option (PowerOption);
find_option ([Power | PowerOption]) ->
	[Power] ++ find_option (PowerOption).

size_combine_type (CardID, CombineType) ->
	{ok, Result} = get_combination_data (CardID),
	count_combine_type (Result, CombineType).

count_combine_type ([], _) -> {0, []};
count_combine_type ([{OptionNumber, {_, _}, null, null, null, null, _, _, _, _, _} | CombineOption], double_combine) ->
	{OptionSize, OptionList} = count_combine_type (CombineOption, double_combine),
	{OptionSize + 1, OptionList ++ [OptionNumber]};
count_combine_type ([{OptionNumber, {_, _}, {_, _}, null, null, null, _, _, _, _, _} | CombineOption], triple_combine) ->
	{OptionSize, OptionList} = count_combine_type (CombineOption, triple_combine),
	{OptionSize + 1, OptionList ++ [OptionNumber]};
count_combine_type ([_ | CombineOption], Any) ->
	{OptionSize, OptionList} = count_combine_type (CombineOption, Any),
	{OptionSize, OptionList}.

%count_combine_type ([], _) -> {0, []};
%count_combine_type ([{OptionNumber, [_, _], null, null, null, null, _, _, _, _, _} | CombineOption], double_combine) ->
%	{OptionSize, OptionList} = count_combine_type (CombineOption, double_combine),
%	{OptionSize + 1, OptionList ++ [OptionNumber]};
%count_combine_type ([{OptionNumber, [_, _], [_, _], null, null, null, _, _, _, _, _} | CombineOption], triple_combine) ->
%	{OptionSize, OptionList} = count_combine_type (CombineOption, triple_combine),
%	{OptionSize + 1, OptionList ++ [OptionNumber]};
%count_combine_type ([_ | CombineOption], Any) ->
%	{OptionSize, OptionList} = count_combine_type (CombineOption, Any),
%	{OptionSize, OptionList}.

get_combination_attack_type (CardID, CombineOrder) ->
	case get_combination_data(CardID) of
		{ok, Result} ->
			case get_attack_type_from_option (Result, CombineOrder) of
				{ok, TypeCode} ->
					TypeCode;
				{error, not_found} ->
					io:format ("Can not find Option ~p From ~p~n", [CombineOrder, CardID]),
					0
			end;
		{error} ->
			io:format("No combination state~n"),
			atk_normal
	end.

get_attack_type_from_option ([], _) ->
	{error, not_found};
get_attack_type_from_option ([{CombineOrder, _, _, _, _, _, _, _, _, _, AtkType} | _], CombineOrder) ->
	{ok, AtkType};
get_attack_type_from_option ([_ | Combine], CombineOrder) ->
	get_attack_type_from_option (Combine, CombineOrder).