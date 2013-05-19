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
-module (seal_card).

-import (mnesia_table, [do/1]).
-include_lib ("stdlib/include/qlc.hrl").
-include ("record.hrl").
-import (lists, [flatlength/1, append/2]).

-compile (export_all).

% seal_card_option = {card_status, active, line, growth, combine, mystic, give_effect, receive_effect, receive_skill_effect, steal_cards, option_plus}
% give_effect = {condition, player_effect, opponent_effect, self_effect, other_card_effect}
get_default_option() -> {[], active, 0, 0, [], [], [], [], [], [], [{card_zone, seal_deck}]}.

get_default_option(CardID) ->
	[{Name, Type, Elem, Naming, Mc, Ma, Level, At, Df, Sp}] = get_seal_default_information(CardID),
	Combination = get_combination_information(CardID),
	{[{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mp_atk, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}],
	Combination, [], active, 0, 0, [], [], [], [], [], [], [{card_zone, seal_deck}]}.
	
get_default_option_field(CardID, Field) ->
	[{Name, Type, Elem, Naming, Mc, Ma, Level, At, Df, Sp}] = get_seal_default_information(CardID),
	case Field of
		card_name -> {ok, Name};
		card_type -> {ok, Type};
		card_element -> {ok, Elem};
		card_naming -> {ok, Naming};
		mp_cast -> {ok, Mc};
		mp_atk -> {ok, Ma};
		level -> {ok, Level};
		attack -> {ok, At};
		defend -> {ok, Df};
		speed -> {ok, Sp};
		_ -> {error, Field}
	end.
	
get_seal_default_information(CardID) ->
	do(qlc:q([{
		X#seal_card.card_name,
		X#seal_card.card_type, 
		X#seal_card.card_element,
		X#seal_card.card_naming, 
		X#seal_card.mp_cast, 
		X#seal_card.mp_atk, 
		X#seal_card.level, 
		X#seal_card.attack, 
		X#seal_card.defend, 
		X#seal_card.speed}||X <- mnesia:table(seal_card), X#seal_card.card_id =:= CardID])).
	
get_combination_information(CardID) ->
	CardCombination = do(qlc:q([X#combination.option_list||X <- mnesia:table(combination), X#combination.card_id =:= CardID])),
	case CardCombination of
		[] -> [];
		[CardCom] -> CardCom
	end.

card_information(_, []) -> error;
card_information(Field, [{Field, Value}|_]) -> Value;
card_information(Field, [{_, _}|OtherField]) -> card_information(Field, OtherField).

add_information(InfoField, Information, OptionData) ->
	[{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}] = Information,
	case InfoField of
		card_name -> [{card_name, Name ++ OptionData}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_type -> [{card_name, Name}, {card_type, Type ++ OptionData}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_element -> [{card_name, Name}, {card_type, Type}, {card_element, Elem ++ OptionData}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_naming -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {naming, Naming ++ OptionData}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}]
		% mp_cast -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, OptionData}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		% mpattack -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, OptionData}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		% level -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, OptionData}, {attack, At}, {defend, Df}, {speed, Sp}];
		% at -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, OptionData}, {defend, Df}, {speed, Sp}];
		% df -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, OptionData}, {speed, Sp}];
		% sp -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, OptionData}]
	end.

set_information(InfoField, Information, OptionData) ->
	[{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}] = Information,
	case InfoField of
		card_name -> [{card_name, OptionData}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_type -> [{card_name, Name}, {type, OptionData}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_element -> [{card_name, Name}, {card_type, Type}, {card_element, OptionData}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		card_naming -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, OptionData}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		mp_cast -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, OptionData}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		mpattack -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, OptionData}, {level, Level}, {attack, At}, {defend, Df}, {speed, Sp}];
		level -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, OptionData}, {attack, At}, {defend, Df}, {speed, Sp}];
		at -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, OptionData}, {defend, Df}, {speed, Sp}];
		df -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, OptionData}, {speed, Sp}];
		sp -> [{card_name, Name}, {card_type, Type}, {card_element, Elem}, {card_naming, Naming}, {mp_cast, Mc}, {mpattack, Ma}, {level, Level}, {attack, At}, {defend, Df}, {speed, OptionData}]
	end.

create_option (Option) ->
	create_option(get_default_option (), Option).
	
set_create_option(CardID, Option) ->
	create_option(get_default_option(CardID), Option).
	
create_option (CardOption, []) -> CardOption;
create_option (CardOption, [{OptionField, OptionData} | T]) ->
	create_option (set_seal_option (CardOption, OptionField, OptionData), T).

% seal_card_detail = {card_status, active, line, growth, combine, mystic, give_effect, receive_effect}
% GetEffect = [{card_data, effect_type_list, duration}] --> [{{<0.85.0>, 2, 257}, [{curse, stone_curse}], 2}, {{<0.85.0>, 3, 287}, [{at, 1}], 2}]
% ถ้า Curse ที่มีผลตามมาจะมีรูปแบบดังนี้  -- [{{<0.85.0>, 2, 257}, [{curse, stone_curse}, {at, 2}], 2}]
% {curse, stone_curse}, {curse, last_dance_curse}
% {at, 2}, {at, -1}, {df, 1}
% {assign_line, disallow}, {assign_atk, disallow}
% CombinationState = {sub_seal_list, data_change_list} --> {[Card, Card], [{at, 12}, {sp, 3}]}
% mystic = {player_pid, card_order, card_id, effect} --> [{<0.195.0>, 27, 399, [{at, 2}, {df, -1}]}, {...}]
get_seal_option(CardOption, Field) ->
	{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, OtherOption} = CardOption,
	case Field of
		information -> {ok, Information};
		combination -> {ok, Combination};
		card_status -> {ok, StatusList};
		active -> {ok, Active};
		line -> {ok, Line};
		growth -> {ok, Growth};
		combine -> {ok, Combine};
		mystic -> {ok, Mystic};
		give_effect -> {ok, GiveEffect};
		receive_effect -> {ok, ReceiveEffect};
		skill_effect -> {ok, SkillEffect};
		steal -> {ok, Steal};
		_ -> get_other_option (Field, OtherOption)
	end.

get_other_option (_, []) -> {error, no_option_field};
get_other_option (GetOption, [{GetOption, Value}| _]) -> {ok, Value};
get_other_option (GetOption, [_| Option]) -> get_other_option (GetOption, Option).

% status_list = [casting, attacker, changing_line, assign_line, combination, ...]
set_seal_option (CardOption, OptionField, OptionData) ->
%	io:format("Set Field data is ~p~n", [OptionData]),
	{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option} = CardOption,
	case OptionField of
		card_status ->
			CardStatus = set_status_list (StatusList, OptionData),
			{Information, Combination, CardStatus, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		active ->
			{Information, Combination, StatusList, OptionData, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		line ->
			{Information, Combination, StatusList, Active, OptionData, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		growth ->
			{Information, Combination, StatusList, Active, Line, OptionData, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		combine ->
			{Information, Combination, StatusList, Active, Line, Growth, OptionData, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		mystic ->
			{Information, Combination, StatusList, Active, Line, Growth, Combine, OptionData ++ Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		give_effect ->
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, OptionData ++ GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		receive_effect ->
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, OptionData ++ ReceiveEffect, SkillEffect, Steal, Option};
		skill_effect ->
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, OptionData ++ SkillEffect, Steal, Option};
		steal ->
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, OptionData ++ Steal, Option};
		card_zone ->
			OptionUpdate = update_option_field (OptionField, OptionData, Option),
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, OptionUpdate};
		_ ->	OptionUpdate = add_seal_option_field (OptionField, OptionData, Option),
			io:format ("Add seal option plus ~p~n", [OptionField]),
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, OptionUpdate}
	end.

add_seal_option_field (OptionField, OptionData, []) -> [{OptionField, OptionData}];
add_seal_option_field (OptionField, OptionData, [{OptionField, DataList} | Option]) ->
	DataAdd = OptionData -- DataList,
	[{OptionField, DataList ++ DataAdd}] ++ Option;
add_seal_option_field (OptionField, OptionData, [Op | Option]) ->
	[Op] ++ add_seal_option_field (OptionField, OptionData, Option).

% ไว้สำหรับใส่ค่าต่างๆลงไปโดยไม่สนใจ ค่าเก่าที่มีอยู่
update_seal_option (CardOption, OptionField, OptionData) ->
%	io:format("Set Field data is ~p~n", [OptionData]),
	{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option} = CardOption,
	case OptionField of
		information -> {OptionData, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		{information, add, InfoField} ->
			InfoUpdate = add_information(InfoField, Information, OptionData),
			{InfoUpdate, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		{information, set, InfoField} ->
			InfoUpdate = set_information(InfoField, Information, OptionData),
			{InfoUpdate, Combination, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		combination -> {Information, OptionData, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		card_status -> {Information, Combination, OptionData, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		active -> {Information, Combination, StatusList, OptionData, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		line -> {Information, Combination, StatusList, Active, OptionData, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		growth -> {Information, Combination, StatusList, Active, Line, OptionData, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		combine -> {Information, Combination, StatusList, Active, Line, Growth, OptionData, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		mystic -> {Information, Combination, StatusList, Active, Line, Growth, Combine, OptionData, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option};
		give_effect -> {Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, OptionData, ReceiveEffect, SkillEffect, Steal, Option};
		receive_effect -> {Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, OptionData, SkillEffect, Steal, Option};
		skill_effect -> {Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, OptionData, Steal, Option};
		steal -> {Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, OptionData, Option};
		_ ->	OptionUpdate = update_option_field (OptionField, OptionData, Option),
%			io:format ("Update seal option plus ~p~n", [OptionField]),
			{Information, Combination, StatusList, Active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, OptionUpdate}
	end.

update_option_field (OptionField, OptionData, []) -> [{OptionField, OptionData}];
update_option_field (OptionField, OptionData, [{OptionField, _} | Option]) ->
	[{OptionField, OptionData}] ++ Option;
update_option_field (OptionField, OptionData, [Op | Option]) ->
	[Op] ++ update_option_field (OptionField, OptionData, Option).

set_status_list ([], OptionData) -> [OptionData];
set_status_list ([OptionData | StatusList], OptionData) -> [OptionData] ++ StatusList;
set_status_list ([Status | StatusList], OptionData) ->
	[Status] ++ set_status_list (StatusList, OptionData).

remove_seal_status (CardOption, Status) ->
	{ok, CardStatus} = get_seal_option (CardOption, card_status),
	CardStatusUpdate = remove_status (CardStatus, Status),
	update_seal_option (CardOption, card_status, CardStatusUpdate).

remove_status ([], _) -> [];
remove_status ([Status | CardStatus], Status) -> CardStatus;
remove_status ([Other | CardStatus], Status) ->
	[Other] ++ remove_status (CardStatus, Status).

% CombinationState = [support_seal_list, data_change_list] --> [{support_seal, [Card, Card]}, {power_change, [{at, 12}, {sp, 3}]}]
add_support_seal (CardOption, SupportCard) ->
	case get_seal_option (CardOption, combine) of
		{ok, CombineState} ->
			CombineUpdate = add_support_cards (CombineState, SupportCard),
			set_seal_option (CardOption, combine, CombineUpdate);
		{error, _} -> {error, no_option}
	end.

add_support_cards ([], SupportCard) -> [{support_seal, [SupportCard]}];
add_support_cards ([{support_seal, SupportList} | CombineState], SupportCard) ->
	[{support_seal, SupportList ++ [SupportCard]}] ++ CombineState;
add_support_cards ([Combine | CombineState], SupportCard) ->
	[Combine] ++ add_support_cards (CombineState, SupportCard).

remove_support_cards ([], _) -> [];
remove_support_cards ([{support_seal, SupportList} | CombineState], SupportCard) ->
	SupportUpdate = remove_support (SupportList, SupportCard),
	[{support_seal, SupportUpdate}] ++ CombineState;
remove_support_cards ([Combine | CombineState], SupportCard) ->
	[Combine] ++ remove_support_cards (CombineState, SupportCard).

remove_support (SupportList, []) -> SupportList;
remove_support (SupportList, [{CardOwner, CardOrder, CardID} | Cards]) ->
	SupportUpdate = remove_support (SupportList, CardOwner, CardOrder, CardID),
	remove_support (SupportUpdate, Cards).

remove_support ([], _, _, _) -> [];
remove_support ([{{CardOwner, CardOrder, CardID}, _} | Support], CardOwner, CardOrder, CardID) ->
	Support;
remove_support ([SupportCard | Support], CardOwner, CardOrder, CardID) ->
	[SupportCard] ++ remove_support (Support, CardOwner, CardOrder, CardID).

get_support_seal ([]) -> [];
get_support_seal ([{support_seal, SupportList} | _]) -> SupportList;
get_support_seal ([_ | T]) -> get_support_seal (T).

update_support_seal (CardOption, SupportCard, UpdateType) ->
	case get_seal_option (CardOption, combine) of
		{ok, CombineState} ->
			case UpdateType of
				add_support ->
					CombineUpdate = add_support_cards (CombineState, SupportCard);
				remove_support ->
					Update = remove_support_cards (CombineState, SupportCard),
					CombineUpdate = check_update_data (Update),
					io:format ("Combine update ~p~n", [CombineUpdate])
			end,
			set_seal_option (CardOption, combine, CombineUpdate);
		{error, _} -> {error, no_option}
	end.

check_update_data ([]) -> [];
check_update_data ([{support_seal, Support} | CombineState]) ->
	case Support of
		[] -> [];
		_ ->	[{support_seal, Support}] ++ CombineState
	end;
check_update_data ([Combine | CombineState]) -> check_update_data (CombineState ++ Combine).

get_power_change ([]) -> {not_change};
get_power_change ([{power_change, ChangeList} | _]) -> {ok, ChangeList};
get_power_change ([_ | T]) -> get_power_change (T).

change_combine_power (CardOption, PowerChange) ->
	case get_seal_option (CardOption, combine) of
		{ok, CombineState} ->
			CombineUpdate = set_power_combine (CombineState, PowerChange, []),
			set_seal_option (CardOption, combine, CombineUpdate);
		{error, _} -> {error, no_option}
	end.

set_power_combine ([], PowerChange, CombineState) ->
	CombineState ++ [{power_change, PowerChange}];
set_power_combine ([{power_change, _} | T], PowerChange, CombineState) ->
	CombineState ++ [{power_change, PowerChange}] ++ T;
set_power_combine ([H | T], PowerChange, CombineState) ->
	set_power_combine(T, PowerChange, CombineState ++ [H]).

set_combine_option (CardOption, OptionNumber) ->
	Combine = get_seal_option (CardOption, combine),
%	io:format("Combine ~p~n", [Combine]),
	case Combine of
		{ok, CombineState} ->
			CombineUpdate = set_option_combine (CombineState, OptionNumber, []),
%			io:format("Combine update ~p~n", [CombineUpdate]),
			set_seal_option (CardOption, combine, CombineUpdate);
		{error, _} -> {error, no_option}
	end.

set_option_combine ([], OptionNumber, CombineState) ->
	CombineState ++ [{option_number, OptionNumber}];
set_option_combine ([{option_number, _} | T], OptionNumber, CombineState) ->
	CombineState ++ [{option_number, OptionNumber}] ++ T;
set_option_combine ([H | T], OptionNumber, CombineState) ->
	set_option_combine(T, OptionNumber, CombineState ++ [H]).

get_combine_option ([]) -> {error, on_combine};
get_combine_option ([{option_number, OptionNumber} | _]) -> {ok, OptionNumber};
get_combine_option ([_ | T]) -> get_combine_option (T).

get_combine_field (Combine, Field) ->
	case Field of
		power_change -> get_power_change (Combine);
		support_seal -> get_support_seal (Combine);
		option_number -> get_combine_option (Combine)
	end.

pop_combine_status (CardOption) ->
	case get_seal_option (CardOption, combine) of
		{ok, CombineState} ->
			%SupportSeal = get_support_seal(CombineState),
			OptionUpdate = set_seal_option (CardOption, combine, []),
			%{ok, OptionUpdate, SupportSeal};
			{ok, OptionUpdate};
		{error, _} -> {error, no_option}
	end.

check_card_status (CardOption, StatusCheck) ->
	case get_seal_option(CardOption, card_status) of
		{ok, CardStatus} ->
			check_status (CardStatus, StatusCheck);
		{error, _} ->
			{error, no_option}
	end.

check_status ([], _) -> {ok, have_no_status};
check_status ([StatusCheck|_], StatusCheck) -> {ok, have_status};
check_status ([_|T], StatusCheck) -> check_status (T, StatusCheck).

is_on_line (CardOption, Line) ->
	case get_seal_option(CardOption, line) of
		{ok, Line} -> on_line;
		{ok, _} -> off_line
	end.

%% ส่วนของการ Combination
get_status_change ([{Status, Value}|_], Status) -> {found, Value};
get_status_change ([_|T], Status) -> get_status_change (T, Status);
get_status_change ([], _) -> {not_found}.

get_atk_cost (CardID, CardOption) ->
	{ok, ComStatus} = get_seal_option (CardOption, combine),
	case get_power_change (ComStatus) of
		{not_change} ->
			mnesia_odbc:get_seal_data (CardID, mp_atk);
		{ok, ChangeList} ->
			case get_status_change (ChangeList, mp) of
				{found, MpCost} -> {ok, MpCost};
				{not_found} -> mnesia_odbc:get_seal_data (CardID, mp_atk)
			end
	end.
% หาค่าพลังพื้นฐานของการ์ด
get_seal_base_power(CardOwner, CardOrder, CardID, PowerType) ->
	% การ์ด นั้นอยู่ในท่ารวมร่างใดๆหรือไม่
	case card_utility:get_card_combine_part(CardOwner, CardOrder, CardID, option_number) of
		% อยู่ในท่ารวมร่างใดๆ
		{ok, OptionNumber} ->
			% หาค่าพลังพื้นฐาน ของท่ารวมร่างนั้น
			get_combine_power_type(CardOwner, CardOrder, CardID, OptionNumber, PowerType);
			% ไม่อยู่ในท่ารวมร่างใดๆ หาค่าพลังจาก พลังปกติ
		_ ->
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = get_seal_option(CardOption, information),
			get_power_type(PowerType, CardInfo)
	end.
	
% get_seal_base_power(CardOwner, CardOrder, CardID, PowerType, Combine) ->
	% {ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
	% {ok, CardCombine} = get_seal_option(CardOption, combination),
	% get_power_type(PowerType, CardCombine).

get_power_type(_, []) -> {error, 0};	
get_power_type(PowerType, [{PowerType, Power}|_]) -> {ok, Power};
get_power_type(PowerType, [_|CardInfo]) -> get_power_type(PowerType, CardInfo).

% หาค่าพลัง ตามท่ารวมร่างนั้น
get_combine_power_type(CardOwner, CardOrder, CardID, CombineOption, PowerType) ->
						% {ok, ComPowerData} = get_combine_power(CardOwner, CardOrder, CardID, CombineOption),
						% case get_power_type(PowerType, ComPowerData) of
							% {error, 0} ->
								% {ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
								% {ok, CardInfo} = get_seal_option(CardOption, information),
								% get_power_type(PowerType, CardInfo);
							% {ok, Power} -> {ok, Power}
						% end.
	% 
	case get_combine_power(CardOwner, CardOrder, CardID, CombineOption) of
		% ComPowerData = [{attack, At}, {defend, Df}, {speed, Sp}, {mp_atk, Ma}, {attack_time, AtkTime}]
		% โดย At, Df, Sp, Ma, AtkTime ต่้องไม่เป็น 0 หรือ null ถ้าใช่ จะไม่ปรากฏ เช่น เมื่อ Df เป็น 0 จะได้ [{attack, At}, {speed, Sp}, {mp_atk, Ma}, {attack_time, AtkTime}]
		{ok, ComPowerData} ->
			case get_power_type(PowerType, ComPowerData) of
				% คือ PowerType ที่ต้องการไม่ต่างจาก ค่าพลังปกติ ดังนั้น จึงไปหาเอาจากค่าพลังปกติ
				{error, 0} ->
					{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
					{ok, CardInfo} = get_seal_option(CardOption, information),
					get_power_type(PowerType, CardInfo);
				{ok, Power} -> {ok, Power}
			end;
		% กรณีหา ค่าพลังที่เปลี่ยนแปลงจากท่ารวมร่าง (CombineOption) ที่ส่งมาไม่เจอ ซึ่งไม่น่า เกิดได้ .....แต่เกิด ต้องหาสามเหตุและแก้ไข
		{error, option_error} ->
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = get_seal_option(CardOption, information),
			get_power_type(PowerType, CardInfo)
	end.
	
% การสลับค่าพลัง จาก PowerType1 กับ PowerType2 (เฉพาะค่าพลังพื้นฐาน)
swap_card_power(CardOwner, CardOrder, CardID, {PowerType1, PowerType2}) ->
	{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
	{ok, CardInfo} = get_seal_option(CardOption, information),
	{ok, Power1} = get_power_type(PowerType1, CardInfo),
	{ok, Power2} = get_power_type(PowerType2, CardInfo),
	UpdateInfo = ((CardInfo -- [{PowerType1, Power1}]) -- [{PowerType2, Power2}]) ++ [{PowerType1, Power2}] ++ [{PowerType2, Power1}],
	OptionUpdate1 = update_seal_option(CardOption, information, UpdateInfo),
	{ok, CardCombination} = get_seal_option(CardOption, combination),
	UpdateCom = card_combination_info(CardCombination, PowerType1, PowerType2, []),
	OptionUpdate = update_seal_option(OptionUpdate1, combination, UpdateCom),
	card_utility:update_card(CardOwner, CardOrder, CardID, OptionUpdate, arena_zone),
	% Check ว่าการ์ดที่สลับค่าพลังนั้น กำลังรวมร่างอยู่ในท่าใดหรือไม่
	case card_utility:get_card_combine_part(CardOwner, CardOrder, CardID, option_number) of
		% ถ้ารวมก็สลับ เปลี่ยนแปลงค่าพลัง เพื่อเตรียม update ให้ Client
		{ok, OptionNumber} ->
			{ok, PowerChange} =  get_combine_power(CardOwner, CardOrder, CardID, OptionNumber),
			arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange);
		{error, _} -> do_noting
	end.
	
% เปลี่ยนแปลงธาต ุ
change_card_element(CardOwner, CardOrder, CardID, ElementCode) ->
	{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
	{ok, CardInfo} = get_seal_option(CardOption, information),
	{ok, [CardElement|ShadeOf]} = get_power_type(card_element, CardInfo),
	UpdateInfo = (CardInfo -- [{card_element, [CardElement|ShadeOf]}]) ++ [{card_element, ElementCode ++ ShadeOf}],
	OptionUpdate = update_seal_option(CardOption, information, UpdateInfo),
	card_utility:update_card(CardOwner, CardOrder, CardID, OptionUpdate, arena_zone).
	
rechange_element(CardOwner, CardOrder, CardID, {GOwner, GOrder, GID, AbilityID}) ->
	{ElementCode, RemainFx} =  previous_element(CardOwner, CardOrder, CardID, {GOwner, GOrder, GID, AbilityID}),
	change_card_element(CardOwner, CardOrder, CardID, ElementCode),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx).
	
previous_element(CardOwner, CardOrder, CardID, {GOwner, GOrder, GID, AbilityID}) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	case function_utility:check_contain({GOwner, GOrder, GID, AbilityID}, CardFx) of
		{value, {GiveFx, Fx, Duration}} ->
			RemainFx = CardFx -- [{GiveFx, Fx, Duration}],
			Element = check_previous_element(CardOwner, CardOrder, CardID, RemainFx),
			{Element, RemainFx};
		_ -> 
			Element = check_previous_element(CardOwner, CardOrder, CardID, CardFx),
			{Element, CardFx}
	end.
	
check_previous_element(CardOwner, CardOrder, CardID, CardFx) ->
	FxReverse = lists:reverse(CardFx),
	get_previous_element(CardOwner, CardOrder, CardID, FxReverse).
	
get_previous_element(_CardOwner, _CardOrder, CardID, []) ->
	{ok, Element} = get_default_option_field(CardID, card_element),
	Element;
get_previous_element(_CardOwner, _CardOrder, _CardID, [{_GFx, [{elem, Element}], _Duration}|_Fx]) -> Element;
get_previous_element(CardOwner, CardOrder, CardID, [_|Fx]) -> get_previous_element(CardOwner, CardOrder, CardID, Fx).
	
card_combination_info([], _, _, Result) -> Result;
card_combination_info([{ComNo,A,B,C,D,Level,At,Df,Sp,Ma,AtkTime}|Combination], PowerType1, PowerType2, Result) ->
	 case PowerType1 of
	 	attack -> 
			case PowerType2 of
				defend -> card_combination_info(Combination, PowerType1, PowerType2, Result++[{ComNo,A,B,C,D,Level,Df,At,Sp,Ma,AtkTime}])
			end
	end.
	
get_combine_power(CardOwner, CardOrder, CardID, CombineOption) ->
	{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
	{ok, CardCombine} = get_seal_option(CardOption, combination),
	get_card_combine_data(CardCombine, CombineOption).
	
get_card_combine_data([], CombineOption) -> io:format("cannot find this combine option ~p~n", [CombineOption]), {error, option_error};
get_card_combine_data([{CombineOption,_,_,_,_,_,At,Df,Sp,Ma,AtkTime}|_], CombineOption) ->
	PowerOption = find_option([{attack, At}, {defend, Df}, {speed, Sp}, {mp_atk, Ma}, {attack_time, AtkTime}]),
	{ok, PowerOption};
get_card_combine_data([{CombineOption,_,_,_,_,_,At,Df,Sp,Ma,AtkTime}|_], [CombineOption]) ->
	PowerOption = find_option([{attack, At}, {defend, Df}, {speed, Sp}, {mp_atk, Ma}, {attack_time, AtkTime}]),
	{ok, PowerOption};
get_card_combine_data([{CombineOption,_,_,_,_,_,At,Df,Sp,Ma,AtkTime}|_], [[CombineOption]]) ->
	PowerOption = find_option([{attack, At}, {defend, Df}, {speed, Sp}, {mp_atk, Ma}, {attack_time, AtkTime}]),
	{ok, PowerOption};
get_card_combine_data([_|Combine], CombineOption) -> get_card_combine_data(Combine, CombineOption).

find_option([]) -> [];
find_option([{_, 0} | PowerOption]) -> find_option(PowerOption);
find_option([{_, null} | PowerOption]) -> find_option(PowerOption);
find_option([Power | PowerOption]) ->
	[Power] ++ find_option (PowerOption).
	
get_standard_power(CardOwner, CardOrder, CardID) ->
	{ok, Attack} = get_seal_base_power(CardOwner, CardOrder, CardID, attack),
	{ok, Defend} = get_seal_base_power(CardOwner, CardOrder, CardID, defend),
	{ok, Speed} = get_seal_base_power(CardOwner, CardOrder, CardID, speed),
	{ok, MAtk} = get_seal_base_power(CardOwner, CardOrder, CardID, mp_atk),
	[{at, Attack}, {df, Defend}, {sp, Speed}, {ma, MAtk}, {att, 0}].
	
