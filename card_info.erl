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
-module (card_info).

-include ("play_record.hrl").
-include_lib ("stdlib/include/qlc.hrl").

-import (mnesia_table, [do/1]).

-export ([check_update_card_info_data/3, remove_card_info/1]).

q_transaction (Row) ->
	F = fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).

remove_card_info (PlayerPid) ->
	remove_cards_info_player (PlayerPid, 1).

remove_cards_info_player (_, 51) -> remove_all_card_info;
remove_cards_info_player (PlayerPid, CardOrder) ->
	Oid = {card_infomation, {PlayerPid, CardOrder}},
	F = fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F),
	remove_cards_info_player (PlayerPid, CardOrder + 1).

% CardInfo = {card_data, atk, def, spd, level, atk_cost, cast_cost, skill_cost}
set_card_info(PlayerPid, CardOrder, _, Atk, Def, Spd, Level, AtkCost, CastCost, SkillCost, Charge) ->
	CardPower = [{attack, Atk}, {defend, Def}, {speed, Spd}, {level, Level}, {mp_atk, AtkCost}, {mp_cast, CastCost}, {mp_skill, SkillCost}, {charge, Charge}],
	Row = #card_infomation {card_data	= {PlayerPid, CardOrder},
						card_power = CardPower},
	q_transaction(Row).

get_card_info (PlayerPid, CardOrder) ->
	Query = qlc:q([X#card_infomation.card_power ||
				X <- mnesia:table(card_infomation), X#card_infomation.card_data =:= {PlayerPid, CardOrder}]),
	case do(Query) of
		[] -> [];
		[Result] -> Result
	end.

check_update_card_info_data(PlayerPid, CardOrder, CardID) ->
	{Atk, Def, Spd, Level, MAtk, MCast, MSkill, Charge} = arena_zone:get_all_card_power_infomation(PlayerPid, CardOrder, CardID),
	%io:format("{attack, ~p}~n, {defend, ~p}~n, {speed, ~p}~n, {level, ~p}~n, {mp_atk, ~p}~n, {mp_cast, ~p}~n, {mp_skill, ~p}~n, {charge, ~p}~n", [Atk, Def, Spd, Level, MAtk, MCast, MSkill, Charge]),
%	[{atk,8},{def,7},{spd,2},{lvl,2},{ma,1},{mc,3},{ms,0}, {charge, 1}] = get_card_info (PlayerPid, CardOrder),
	Result = get_card_info(PlayerPid, CardOrder),
	%io:format ("Result card power ~p~n", [Result]),
	case Result of
		% ถ้าการ์ดนั้น ไม่ได้มีข้อมูลใน Mnesia หมายความว่าการ์ดนั้นยังไม่มี ข้อมูลมาก่อน จึง จึง ยังไม่เกิดการเปลี่ยนแปลงใดๆขึ้น
		[] ->   {ok, CardOption} = card_utility:get_card_option(PlayerPid, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			[_,_,_,_,{mp_cast, BaseMCast},{mp_atk, BaseMAtk},{level, BaseLevel},{attack, BaseAtk},{defend, BaseDef},{speed, BaseSpd}] = CardInfo,
			PowerChange = get_power_change([{attack, BaseAtk}, {defend, BaseDef}, {speed, BaseSpd}, {level, BaseLevel}, {mp_atk, BaseMAtk}, {mp_cast, BaseMCast}, {mp_skill, 0}, {charge, 0}], [{attack, Atk}, {defend, Def}, {speed, Spd}, {level, Level}, {mp_atk, MAtk}, {mp_cast, MCast}, {mp_skill, MSkill}, {charge, Charge}]);
		_ ->	PowerChange = get_power_change(Result, [{attack, Atk}, {defend, Def}, {speed, Spd}, {level, Level}, {mp_atk, MAtk}, {mp_cast, MCast}, {mp_skill, MSkill}, {charge, Charge}])
	end,
	set_card_info (PlayerPid, CardOrder, CardID, Atk, Def, Spd, Level, MAtk, MCast, MSkill, Charge),
	PowerChange.

get_power_change (_, []) -> [];
get_power_change (Result, [ChkType | ChkList]) ->
%	io:format ("get power change ~p~n", [ChkType]),
	check_power_change (Result, ChkType) ++ get_power_change (Result, ChkList).

check_power_change ([], {PowerType, PowerValue}) ->
	get_power_reply_data (PowerType, PowerValue);
check_power_change ([{PowerType, Value} | _], {PowerType, PowerValue}) ->
%	io:format ("check power change ~p ~p~n", [PowerType, Value]),
	case Value of
		PowerValue -> [];
		_ -> get_power_reply_data (PowerType, PowerValue)
	end;
check_power_change ([_ | Result], {PowerType, PowerValue}) ->
	check_power_change (Result, {PowerType, PowerValue}).

get_power_reply_data (PowerType, PowerValue) ->
	case PowerType of
		attack -> [{2, PowerValue}];
		defend -> [{3, PowerValue}];
		speed -> [{4, PowerValue}];
		level -> [{99, PowerValue}];
		mp_atk -> [{6, PowerValue}];
		mp_cast -> [{5, PowerValue}];
		mp_skill -> [{7, 10 + PowerValue}];
		charge ->
%			io:format ("have charge ~p~n", [PowerValue]),
			case PowerValue of
				0 ->	[{10, 8, PowerValue}];
				_ ->	[{1, 8, PowerValue}]
			end
	end.