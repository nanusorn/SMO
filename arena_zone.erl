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
-module (arena_zone).

-import(lists, [append/2, flatlength/1]).

-compile (export_all).

get_card (PlayerPid, CardOrder, CardID) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	find_card (Arena, CardOrder, CardID).

get_card_option (PlayerPid, CardOrder, CardID) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	find_card_option (Arena, CardOrder, CardID).

add_card (PlayerPid, CardOrder, CardID, CardOption) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	ArenaUpdate = append(Arena, [{{PlayerPid, CardOrder, CardID}, CardOption}]),
	mnesia_play:set_player_data (PlayerPid, arena_zone, ArenaUpdate).

get_power_change_field ([], _) -> 0;
get_power_change_field ([{Field, Value} | _], Field) -> Value; 
get_power_change_field ([_ | T], Field) -> get_power_change_field (T, Field).

summary_effect_counter (PlayerPid, CardOrder, CardID, FxCheckSum) ->
	CardFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
	Summary = count_effect_counter(PlayerPid, CardOrder, CardID, CardFx, FxCheckSum),
	{ok, Summary}.

count_effect_counter (_, _, _, [], _) -> 0;
count_effect_counter (PlayerPid, CardOrder, CardID, [{GFx, Fx, _} | CardFx], FxCheckSum) ->
	counting_effect (PlayerPid, CardOrder, CardID, {GFx, Fx}, FxCheckSum) + count_effect_counter (PlayerPid, CardOrder, CardID, CardFx, FxCheckSum).

counting_effect(_, _, _, {_, []}, _) -> 0;
counting_effect(PlayerPid, CardOrder, CardID, {GFx, [{FxCheckSum, Value} | Fx]}, FxCheckSum) ->
	%Count = ability_activate:check_value_data (PlayerPid, CardOrder, CardID, Value),
	Count =
	case is_integer(Value) of
		true -> Value;
		_Error ->	
			io:format("why don' t effect value is no integer ~p~n", [Value]),
			{GOwner, GOrder, GID, _} =GFx,
			effect_value:check_value(GOwner, GOrder, GID, Value, {PlayerPid, CardOrder, CardID})
	end,
	Count + counting_effect(PlayerPid, CardOrder, CardID, {GFx, Fx}, FxCheckSum);
counting_effect(PlayerPid, CardOrder, CardID, {GFx, [_ | Fx]}, FxCheckSum) ->
	counting_effect(PlayerPid, CardOrder, CardID, {GFx, Fx}, FxCheckSum).

get_all_card_power_infomation (PlayerPid, CardOrder, CardID) ->
	{ok, Attack} = get_card_power(PlayerPid, CardOrder, CardID, attack),
	{ok, Defend} = get_card_power(PlayerPid, CardOrder, CardID, defend),
	{ok, Speed} = get_card_power(PlayerPid, CardOrder, CardID, speed),
	{ok, MpCast} = get_card_power(PlayerPid, CardOrder, CardID, mp_cast),
	{ok, MpAtk} = get_card_power(PlayerPid, CardOrder, CardID, mp_atk),
	{ok, MpSkill} = get_card_power(PlayerPid, CardOrder, CardID, mp_skill),
	{ok, Level} = get_card_power(PlayerPid, CardOrder, CardID, level),
	{ok, ChargeCounter} = get_card_power(PlayerPid, CardOrder, CardID, charge_counter),
	{Attack, Defend, Speed, Level, MpAtk, MpCast, MpSkill, ChargeCounter}.

get_card_power(PlayerPid, CardOrder, CardID, PowerType) ->
%	io:format("Get card power Type ~p~n", [PowerType]),
	% case card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, combine, arena_zone) of
		% {ok, Combine} ->
			% get_card_power(PlayerPid, CardOrder, CardID, PowerType, Combine);
		% {error, _} ->
			% io:format ("Get card power ~p not on arena zone~n", [{PlayerPid, CardOrder, CardID}]),
			% get_card_power(PlayerPid, CardOrder, CardID, PowerType, [])
	% end.
	MainPower =
	case seal_card:get_seal_base_power(PlayerPid, CardOrder, CardID, PowerType) of
		{ok, Power} -> Power;
		{error, _} -> 0
	end,
	%io:format("main power ~p~n", [MainPower]),
	case PowerType of
		attack -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, at);
		defend -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, df);
		speed -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, sp);
		mp_cast -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, mc);
		mp_atk -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, ma);
		mp_skill -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, ms);
		level -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, lv);
		charge_counter -> summary_effect_counter (PlayerPid, CardOrder, CardID, counter);
		skill_use -> summary_effect_counter (PlayerPid, CardOrder, CardID, skill_use)
	end.

% get_card_power (PlayerPid, CardOrder, CardID, PowerType, Combine) ->
	% %case mnesia_odbc:get_seal_data (CardID, PowerType) of
	% MainPower = 
	% case seal_card:get_seal_base_power(PlayerPid, CardOrder, CardID, PowerType) of
		% {ok, Power} -> Power;
		% {error, _} -> 0
	% end,
	% io:format("main power ~p~n", [MainPower]),
	% case PowerType of
		% attack -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, at);
		% defend -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, df);
		% speed -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, sp);
		% mp_cast -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, mc);
		% mp_atk -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, ma);
		% mp_skill -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, ms);
		% level -> calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, lv);
		% charge_counter -> summary_effect_counter (PlayerPid, CardOrder, CardID, counter);
		% skill_use -> summary_effect_counter (PlayerPid, CardOrder, CardID, skill_use)
	% end.

%calculate_power (PlayerPid, CardOrder, CardID, MainPower, Combine, PowerType) ->
%	io:format("MainPower ~p [arena_zone.erl]~n", [MainPower]),
	% case seal_card:get_power_change (Combine) of
		% {ok, PowerChange} ->
			% case get_power_change_field (PowerChange, PowerType) of
				% 0 -> summary_effect_power (PlayerPid, CardOrder, CardID, PowerType, MainPower);
				% Change -> summary_effect_power (PlayerPid, CardOrder, CardID, PowerType, Change)
			% end;
		% {not_change} ->
			% summary_effect_power (PlayerPid, CardOrder, CardID, PowerType, MainPower)
	% end.
calculate_power(PlayerPid, CardOrder, CardID, MainPower, PowerType) ->
	summary_effect_power(PlayerPid, CardOrder, CardID, PowerType, MainPower).

summary_effect_power(PlayerPid, CardOrder, CardID, PowerField1, MainPower) ->
	CardFx = card_utility:get_all_card_effect (PlayerPid, CardOrder, CardID),
	%io:format("get all card effect ~p~n", [CardFx]),
	PowerField =
	case PowerField1 of
		at ->
			WhatContain = function_utility:what_contain([{swap_power, {attack, defend}}], CardFx),
			%io:format("swap power contain ~p~n", [WhatContain]),
			case WhatContain of
				[] -> at;
				Contain -> 
					case function_utility:is_odd(length(Contain)) of
						true -> df;
						_ -> at
					end
			end;
		df ->
			WhatContain = function_utility:what_contain([{swap_power, {attack, defend}}], CardFx),
			%io:format("swap power contain ~p~n", [WhatContain]),
			case WhatContain of
				[] -> df;
				Contain -> 
					case function_utility:is_odd(length(Contain)) of
						true -> at;
						_ -> df
					end
			end;
		_ -> PowerField1
	end,
	{MainFx, ChangeFx} = get_card_effect_type(PlayerPid, CardOrder, CardID, CardFx, PowerField),
	PowerChange = summary_power_change (PlayerPid, CardOrder, CardID, ChangeFx),
	%erase(power_round),
%	io:format ("MainPower ~p MainFx ~p PowerChange ~p~n", [MainPower, MainFx, PowerChange]),
	case MainFx of
		[] ->	NetPower = MainPower + PowerChange;			
		_ ->	NetPower = MainFx + PowerChange
	end,
	case PowerField of
			ms -> {ok, NetPower};
			_ ->
				if
					NetPower < 0 -> {ok, 0};
					true -> {ok, NetPower}
				end
	end.

get_card_effect_type(PlayerPid, CardOrder, CardID, CardFx, PowerField) -> get_card_effect_type(PlayerPid, CardOrder, CardID, CardFx, PowerField, {[], []}).

get_card_effect_type(_, _, _, [], _, Result) -> Result;
get_card_effect_type(PlayerPid, CardOrder, CardID, [{_, Fx, _} | CardFx], PowerField, {Main, Change}) ->
	%{MainFx, ChangeFx} = get_card_effect_type (PlayerPid, CardOrder, CardID, CardFx, PowerField),
	Value =
	case get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField) of
		{[], ReChange} -> {Main, Change ++ ReChange};
		{MainV, ReChange} -> {MainV, Change ++ ReChange}
	end,
	get_card_effect_type(PlayerPid, CardOrder, CardID, CardFx, PowerField, Value).

% หาเอฟเฟคโดยแยกเป็นสองประเภท 1. กำหนดค่าพลัง 2. เปลี่ยนแปลงค่าพลัง
get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField) -> get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField, {[], []}).

get_effect_type(_, _, _, [], _, Result) -> Result;
get_effect_type(PlayerPid, CardOrder, CardID, [{PowerField, Value} | Fx], PowerField, {Main, Change}) ->
	%{MainV, ChangeV} = get_effect_type (PlayerPid, CardOrder, CardID, Fx, PowerField),
	ReValue =
	case Value of
		{equal, Power} -> {Power, Change};
		_ ->	{Main, [Value] ++ Change}
	end,
	get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField, ReValue);
	
get_effect_type(PlayerPid, CardOrder, CardID, [{curse, {last_dance_curse, {PowerField, Value}}} | Fx], PowerField, {Main, Change}) ->
	%{MainV, ChangeV} = get_effect_type (PlayerPid, CardOrder, CardID, Fx, PowerField),
	Revalue =
	case Value of
		{equal, Power} -> {Power, Change};
		{target, Power} ->
			case Power of
				level -> Level =game_info:card_level({xxx, {PlayerPid, CardOrder, CardID}}), {Main, Change ++ [Level]};
				_ -> {Main, Change}
			end;
		_ ->	{Main, Change ++ [Value]}
	end,
	get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField, Revalue);
get_effect_type(PlayerPid, CardOrder, CardID, [_ | Fx], PowerField, Revalue) -> get_effect_type(PlayerPid, CardOrder, CardID, Fx, PowerField, Revalue).

% เอาเอฟเฟคที่เปลี่ยนแปลงค่าพลังทั้งหมดมาบวกรวมกัน
summary_power_change (_, _, _, []) -> 0;
summary_power_change (PlayerPid, CardOrder, CardID, [Value | ChangeFx]) ->
	%Change = ability_activate:check_value_data (PlayerPid, CardOrder, CardID, Value),
	Change =
	case is_integer(Value) of
		true -> Value;
		_Error -> 
			io:format("why don' t effect value is no integer ~p~n then Value Change is 0", [Value]),
			%[{_, ReValue}] = effect_value:check_value(PlayerPid, CardOrder, CardID, {ignore, Value}),
			0
	end,
	Change + summary_power_change (PlayerPid, CardOrder, CardID, ChangeFx).

update ([], _, _, _, ArenaUpdate) -> ArenaUpdate;
update ([{{PlayerPid, CardOrder, CardID}, _} | T], CardOrder, CardID, CardOption, ArenaUpdate) -> 
	append (ArenaUpdate, append ([{{PlayerPid, CardOrder, CardID}, CardOption}], T) );
update ([H|T], CardOrder, CardID, CardOption, ArenaUpdate) -> update (T, CardOrder, CardID, CardOption, append(ArenaUpdate, [H]) ).

update_card (PlayerPid, CardOrder, CardID, CardOption) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	ArenaUpdate = update(Arena, CardOrder, CardID, CardOption, []),
	mnesia_play:set_player_data (PlayerPid, arena_zone, ArenaUpdate).

seal_to_arena(PlayerPid, CardOrder, CardID, Option, FromZone) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{ok, CardStatus} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, card_status, FromZone),
			{ok, RFx} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, FromZone),
			%{ok, SFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, FromZone),
			% case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect_duration, FromZone) of
				% {ok, Duration} -> DurationFx = Duration;
				% _ -> DurationFx = []
			% end,
			% case card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, card_give_ability_effect, FromZone) of
				% {ok, CardGiFX} -> GFX = CardGiFX;
				% _ -> GFX = []
			% end,
			card_utility:remove_card(PlayerPid, CardOrder, CardID, FromZone),
			OptionDefault = 
			case FromZone of
				hand_cards -> [{card_status, assign_line}, {card_status, supported}, {line, 1}, {growth, 0}];
				_ -> [{card_status, assign_line}, {line, 1}, {growth, 0}]
			end,
			CardOption = seal_card:set_create_option(CardID, Option ++ OptionDefault ++ transform(CardStatus)),
			add_card(PlayerPid, CardOrder, CardID, CardOption),
			card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, RFx, arena_zone);
			%card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, SFx, arena_zone),
			%io:format("I ~p~n", [card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect_duration, arena_zone)]),
			%card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, give_effect_duration, DurationFx, arena_zone),
			%io:format("J ~p~n", [card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect_duration, arena_zone)]),
			%card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, card_give_ability_effect, GFX, arena_zone);
		is_not_seal ->
			mystic_to_arena(PlayerPid, CardOrder, CardID, Option, FromZone)
	end.

transform([]) -> [];
transform([Status | T]) -> [{card_status, Status}] ++ transform(T).

mystic_to_arena(PlayerPid, CardOrder, CardID, Option, FromZone) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			seal_to_arena(PlayerPid, CardOrder, CardID, Option, FromZone);
		is_not_seal ->
			{ok, CardStatus} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, card_status, FromZone),
			{ok, RFx} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, FromZone),
			%{ok, SFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, FromZone),
			card_utility:remove_card(PlayerPid, CardOrder, CardID, FromZone),
			{ok, Duration} = mnesia_odbc:get_mystic_data(CardID, duration),
			CardOption = mystic_card:set_create_option(CardID, Option ++ [{duration, Duration}] ++ transform(CardStatus)),
			add_card(PlayerPid, CardOrder, CardID, CardOption),
			RFxUpdate = remove_effect_not_use(RFx),
			%SFxUpdate = remove_effect_not_use (SFx),
			card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, RFxUpdate, arena_zone)
			%card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, skill_effect, SFxUpdate, arena_zone)
	end.

remove_effect_not_use ([]) -> [];
remove_effect_not_use ([{GFx, Fx, Duration} | CardFx]) ->
	case Duration of
		end_of_subturn_if_not_use ->
			remove_effect_not_use (CardFx);
		_ ->	[{GFx, Fx, Duration}] ++ remove_effect_not_use (CardFx)
	end.	

find_card ([], _, _) -> {error, no_card_on_arena};
find_card ([{{PlayerPid, CardOrder, CardID}, CardOption}|_], CardOrder, CardID) -> {ok, {{PlayerPid, CardOrder, CardID}, CardOption}};
find_card ([_|T], CardOrder, CardID) -> find_card (T, CardOrder, CardID).

find_card_option ([], _, _) -> {error, no_card_on_arena};
find_card_option ([{{_, CardOrder, CardID}, CardOption}|_], CardOrder, CardID) -> {ok, CardOption};
find_card_option ([_|T], CardOrder, CardID) -> find_card_option (T, CardOrder, CardID).

get_combine_option (PlayerPid, CardOrder, CardID) ->
	case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone) of
		{ok, Combine} -> seal_card:get_combine_option (Combine);
		{error, Reason} -> io:format ("Get card option error from ~p~n", [Reason])
	end.

% get_attack_type (PlayerPid, CardOrder, CardID) ->
	% case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone) of
		% {ok, Combine} ->
			% case seal_card:get_power_change(Combine) of
				% {ok, ChangeList} -> check_attack_type (ChangeList);
				% {not_change} -> {ok, 0}
			% end;
		% {error, Reason} ->
			% io:format ("Get card option error from ~p~n", [Reason])
	% end.

check_attack_type ([]) -> {ok, 0};
check_attack_type ([{att, AttackType} | _]) -> {ok, AttackType};
check_attack_type ([_ | T]) -> check_attack_type (T).

get_size (PlayerPid) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	flatlength(Arena).

set_all_seal_active (PlayerPid) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	UpdateArena = set_active (Arena, []),
	mnesia_play:set_player_data (PlayerPid, arena_zone, UpdateArena).

set_active ([], Arena) -> Arena;
set_active ([{{PlayerPid, CardOrder, CardID}, CardOption} | T], Arena) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			UpdateOption = seal_card:set_seal_option (CardOption, active, active),
			set_active (T, Arena ++ [{{PlayerPid, CardOrder, CardID}, UpdateOption}]);
		is_not_seal ->
			set_active (T, Arena ++ [{{PlayerPid, CardOrder, CardID}, CardOption}])
	end.

set_seal_active (PlayerPid, CardOrder, CardID) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	UpdateArena = set_active_status (Arena, PlayerPid, CardOrder, CardID, [], active),
	mnesia_play:set_player_data (PlayerPid, arena_zone, UpdateArena).

set_seal_inactive (PlayerPid, CardOrder, CardID) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	UpdateArena = set_active_status (Arena, PlayerPid, CardOrder, CardID, [], inactive),
	mnesia_play:set_player_data (PlayerPid, arena_zone, UpdateArena).

set_active_status ([], _, _, _, _, _) -> io:format("Card not found on arena~n");
set_active_status ([{{PlayerPid, CardOrder, CardID}, CardOption} | T], PlayerPid, CardOrder, CardID, Arena, Status) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->	UpdateOption = seal_card:set_seal_option (CardOption, active, Status),
				Arena ++ [{{PlayerPid, CardOrder, CardID}, UpdateOption}] ++ T;
		is_not_seal -> Arena ++ [{{PlayerPid, CardOrder, CardID}, CardOption}] ++ T
	end;
set_active_status ([Card | T], PlayerPid, CardOrder, CardID, Arena, Status) ->
	set_active_status (T, PlayerPid, CardOrder, CardID, append(Arena, [Card]), Status).

set_arena_card_option (PlayerPid, CardOrder, CardID, Option, Data) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	case find_card_option (Arena, CardOrder, CardID) of
		{ok, CardOption} ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal ->	OptionUpdate = seal_card:set_seal_option (CardOption, Option, Data),
						update_card (PlayerPid, CardOrder, CardID, OptionUpdate);
				is_not_seal ->	OptionUpdate = mystic_card:set_mystic_option (CardOption, Option, Data),
							update_card (PlayerPid, CardOrder, CardID, OptionUpdate)
			end;
		{error, Reason} ->
			{error, Reason}
	end.

get_all_effect (PlayerPid, CardOrder, CardID) ->
	case get_card_option (PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal ->
					{ok, RFx} = seal_card:get_seal_option(CardOption, receive_effect),
					{ok, SFx} = seal_card:get_seal_option(CardOption, skill_effect),
					{ok, RFx ++ SFx};
				is_not_seal -> mystic_card:get_mystic_option(CardOption, receive_effect)
			end;
		{error, Reason} ->
			{error, Reason}
	end.

get_receive_effect (PlayerPid, CardOrder, CardID) ->
	case get_card_option (PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> seal_card:get_seal_option(CardOption, receive_effect);
				is_not_seal -> mystic_card:get_mystic_option(CardOption, receive_effect)
			end;
		{error, Reason} ->
			{error, Reason}
	end.

change_arena_line (PlayerPid, CardOrder, CardID) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->	{ok, CardOption} = get_card_option (PlayerPid, CardOrder, CardID),
				{ok, Line} = seal_card:get_seal_option (CardOption, line),
				LineChange = change_line (Line),
				OptionUpdate = seal_card:set_seal_option (CardOption, line, LineChange),
				update_card (PlayerPid, CardOrder, CardID, OptionUpdate),
				{ok, Line};
		is_not_seal -> io:format("Change line card is not seal ~n")
	end.

assign_to_line (PlayerPid, CardOrder, CardID, LineAssign) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->	card_utility:set_card_option_field (PlayerPid, CardOrder, CardID, line, LineAssign, arena_zone);
		is_not_seal -> not_seal
	end.

change_line (Line) ->
	case Line of
		0 -> 1;
		1 -> 0
	end.

get_combat_data (PlayerPid, CardOrder, CardID) ->
	{ok, Attack} = get_card_power(PlayerPid, CardOrder, CardID, attack),
	{ok, Defend} = get_card_power(PlayerPid, CardOrder, CardID, defend),
	{ok, Speed} = get_card_power(PlayerPid, CardOrder, CardID, speed),
	{ok, {Attack, Defend, Speed}}.
	%% ตรวจสอบว่า มี Effect สลับค่าพลังหรือไม่ ถ้ามีให้ทำการสลับค่าพลัง
	% case ability_effect:check_card_effect(PlayerPid, CardOrder, CardID, switch_power) of
		% {ok, have_effect} ->
			% {ok, {Defend, Attack, Speed}};
		% {ok, no_effect} ->
			% {ok, {Attack, Defend, Speed}}
	% end.

get_combat_data (CombatData, []) -> {ok, CombatData};
get_combat_data ({_, Def, Spd}, [{at, Value} | T]) -> get_combat_data ({Value, Def, Spd}, T);
get_combat_data ({Atk, _, Spd}, [{df, Value} | T]) -> get_combat_data ({Atk, Value, Spd}, T);
get_combat_data ({Atk, Def, _}, [{sp, Value} | T]) -> get_combat_data ({Atk, Def, Value}, T);
get_combat_data (CombatData, [_ | T]) -> get_combat_data (CombatData, T).

set_support_seal(PlayerPid, CardOrder, CardID, CombineOption, SupportSealList) ->
	%{ok, PowerChange} =  mnesia_odbc:get_power_change_data (CardID, CombineOption),
	{ok, PowerChange} =  seal_card:get_combine_power(PlayerPid, CardOrder, CardID, CombineOption),
	{ok, Line} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, line, arena_zone),
	% หาการ์ด ออฟชั่นของการ์ดใบหลัก
	case check_support_seal (SupportSealList) of
		{support_complete} ->
			move_support_to_support_zone(SupportSealList, Line),
			%remove_support_effect(SupportSealList),
			update_main_option(PlayerPid, CardOrder, CardID, SupportSealList, PowerChange, CombineOption),
			%lists:foreach(fun({SupOwner, SubOrder, SubID}) -> card_utility:add_card_status(SupOwner, SubOrder, SubID, {support_to, PlayerPid, CardOrder, CardID}, support_cards) end, SupportSealList),
			lists:foreach(fun({SupOwner, SubOrder, SubID}) ->
				card_utility:add_card_status(SupOwner, SubOrder, SubID, {support_to, PlayerPid, CardOrder, CardID}, support_cards),
				{ok, Result} = card_utility:get_card_option_field(SupOwner, SubOrder, SubID, card_status, support_cards),
				smo_logger:fmsg("card status of {~p, ~p, ~p} is ~p~n", [SupOwner, SubOrder, SubID, Result])
			end, SupportSealList),
			% ทดสอบ 
			remove_support_from_arena(SupportSealList),
			{completed};
		{error, CardID, Reason} ->
			io:format ("Can not set support from card id ~p because ~p~n", [CardID, Reason]),
			{incomplete, Reason}
	end.

check_support_seal ([]) -> {support_complete};
check_support_seal ([{PlayerPid, CardOrder, CardID} | T]) ->
	case card_utility:check_card_zone (PlayerPid, CardOrder, CardID) of
		arena_zone -> check_support_seal (T);
		_ ->	io:format ("Support card not on arena~n"),
			{error, CardID, not_on_arena}
	end.

% remove_support_effect ([]) -> ok;
% % {CardOwner, CardOrder, CardID} คือ Support Seal
% remove_support_effect ([{CardOwner, CardOrder, CardID} | SupportList]) ->
	% % ลบ Effect ออกจากการ์ด ที่ได้รับ Effect จากใบที่จะเป็น Seal รองนี้
	% ability_affect:remove_all_give_effect_target (CardOwner, CardOrder, CardID),
	% % การ์ดที่เป็น ซีลรองจะลืมเอฟเฟคที่กระทำกับตัวมัน
% %	CardZone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% update_remove_effect (CardOwner, CardOrder, CardID),
	% effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [], update),
% %	card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, [], CardZone),
% %	card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, [], CardZone),
	% remove_support_effect (SupportList).

update_remove_effect (CardOwner, CardOrder, CardID) ->
	update_remove_effect (CardOwner, CardOrder, CardID, receive_effect),
	update_remove_effect (CardOwner, CardOrder, CardID, skill_effect).

update_remove_effect (CardOwner, CardOrder, CardID, OptionField) ->
	{ok, CFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, OptionField, arena_zone),
	UCFx = check_effect_remove_when_leave_play (CFx),
	card_utility:update_card_option_field (CardOwner, CardOrder, CardID, OptionField, UCFx, arena_zone).

check_effect_remove_when_leave_play ([]) -> [];
check_effect_remove_when_leave_play ([{GFx, Fx, D} | CFx]) ->
	case check_remove_effect (Fx) of
		[] ->	check_effect_remove_when_leave_play (CFx);
		UFx ->
			[{GFx, UFx, D}] ++ check_effect_remove_when_leave_play (CFx)
	end.

check_remove_effect ([]) -> [];
check_remove_effect ([FxCheck | Fx]) ->
	case FxCheck of
		{move, to_deck} ->
			[FxCheck] ++ check_remove_effect (Fx);
		{counter, _} ->
			[FxCheck] ++ check_remove_effect (Fx);
		_ ->	check_remove_effect (Fx)
	end.

update_main_option (PlayerPid, CardOrder, CardID, [], PowerChange, CombineOption) ->
	set_main_option (PlayerPid, CardOrder, CardID, power_change, PowerChange),
	set_main_option (PlayerPid, CardOrder, CardID, combine_option, CombineOption);
update_main_option (PlayerPid, CardOrder, CardID, [{PlayerPid, Sorder, Scid} | Support], PowerChange, CombineOption) ->
	{ok, CardOption} = card_utility:get_card_option (PlayerPid, CardOrder, CardID),
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	case find_card_option (Arena, Sorder, Scid) of
		{ok, Scoption} ->
			OptionUpdate = seal_card:add_support_seal (CardOption, {{PlayerPid, Sorder, Scid}, Scoption}),
			update_card (PlayerPid, CardOrder, CardID, OptionUpdate),
			update_main_option (PlayerPid, CardOrder, CardID, Support, PowerChange, CombineOption);
		{error, Reason} ->
			io:format ("Get card optioin error from ~p~n", [Reason])
	end.

set_main_option (PlayerPid, CardOrder, CardID, MainOption, Data) ->
	{ok, CardOption} = card_utility:get_card_option (PlayerPid, CardOrder, CardID),
	case MainOption of
		power_change ->
			OptionUpdate = seal_card:change_combine_power(CardOption, Data);
		combine_option ->
			OptionUpdate = seal_card:set_combine_option (CardOption, Data)
	end,
	update_card (PlayerPid, CardOrder, CardID, OptionUpdate).

remove_support_from_arena(SupportList) ->
	lists:foreach(
								fun({PlayerPid, CardOrder, CardID}) -> 
									{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
									ArenaUpdate = card_utility:remove(Arena, PlayerPid, CardOrder, CardID),
									mnesia_play:set_player_data(PlayerPid, arena_zone, ArenaUpdate)
								end, SupportList).
	
move_support_to_support_zone(SupportList, Line) ->
	lists:foreach(
								fun({SupPid, SupOrder, SupID}) -> 
									card_utility:remove_card_status(SupPid, SupOrder, SupID, assign_line, arena_zone),
									{ok, Arena} = mnesia_play:get_player_data (SupPid, arena_zone),
									{value, MoveSeal} = lists:keysearch({SupPid, SupOrder, SupID}, 1, Arena),
									{ok, GetSupport} = mnesia_play:get_player_data(SupPid, support_cards),
									% ย้ายจาก Arena Zone ไปไว้ที่ Support Zone เมือตอนแยกร่างออกมาจะได้ความสามารถไม่หาย
									mnesia_play:set_player_data(SupPid, support_cards, GetSupport ++ [MoveSeal]),
									card_utility:set_card_option_field(SupPid, SupOrder, SupID, line, Line, support_cards)
								end, SupportList).

set_combine_power (PlayerPid, CardOrder, CardID, PowerChange) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	case find_card_option (Arena, CardOrder, CardID) of
		{ok, CardOption} ->
			OptionUpdate = seal_card:change_combine_power (CardOption, PowerChange),
			update_card (PlayerPid, CardOrder, CardID, OptionUpdate);
		{error, Reason} ->
			{error, Reason}
	end.

break_support_seal(CardOwner, CardOrder, CardID) ->
	% ดูว่า บน Arena มีการ์ด อะไรอยู่่บ้่าง
	{ok, Arena} = mnesia_play:get_player_data(CardOwner, arena_zone),
	case lists:keysearch({CardOwner, CardOrder, CardID}, 1, Arena) of
	%case find_card_option (Arena, CardOrder, CardID) of
		%{ok, CardOption} ->
		{value, {{CardOwner, CardOrder, CardID}, CardOption}} -> 
			{ok, Line} = seal_card:get_seal_option (CardOption, line),
			case seal_card:pop_combine_status (CardOption) of
				%{ok, OptionUpdate, SupportSeal} ->
				{ok, OptionUpdate} ->
					%ReplyCard = support_to_arena(SupportSeal, Line, [{{PlayerPid, CardOrder, CardID}, CardOption}]),
					SupportSeal = arena_zone:get_support_seals(CardOwner, CardOrder, CardID), %SupportSeal= การ์ดที่อยู่ใน support_cards และเป็น Seal รองของการ์ดใบน
					{ok, SupportZone} = mnesia_play:get_player_data(CardOwner, support_cards), %SupportZone= การ์ดที่อยู่ใน support_cards ทั้งหมด
					Supporter = retrieve_support_data(CardOwner, SupportSeal, [], SupportZone), %Supporter= การ์ดที่อยู่ใน support_cards และเป็น Seal รองของการ์ดใบนี้ พร้อมการ์ด Option
					mnesia_play:set_player_data(CardOwner, support_cards, SupportZone -- Supporter), % ลบการ์ดที่จะต้องถูกแยกร่างออกมา ที่อยู่ใน support_cards ออก
					UpdateSupporter = update_supporter_data(Supporter, Line, []),
					mnesia_play:set_player_data(CardOwner, arena_zone, Arena ++ UpdateSupporter), % บวกการ์ดที่จะต้องถูกแยกร่างออกมาเข้่าไปใน arena_zone
					update_card(CardOwner, CardOrder, CardID, OptionUpdate),
					CheckMystic = check_mystic_support(SupportSeal),
					% [{{PlayerPid, CardOrder, CardID}, CardOption}] ++ UpdateSupporter คือ Seal ที่จะอยู่บน Arena ทั้งหมด ทั้ง Seal หลัก และ Seal รองที่แยกออกมา
					{ok, [{{CardOwner, CardOrder, CardID}, CardOption}] ++ UpdateSupporter, CheckMystic};
				{error, Reason} ->
					io:format ("Pop combine from ~p error because ~p~n", [CardID, Reason])
			end;
		_Reason ->
			{incomplete, _Reason}
	end.

check_mystic_support([]) -> [];
check_mystic_support([{PlayerPid, CardOrder, CardID}|SupportLists]) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> check_mystic_support(SupportLists); 
		_ -> [{PlayerPid, CardOrder, CardID}] ++ check_mystic_support(SupportLists)
	end.
	
retrieve_support_data(_, _, _, []) -> [];
retrieve_support_data(_, [], LastData, _) -> LastData;
retrieve_support_data(PlayerPid, [Card|Support], FirstData, SupportZone) ->
	case lists:keysearch(Card, 1, SupportZone) of
		{value, SupportData} -> 	retrieve_support_data(PlayerPid, Support, FirstData ++ [SupportData], SupportZone);
		_ -> retrieve_support_data(PlayerPid, Support, FirstData, SupportZone)
	end.
	
update_supporter_data([], _, LastData) -> LastData;
update_supporter_data([{{PlayerPid, CardOrder, CardID}, CardOption} | T], Line, FirstData) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			{CardInfo, CardCombine, CardStatus, _Active, _Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option} = CardOption,
			{value, {support_to, MainOwner, MainOrder, MainID}} = lists:keysearch(support_to, 1, CardStatus),
			%OptionUpdate1 = seal_card:set_seal_option (CardOption, line, Line),
			%OptionUpdate2 = seal_card:set_seal_option (OptionUpdate1, active, active),
			support_to_arena(T, Line, FirstData ++ [{{PlayerPid, CardOrder, CardID}, {CardInfo, CardCombine, CardStatus -- [{support_to, MainOwner, MainOrder, MainID}], active, Line, Growth, Combine, Mystic, GiveEffect, ReceiveEffect, SkillEffect, Steal, Option}}]);
		is_not_seal ->
			support_to_arena (T, Line, FirstData)
	end.
	

support_to_arena ([], _, ReplyCard) -> ReplyCard;
support_to_arena ([{{PlayerPid, CardOrder, CardID}, CardOption} | T], Line, ReplyCard) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			OptionUpdate = seal_card:set_seal_option (CardOption, line, Line),
			support_to_arena (T, Line, ReplyCard ++ [{{PlayerPid, CardOrder, CardID}, OptionUpdate}]);
		is_not_seal ->
			support_to_arena (T, Line, ReplyCard)
	end.

get_support_seals (PlayerPid, CardOrder, CardID) ->
	%{ok, CombineState} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone),
	case card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, combine, arena_zone) of
		{ok, CombineState} ->
			SupportSeal = seal_card:get_support_seal(CombineState),
			get_card_data (SupportSeal);
		_ -> []
	end.
		

get_card_data ([]) -> [];
get_card_data ([{CardOwner, CardOrder, CardID} | Cards]) ->
	[{CardOwner, CardOrder, CardID}] ++ get_card_data (Cards);
get_card_data ([{CardData, _} | Cards]) -> [CardData] ++ get_card_data (Cards).

get_mystic_pasted (CardOwner, CardOrder, CardID) ->
%	Zone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
%	io:format ("Get mystic pasted from ~p on ~p~n", [{CardOwner, CardOrder, CardID}, Zone]),
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, mystic) of
		{ok, Mystic} ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> get_card_data (Mystic);						
				is_not_seal -> io:format("Get mystic on mystic~n")
			end;
		{error, _} -> []
	end.

%% Card Effect
check_more_attack_card (RoomPid) ->
	{ok, PlayerTurnPid} = mnesia_play:get_game_data (RoomPid, player_turn),
	{ok, Arena} = mnesia_play:get_player_data (PlayerTurnPid, arena_zone),
	check_more_attack_status (Arena).

check_more_attack_status ([]) -> {no_more_attack};
check_more_attack_status ([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			case card_utility:check_card_status (PlayerPid, CardOrder, CardID, more_attack, arena_zone) of
				{ok, have_status} ->
					case decrease_more_attack(PlayerPid, CardOrder, CardID) of
						{ok, ok} -> {ok, {PlayerPid, CardOrder, CardID}};
						{ok, remove} -> {no_more_attack}
					end;
				{ok, have_no_status} -> check_more_attack_status (T)
			end;
		is_not_seal -> check_more_attack_status (T)
	end.

decrease_more_attack(PlayerPid, CardOrder, CardID) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	case find_card_option (Arena, CardOrder, CardID) of
		{ok, CardOption} ->
			{ok, SkillEffect} = seal_card:get_seal_option (CardOption, skill_effect),
			{Status, SkillEffectUpdate} = decrease_more_attack (SkillEffect, []),
			card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, SkillEffectUpdate, arena_zone),
			case Status of
				remove -> card_utility:remove_card_status(PlayerPid, CardOrder, CardID, more_attack, arena_zone);
				ok -> no_more_do
			end,
			{ok, Status};
		{error, Reason} -> io:format("Find card error from ~p~n", [Reason])
	end.

decrease_more_attack ([], SkillEffect) -> {ok, SkillEffect};
decrease_more_attack ([{CardData, Effect, Duration} | T], SkillEffect) ->
	case check_decrease (Effect, []) of
		{remove, _EffectUpdate} -> {remove, SkillEffect ++ T};
		{ok, EffectUpdate} -> {ok, SkillEffect ++ [{CardData, EffectUpdate, Duration}] ++ T};
		{not_found, Effect} -> decrease_more_attack (T, SkillEffect ++ [{CardData, Effect, Duration}])
	end.

check_decrease ([], Effect) -> {not_found, Effect};
check_decrease ([{more_attack, Times} | T], Effect) ->
	TimesUpdate = Times - 1,
	if
		TimesUpdate < 0 -> {remove, Effect ++ T};
		TimesUpdate =:= 0 -> {remove, Effect ++ T};
		true -> {ok, Effect ++ [{more_attack, TimesUpdate}] ++ T}
	end;
check_decrease ([H | T], Effect) -> check_decrease (T, Effect ++ [H]).

check_up_arena(PlayerPid) ->
	{ok, HandCards} = mnesia_play:get_player_data(PlayerPid, hand_cards),
	checkup_hand_status(HandCards),
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	{ok, HandCardsOp} = mnesia_play:get_player_data(OpponentPid, hand_cards),
	checkup_hand_status(HandCardsOp),
	{ok, Arena} = mnesia_play:get_player_data(PlayerPid, arena_zone),
	checkup_arena_status(Arena),
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	{ok, ArenaOpp} = mnesia_play:get_player_data(OpponentPid, arena_zone),
	checkup_arena_opp(ArenaOpp).
	
checkup_hand_status(HandCards) ->
	lists:foreach(
		fun({{PlayerPid, CardOrder, CardID}, _}) ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal ->
					card_utility:remove_card_status(PlayerPid, CardOrder, CardID, arena_to_hand_success, hand_cards);
				is_not_seal ->
					do_nothing%checkup_arena_status(T)
			end
		end, HandCards).
		
checkup_arena_status(ArenaCard) ->
	lists:foreach(
		fun({{PlayerPid, CardOrder, CardID}, _}) ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal ->
					check_moving_to_arena(PlayerPid, CardOrder, CardID),
					set_seal_active(PlayerPid, CardOrder, CardID);
				is_not_seal ->
					do_nothing%checkup_arena_status(T)
			end
		end, ArenaCard).

check_moving_to_arena(PlayerPid, CardOrder, CardID) ->
	{ok, CardOption} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, card_status, arena_zone),
	case find_option(CardOption) of
		ready_to_support ->
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, on_arena_success, arena_zone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, {supported, 1}, arena_zone);
		{not_ready, 1} -> 
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, on_arena_success, arena_zone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, supported, arena_zone),
			card_utility:add_card_status(PlayerPid, CardOrder, CardID, {supported, 1}, arena_zone)
	end.
		
checkup_arena_opp(OppArena) ->
	lists:foreach(fun({{OppPid, CardOrder, CardID}, _}) ->
		{ok, CardOption} = card_utility:get_card_option_field(OppPid, CardOrder, CardID, card_status, arena_zone),
		case find_option(CardOption) of
			ready_to_support -> 
				card_utility:remove_card_status(OppPid, CardOrder, CardID, {supported, 1}, arena_zone);
			{not_ready, 1} -> 
				card_utility:remove_card_status(OppPid, CardOrder, CardID, supported, arena_zone),
				card_utility:add_card_status(OppPid, CardOrder, CardID, {supported, 1}, arena_zone)
		end
	end, OppArena).

find_option ([]) -> ready_to_support;
find_option ([supported|_]) -> {not_ready, 1};
find_option ([{supported, 1}|_]) -> ready_to_support;
find_option ([_|T]) -> find_option (T).
