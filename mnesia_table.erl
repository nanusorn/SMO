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
-module (mnesia_table).

-include_lib ("stdlib/include/qlc.hrl").
-include ("record.hrl").
-include ("play_record.hrl").
-include ("s_ability.hrl").
%-include ("s_ability_new.hrl").
-include ("s_skill.hrl").
-include ("mystic_ability.hrl").
-include ("m_skill.hrl").
%-include ("s_added_skill.hrl").

-import (lists, [foreach/2]).

-export ([start/0, select/1, do/1, clear/1, remove_data/2, count_data/1]).

-export([initial_s_card_table/0]).
-export([write_to_s_skill/3, get_skill_id/1, write_to/0]).

start() ->
	register(mnesia_table, spawn( fun() -> start_mnesia_table() end) ).
	%% McDuck : Why do we need to spawn process? since the process never have loop and just
	%% need to initialize mnesia application process.
	%%start_mnesia_table().

start_mnesia_table() ->
	%%smo_logger:msg("Hellow1"),
	mnesia_receiver_start(),
	%%smo_logger:msg("Hellow2"),
	mnesia_start().

mnesia_receiver_start () ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table (room,			[{attributes, record_info(fields, room)}]),
	mnesia:create_table (user_data,		[{attributes, record_info(fields, user_data)}]),
	mnesia:create_table (seal_card,		[{attributes, record_info(fields, seal_card)}]),
	mnesia:create_table (mystic_card,	[{attributes, record_info(fields, mystic_card)}]),
	mnesia:create_table (combination,	[{attributes, record_info(fields, combination)}]),

	mnesia:create_table (game_data,		[{attributes, record_info(fields, game_data)}]),
	mnesia:create_table (player_data,	[{attributes, record_info(fields, player_data)}]),
	mnesia:create_table (play_stack,	[{attributes, record_info(fields, play_stack)}]),
	mnesia:create_table (growth_data,	[{attributes, record_info(fields, growth_data)}]),

	mnesia:create_table (card_infomation,	[{attributes, record_info(fields, card_infomation)}]),
	
	mnesia:create_table(card_ability,	[{attributes, record_info(fields, card_ability)}]),
	mnesia:create_table(continue_condition,	[{attributes, record_info(fields, continue_condition)}]),
	mnesia:create_table(card_skill,		[{attributes, record_info(fields, card_skill)}]),
	mnesia:create_table(mystic_ability,	[{attributes, record_info(fields, mystic_ability)}]),
	
	%mnesia:create_table(added_skill,		[{attributes, record_info(fields, added_skill)}]),
	%%smo_logger:msg("Stop mnesia"),
	mnesia:stop().

mnesia_start() ->
	%%smo_logger:msg("Start again"),
	mnesia:start(),
	mnesia:wait_for_tables([	room, 
														user_data, 
														seal_card, 
														mystic_card, 
														combination,
														game_data, 
														player_data, 
														play_stack, 
														growth_data, 
														card_infomation,
														card_ability, 
														%continue_condition, 
														card_skill, 
														mystic_ability], 20000),
	%%smo_logger:msg("First initialize"),
	initial_ability_card_table(),
	initial_continue_condition_table(),
	%%smo_logger:msg("Second initialize"),
	initial_skill_card_table(),
	initial_ability_mystic_table(),
	
	initial_s_card_table(),
	initial_m_card_table(),
	initial_growth_table(),
	initial_combination_table().
	
	%initial_added_skill_table(),
	%initial_new_ability_card_table(),
	%psql_odbc ! {store_card_data}.

%% ============== ส่วนของการทดสอบ Table =======
select(TableName) ->
	do(qlc:q([X || X <- mnesia:table(TableName)])).

clear(TableName) ->
	mnesia:clear_table(TableName).

remove_data (TableName, PrimaryKey) ->
	Oid = {TableName, PrimaryKey},
	F = fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

count_data (TableName) ->
	Query = do(qlc:q([X || X <- mnesia:table(TableName)])),
	case do(Query) of
		Result -> lists:flatlength (Result)
	end.
	

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

initial_ability_card_table() ->
    mnesia:clear_table(card_ability),
    F = fun() ->
		foreach(fun mnesia:write/1, ability_table() )
	end,
    mnesia:transaction(F).
	 
initial_continue_condition_table() ->
	mnesia:clear_table(continue_condition),
   F = 
	fun() ->
		foreach(fun mnesia:write/1, continue_condition_table() )
	end,
   mnesia:transaction(F).
	
%initial_new_ability_card_table() ->
%    mnesia:clear_table(card_ability),
%    F = fun() ->
%		foreach(fun mnesia:write/1, ability_table_new() )
%	end,
%    mnesia:transaction(F).

initial_skill_card_table() ->
	mnesia:clear_table(card_skill),
    F = fun() ->
		foreach(fun mnesia:write/1, skill_table() )
	end,
    mnesia:transaction(F).
	
initial_ability_mystic_table() ->
	mnesia:clear_table(mystic_ability),
    F = fun() ->
		foreach(fun mnesia:write/1, m_ability_table() )
	end,
    mnesia:transaction(F).
%------------------------------------------------------------------
initial_s_card_table() ->
	mnesia:clear_table(seal_card),
    F = fun() ->
		foreach(fun mnesia:write/1, s_card_table() )
	end,
    mnesia:transaction(F).

initial_m_card_table() ->
	mnesia:clear_table(mystic_card),
    F = fun() ->
		foreach(fun mnesia:write/1, m_card_table() )
	end,
    mnesia:transaction(F).
	 
initial_growth_table() ->
	mnesia:clear_table(growth_data),
    F = fun() ->
		foreach(fun mnesia:write/1, growth_table() )
	end,
    mnesia:transaction(F).
	 
initial_combination_table() ->
	mnesia:clear_table(combination),
    F = fun() ->
		foreach(fun mnesia:write/1, combination_table() )
	end,
    mnesia:transaction(F).
	
%initial_added_skill_table() ->
%	mnesia:clear_table(added_skill),
%   F = fun() ->
%		foreach(fun mnesia:write/1, added_skill_table() )
%	end,
%   mnesia:transaction(F).
    
ability_table() ->
	set1_dark:dark1_ability()++set2_dark:dark2_ability()++set3_dark:dark3_ability()++
	set1_earth:earth1_ability()++set2_earth:earth2_ability()++set3_earth:earth3_ability()++
	set1_fire:fire1_ability()++set2_fire:fire2_ability()++set3_fire:fire3_ability()++
	set1_light:light1_ability()++set2_light:light2_ability()++set3_light:light3_ability()++
	set1_water:water1_ability()++set2_water:water2_ability()++set3_water:water3_ability()++
	set1_wind:wind1_ability()++set2_wind:wind2_ability() ++ set3_wind:wind3_ability()++
	set2_mystic_ability:mystic() ++ set3_mystic_ability:mystic().
	
continue_condition_table() -> 
	set1_light_continue:continue_condition() ++ set2_light_continue:continue_condition() ++ set3_light_continue:continue_condition() ++
	set1_earth_continue:continue_condition() ++ set2_earth_continue:continue_condition() ++ set3_earth_continue:continue_condition() ++
	set1_fire_continue:continue_condition() ++ set2_fire_continue:continue_condition() ++ set3_fire_continue:continue_condition() ++
	set1_water_continue:continue_condition() ++ set2_water_continue:continue_condition() ++ set3_water_continue:continue_condition() ++
	set1_wind_continue:continue_condition() ++ set2_wind_continue:continue_condition() ++ set3_wind_continue:continue_condition() ++
	set1_dark_continue:continue_condition() ++ set2_dark_continue:continue_condition()++ set3_dark_continue:continue_condition() ++
	set1_mystic_continue:continue_condition() ++ set2_mystic_continue:continue_condition()++set3_mystic_continue:continue_condition().
	
%ability_table_new() ->
%	set1_dark_new:dark1_ability()++set2_dark_new:dark2_ability()++set3_dark:dark3_ability()++
%	set1_earth_new:earth1_ability()++set2_earth_new:earth2_ability()++set3_earth:earth3_ability()++
%	set1_fire_new:fire1_ability()++set2_fire_new:fire2_ability()++set3_fire:fire3_ability()++
%	set1_light_new:light1_ability()++set2_light_new:light2_ability()++set3_light:light3_ability()++
%	set1_water_new:water1_ability()++set2_water_new:water2_ability()++set3_water:water3_ability()++
%	set1_wind_new:wind1_ability()++set2_wind_new:wind2_ability()++set3_wind:wind3_ability().

skill_table() ->
	s_skill_set1:dark_skill()++s_skill_set2:dark_skill()++s_skill_set3:dark_skill()++
	s_skill_set1:earth_skill()++s_skill_set2:earth_skill()++s_skill_set3:earth_skill()++
	s_skill_set1:fire_skill()++s_skill_set2:fire_skill()++s_skill_set3:fire_skill()++
	s_skill_set1:light_skill()++s_skill_set2:light_skill()++s_skill_set3:light_skill()++
	s_skill_set1:water_skill()++s_skill_set2:water_skill()++s_skill_set3:water_skill()++
	s_skill_set1:wind_skill()++s_skill_set2:wind_skill()++s_skill_set3:wind_skill()++
	s_skill_set1:m_skill()++s_skill_set3:m_skill()++added_skill:ps_ability()++s_skill_set2:promo_skill().
	
m_ability_table() ->
	mystic_ability_set1:ability()++
	mystic_ability_set2:ability()++
	mystic_ability_set3:ability().
	%m_ability_set3:ability().

%added_skill_table() ->
%	added_skill:ps_ability().

s_card_table() ->
	set1_seal_light:light()++set2_seal_light:light()++set3_seal_light:light()++set4_seal_light:light()++
	set1_seal_earth:earth()++set2_seal_earth:earth()++set3_seal_earth:earth()++set4_seal_earth:earth()++
	set1_seal_fire:fire()++set2_seal_fire:fire()++set3_seal_fire:fire()++set4_seal_fire:fire()++
	set1_seal_water:water()++set2_seal_water:water()++set3_seal_water:water()++set4_seal_water:water()++
	set1_seal_wind:wind()++set2_seal_wind:wind()++set3_seal_wind:wind()++set4_seal_wind:wind()++
	set1_seal_dark:dark()++set2_seal_dark:dark()++set3_seal_dark:dark()++set4_seal_dark:dark().
	
m_card_table() ->
	set1_mystic:mystic()++set2_mystic:mystic()++set3_mystic:mystic().
	
growth_table() ->
	growth_set2:growth()++growth_set3:growth()++growth_set4:growth().
	
combination_table() ->
	combination_set1:combination()++combination_set2:combination()++combination_set3:combination()++combination_set4:combination().
	
% -----------------------------------------------------------------
% write_to_s_ability(AbilityID, WirteField) ->
	% Row =	
	% case WriteField of
		% _ -> #card_ability{ability_id = AbilityID, option_list = OptionList}
	% end,
	% F =	fun() -> mnesia:write(Row) end,
	% mnesia:transaction(F).
	
write_to_s_skill(SkillID, WriteField, FieldWrite) ->
	Get = get_skill_id(SkillID),
	[{card_skill, SkillID, CID, SNo, SMp, SInfere,
				Sneed, 
					SThen, SDo,
						OmustCheck, OCheck,
						PmustCheck, Pside, Pcheck,									
						SmustCheck, _SCheck, 											
						OtmustCheck, OtplayerCheck, OtzoneCheck, OtCard, OtselfInclude, Otcheck, Otcount,			                        
							OhaveFx, OFx, OFxDu,
							PhaveFx, PgotFx, PFx,	PFxDu,										
							ShaveFx, SFx, SFxDu, 
							OthaveFx, OtOfPlayer, OtOnZone, OtCardType, TselfInclude, TCon,
								TFx, TFxDu,
							OrhaveFx, Orplayer, OrOnZone, OrCardType, OrSelfInclude, OrCon,
								OrFx, OrDxDu
	}] = Get,
	io:format("--------------------------------------------------------"),								
	Row =
	case WriteField of
		owner_check ->
			#card_skill{skill_id = SkillID, card_id = CID, skill_no = SNo, skill_mp = SMp, can_interfere = SInfere,
				skill_condition_need_check = Sneed, 
					then_assign = SThen, then_do = SDo,
						playerown_must_check = OmustCheck, playerown_check = OCheck,
						player_must_check = PmustCheck, player_side_check = Pside, player_check =  Pcheck,									
						owner_must_check = SmustCheck, owner_check = FieldWrite, 											
						other_must_check = OtmustCheck, other_player_check = OtplayerCheck, other_present_check = OtzoneCheck, other_card_type_check = OtCard, other_self_include_check = OtselfInclude, other_check = Otcheck, other_match_count = Otcount,			                        
							playerown_have_effect = OhaveFx, playerown_effect = OFx, playerown_duration_effect = OFxDu,
							player_have_effect = PhaveFx, player_which_got_effect = PgotFx, player_effect = PFx,	player_duration_effect = PFxDu,										
							owner_have_effect = ShaveFx, owner_effect = SFx, owner_duration_effect = SFxDu, 
							other_target_have_effect = OthaveFx, target_player_check = OtOfPlayer, target_present_check = OtOnZone, target_card_type_check = OtCardType, target_self_include_check = TselfInclude, target_check = TCon,
								other_effect = TFx, other_duration_effect = TFxDu,
							or_target_have_effect = OrhaveFx, or_target_player_check = Orplayer, or_target_present_check = OrOnZone, or_target_card_type_check = OrCardType, or_self_include_check = OrSelfInclude, or_check = OrCon,
								or_result_effect = OrFx, or_duration = OrDxDu}
	end,
	F =	fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).
			
get_skill_id(SkillID) ->
	do(qlc:q([X || X <-mnesia:table(card_skill), X#card_skill.skill_id =:= SkillID])).
	
	
write_to() ->
	Row = 
	#mystic_ability	{m_ability_id = s1_no098_a1, card_id = 354, m_ability_number = 1, can_interfere = n,
				need_check_self = n, self_condition_check = [],
				need_other_check = y, player_card_check = opponent, player_card_zone_check = [arena_zone], card_tyep_check = seal, include_this_check = null, other_condition_check =  [{type, {n, "Machine"}}, {can_move_to, 1}, {line, null}], number_of_require = 1,
					then_do_id = null,
						continuous_type = null,
							have_fx_to_owner = n, fx_owner_got_type = null, owner_fx_select = {}, fx_to_owner = [], owner_fx_duration = null,
							have_fx_to_any_player = n, player_got_fx = null, fx_player_got_type = null, player_fx_select = {}, fx_to_player = [], player_fx_duration = null, 										
							have_fx_to_this = n, fx_this_got_type = null, this_fx_select = {}, fx_to_this = [], this_fx_duration = null,
							have_fx_to_target = n, target_card_owner = null, target_card_zone_check = [], target_card_type = null, target_include_this = null, target_condition = [],
								fx_target_got_type = null, target_fx_select = {}, fx_target_receive = [], target_fx_duration = null,
							have_fx_to_beyond_target = y, beyond_target_card_owner = opponent, beyond_target_card_zone = [arena_zone], beyond_target_card_type = seal, beyond_target_include_this = null, beyond_target_condition = [{type, {n, "Machine"}}, {can_move_to, 1}, {line, null}],
								fx_beyond_target_got_type = immediately, beyond_target_fx_select = {do_not_need_select, 0}, fx_beyond_target_receive = [{player_action, change_line}], beyond_target_fx_duration = null},
	F =	fun() -> mnesia:write(Row) end,
	mnesia:transaction(F).
	