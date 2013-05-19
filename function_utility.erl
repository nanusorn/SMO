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
-module(function_utility).
-import (lists, [foreach/2]).

-compile(export_all).

last([]) -> [];
last([E|Es]) -> last(E, Es).
last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.

del_duplicate(List) ->
	SortList = qsort(List),
	del(SortList).
	
del([]) -> [];
del([ListHead1|[]]) -> [ListHead1];
del([ListHead1,ListHead2|Tail]) ->
	if
		ListHead1 =:= ListHead2 -> del([ListHead1|Tail]);  
		ListHead1 =/= ListHead2 -> [ListHead1|del([ListHead2|Tail])]
	end.

keysearch_all(_, []) -> [];
keysearch_all(Key, [{Key, Option, Target}|ToSearch]) ->
	[{Key, Option, Target}] ++ keysearch_all(Key, ToSearch);
keysearch_all(Key, [_|ToSearch]) -> keysearch_all(Key, ToSearch).

search_single_ability_target({CardID, AbilityNo}, Target, []) -> [];
search_single_ability_target({CardID, AbilityNo}, Target, [{Key, Option, []}|ToSearch]) ->
	[{Key, Option, []}] ++ search_single_ability_target({CardID, AbilityNo}, Target, ToSearch);
search_single_ability_target({CardID, AbilityNo}, Target, [{Key, Option, OtherTarget}|ToSearch]) ->
	case Target -- OtherTarget of
		[] -> target_had_receive_effect;
		_ ->	[{Key, Option, OtherTarget ++ Target}] ++ search_single_ability_target({CardID, AbilityNo}, Target, ToSearch)
	end.
	
curse_code([CurseHead|Tail]) ->
	case CurseHead of
		stone_curse -> Curse = 1;
		poison_curse -> Curse = 2;
		{last_dance_curse, _} -> Curse = 3;
		dimension_curse -> Curse = 4;
		freeze_curse -> Curse = 5;
		{charm_curse, _} -> Curse = 6;
		death_curse -> Curse = 7
	end,
	[Curse]++curse_code(Tail);
curse_code([]) -> [].

reverse_curse_code([DataHead|Tail]) ->
	case DataHead of
		1 -> Curse = stone_curse;
		2 -> Curse = poison_curse;
		3 -> Curse = last_dance_curse;
		4 -> Curse = dimension_curse;
		5 -> Curse = freeze_curse;
		6 -> Curse = charm_curse;
		7 -> Curse = death_curse
	end,
	[Curse]++reverse_curse_code(Tail);
reverse_curse_code([]) -> [].
	
attach_zone([{PlayerPid, CardOrder, CardID}|Tail]) ->
	CardZone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	[{CardZone, {PlayerPid, CardOrder, CardID}}]++attach_zone(Tail);
attach_zone([]) -> [].

all_curse_from_target([{PlayerPid, CardOrder, CardID}|Tail]) -> 
	CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
	game_info:card_curse({CardZone, {PlayerPid, CardOrder, CardID}})++all_curse_from_target(Tail);
all_curse_from_target([]) -> [].

with_effect_type(EffectType, EffectList) ->
	lists:map(fun(Effect) -> {EffectType, Effect} end, EffectList).
	
is_contain(FxRequire, Fx) -> is_contain(FxRequire, Fx, []).
	
is_contain(FxRequire, [{_, CardFx, _}|Tail], Contain) ->
	LeftOver = CardFx--FxRequire,
	Interest = CardFx--LeftOver,
	if
		Interest =:= [] -> is_contain(FxRequire, Tail, Contain);
		length(Interest) =< length(FxRequire) -> is_contain(FxRequire, Tail, Contain ++ Interest);
		true -> is_contain(FxRequire, Tail, Contain)
	end;
is_contain(_, [], Contain) -> Contain.

interest_contain(Interest, AllHad) ->
	LeftOver = AllHad--Interest,
	InterestHad = AllHad--LeftOver,
	if
		InterestHad =:= [] -> [];
		length(InterestHad) =< length(Interest) -> InterestHad;
		true -> []
	end.

what_contain(FxRequire, [{GFx, CardFx, Duration}|Tail]) ->
	LeftOver = CardFx--FxRequire,
	Interest = CardFx--LeftOver,
	if
		Interest =:= [] -> what_contain(FxRequire, Tail);
		length(Interest) =< length(FxRequire) -> [{GFx, CardFx, Duration}] ++ what_contain(FxRequire, Tail);
		true -> what_contain(FxRequire, Tail)
	end;
what_contain(_, []) -> [].

check_contain(_, []) -> false;
check_contain({CardOwner, CardOrder, CardID, AbilityID},[{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}|_]) -> {value, {{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}};
check_contain({CardOwner, CardOrder, CardID, AbilityID},[_|Tail]) -> check_contain({CardOwner, CardOrder, CardID, AbilityID}, Tail).
 
list_check_contain(_, []) -> [];
list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, [{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}|RemainFx]) -> 
	[{{CardOwner, CardOrder, CardID, AbilityID}, Fx, Duration}] ++ list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, RemainFx);
list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, [_|RemainFx]) -> list_check_contain({CardOwner, CardOrder, CardID, AbilityID}, RemainFx).
	
replace_effect(_, [], _) -> [];
replace_effect(GiveFx, [Effect|Fx], Duration) -> [{GiveFx, [Effect], Duration}] ++ replace_effect(GiveFx, Fx, Duration).
	
	

is_cancel_any_skill(AllFx) ->
	case check_cancel_any_skill(AllFx) of
		[] -> {error, []};
		FxReq -> {ok, FxReq}
	end.

%function_utility:is_cancel_any_skill([{a, [{cancel_skill, [all]}], du1}, {b, [{at, 1}], du2}]).

check_cancel_any_skill([{_, Fx, _}|Tail]) ->
	check_cancel_skill(Fx)++check_cancel_any_skill(Tail);
check_cancel_any_skill([]) -> [].

check_cancel_skill([Effect|Tail]) ->
	case Effect of
		{cancel_skill, Any} -> Any++check_cancel_skill(Tail);
		_ -> check_cancel_skill(Tail)
	end;
check_cancel_skill([]) -> [].

will_cancel([CancelCase|Tail], CardGiveFx, TPid, TOrder, TID) ->
	{GPid, GOrder, GID} =
	case CardGiveFx of
		{A, B, C} -> {A, B, C};
		{A, B, C, _} -> {A, B, C}
	end,
	case CancelCase of
		all -> {ok, cancel};
		all_opponent -> 
			case GPid of
				TPid -> {ok, not_cancel};
				_ ->	{ok, cancel}
			end;		
		{elem, Elem} -> 
			case attribute_check:element_check(game_info:card_element({arena_zone, {GPid, GOrder, GID}}), Elem) of
				true -> {ok, cancel};
			 _ -> {error, []}
			end;
		{type, Type} ->
			case attribute_check:type_check(game_info:card_type({arena_zone, {GPid, GOrder, GID}}), Type) of
				true -> {ok, cancel};
				_ -> {error, []}
			end;
		_ ->
			will_cancel(Tail, CardGiveFx, TPid, TOrder, TID)
	end;
will_cancel([], _, _, _, _) -> {error, []}.
			
	
qsort([]) -> [];
qsort([Pivot|T]) -> qsort([X || X <- T, X < Pivot]) ++ [Pivot] ++ qsort([X || X <- T, X >= Pivot]).


remove_effect(FxDelete, CardOwner, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	remove_effect(FxDelete, {CardZone, CardOwner, CardOrder, CardID}).
	
remove_effect(FxDelete, {CardZone, CardOwner, CardOrder, CardID}) ->
	{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, CardZone),
	RemainFx = check_remove_effect(FxDelete, ReceiveFx),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx, CardZone).
	
check_remove_effect(FxDelete, [{GiveFx, Fx, Duration}|Tail]) ->
	case FxDelete--Fx of
		[] -> Tail;
		_ -> [{GiveFx, Fx, Duration}] ++ check_remove_effect(FxDelete, Tail)
	end;
check_remove_effect(_, []) -> [].

random_select(0, _) -> [];
random_select(Num, TargetList) ->
	RandomNo = random:uniform(length(TargetList)),
	Target = [lists:nth(RandomNo, TargetList)],
	Target++random_select(Num-1, TargetList--Target).
	

	
add_card_status(Status, CardList) ->
	foreach(fun(Head) ->
		case Head of
			{PlayerPid, CardOrder, CardID} ->
				card_utility:add_card_status(PlayerPid, CardOrder, CardID, Status);
				_ -> io:format('Skill to Player~n')
		end
	end, CardList).

remove_card_status(Status, CardList) ->
	foreach(fun(Head) ->
		case Head of
			{PlayerPid, CardOrder, CardID} ->
				card_utility:remove_card_status(PlayerPid, CardOrder, CardID, Status);
			_ -> io:format('Skill to Player~n')
		end
	end, CardList).
	
card_match_condition([], _) -> [];
card_match_condition([CardHead|Tail], Conditon) ->
	case CardHead of
		{PlayerPid, CardOrder, CardID} -> [TargetWithZone] = attach_zone([{PlayerPid, CardOrder, CardID}]);
		{{PlayerPid, CardOrder, CardID}, _} -> [TargetWithZone] = attach_zone([{PlayerPid, CardOrder, CardID}]);
		{CardZone, {PlayerPid, CardOrder, CardID}} -> [TargetWithZone] = [{CardZone, {PlayerPid, CardOrder, CardID}}];
		{CardZone, {{PlayerPid, CardOrder, CardID}, _}} -> [TargetWithZone] = [{CardZone, {PlayerPid, CardOrder, CardID}}]
	end,
	check_other:check_all_other_require_attribute(xxx, TargetWithZone,  Conditon)++card_match_condition(Tail, Conditon).
	
check_environment(_TargetList, []) -> true;
check_environment(TargetList, [Condition|Con]) ->
	case check_each_environment_condition(TargetList, Condition) of
		true -> check_environment(TargetList, Con);
		Other -> Other
	end.
 
check_each_environment_condition(TargetList, {CheckType, Check}) ->
	case CheckType of
		arena_count ->
			attribute_check:arena_count(TargetList, Check)
		% hand_count ->
			% attribute_check:hand_count(AbilityOwnCardData, PlayerOppID, Value)
	end.
	
card_match_condition(_, [], _) -> [];
card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, [CardHead|Tail], Conditon) ->
	case CardHead of
		{OCardOwner, OCardOrder, OCardID} -> [TargetWithZone] = attach_zone([{OCardOwner, OCardOrder, OCardID}]);
		{{OCardOwner, OCardOrder, OCardID}, _} -> [TargetWithZone] = attach_zone([{OCardOwner, OCardOrder, OCardID}]);
		{OCardZone, {OCardOwner, OCardOrder, OCardID}} -> [TargetWithZone] = [{OCardZone, {OCardOwner, OCardOrder, OCardID}}];
		{OCardZone, {{OCardOwner, OCardOrder, OCardID}, _}} -> [TargetWithZone] = [{OCardZone, {OCardOwner, OCardOrder, OCardID}}]
	end,
	check_other:check_all_other_require_attribute({CardZone, {CardOwner, CardOrder, CardID}}, TargetWithZone,  Conditon)++card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, Tail, Conditon).

remove_card_zone([{_, {PlayerPid, CardOrder, CardID}}|Tail]) -> [{PlayerPid, CardOrder, CardID}]++remove_card_zone(Tail);
remove_card_zone([]) -> [].
	
seal_only([{Zone, {PlayerId, CardOrder, CardID}}|Tail]) ->
	%io:format('seal Only ~n'),
	Result = mnesia_odbc:is_seal_card(CardID),
	%io:format('Result ~p~n',[Result]),
	if
		Result =:= is_seal ->  [{Zone, {PlayerId, CardOrder, CardID}}|seal_only(Tail)];
		Result =:= is_not_seal -> seal_only(Tail)
	end;
seal_only([]) -> [].

at_zone(Zone,[Head|Tail]) ->
	[{Zone,Head}|at_zone(Zone, Tail)];
at_zone(_,[]) -> [].

exclude_option([{CardData, _}|Tail]) -> [CardData]++exclude_option(Tail);
exclude_option([CardData|Tail]) -> [CardData]++exclude_option(Tail);
exclude_option([]) -> [].

check_card_cancel_ability([Target|Tail], AbilityOwner) ->
	case cancel_any_ability(Target, AbilityOwner) of
		{ok, cancel} -> check_card_cancel_ability(Tail, AbilityOwner);
		_ -> [Target]++check_card_cancel_ability(Tail, AbilityOwner)
	end;
check_card_cancel_ability([], _) -> [].

cancel_any_ability(Target, CardGive) ->
	case Target of
		{CardOwner, CardOrder, CardID} ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			%io:format("get all card effect ~p~n", [CardFx]),
			case is_cancel_any_ability(CardFx) of
				{ok, [all]} -> {ok, cancel};
				{ok, Any} -> will_cancel(Any, CardGive, CardOwner, CardOrder, CardID);
				{error, _} -> {error, []}
			end;
		_ -> {error, []}
	end.
	
is_cancel_any_ability(AllFx) ->
	case check_cancel_any_ability(AllFx) of
		[] -> {error, []};
		FxReq -> {ok, FxReq}
	end.
	
check_cancel_any_ability([{_, Fx, _}|Tail]) ->
	check_cancel_ability(Fx)++check_cancel_any_ability(Tail);
check_cancel_any_ability([]) -> [].

check_cancel_ability([Effect|Tail]) ->
	case Effect of
		{cancel_ability, Any} -> Any++check_cancel_ability(Tail);
		_ -> check_cancel_ability(Tail)
	end;
check_cancel_ability([]) -> [].

check_target_cancel_any(_, []) -> [];
check_target_cancel_any({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, [{WhoseFx, CheckReceive}|CheckRec]) ->
	RemainRec =
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			case mnesia_odbc:is_skill(AbilityID) of
				true -> skill_utility:check_target_cancel_skill({CardOwner, CardOrder, CardID}, CheckReceive);
				_ -> check_card_cancel_ability(CheckReceive, {CardOwner, CardOrder, CardID})
			end;
		_ -> mystic_effect:check_target_cancel_mystic(CardOwner, CardOrder, CardID, CheckReceive)
	end,
	case RemainRec of
		[] -> check_target_cancel_any({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, CheckRec);
		_ -> [{WhoseFx, RemainRec}] ++ check_target_cancel_any({CardOwner, CardOrder, CardID, AbilityNo, AbilityID}, CheckRec)
	end.

get_card_to_client(_, [], DestroySize) -> [DestroySize];
get_card_to_client(PlayerPid, [{PlayerPid, CardOrder, CardID} | T], DestroySize) ->
	get_card_to_client(PlayerPid, T, DestroySize + 1) ++ [1, CardOrder, <<CardID:16>>];
get_card_to_client(PlayerPid, [{_, CardOrder, CardID} | T], DestroySize) ->
	get_card_to_client(PlayerPid, T, DestroySize + 1) ++ [0, CardOrder, <<CardID:16>>].
	
reverse_data_to_card(_, []) -> [];
reverse_data_to_card(PlayerPid, [WhoseCard, CardOrder, B1, B2|T]) ->
	OppPid = mnesia_play:get_opponent_pid(PlayerPid),
	<<CardID:16>> = <<B1, B2>>,
	case WhoseCard of
		1 -> [{PlayerPid, CardOrder, CardID}]++reverse_data_to_card(PlayerPid, T);
		_ -> [{OppPid, CardOrder, CardID}]++reverse_data_to_card(PlayerPid, T)
	end.

cal_loose_all_card_counter([], Counter) -> Counter;
cal_loose_all_card_counter([{CardOwner, CardOrder, CardID}|Card], Counter) ->
	CardCounter = game_info:card_counter({arena_zone, {CardOwner, CardOrder, CardID}}),
	{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, arena_zone),
	{_, RemainFx} = delete_counter:check_effect(CardCounter, [], ReceiveFx),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx, arena_zone),
	effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
	cal_loose_all_card_counter(Card, Counter + CardCounter).
	
is_odd(Number) ->
	case (Number rem 2) of 
		0 -> false;
		_ -> true
	end.
	
check_receive_pid([]) -> [];
check_receive_pid([Receive|R]) ->
	case Receive of
		{_Pid, _Order, _ID} -> check_receive_pid(R);
		Pid -> [Pid] ++ check_receive_pid(R)
	end.
	
activate_decide_change_zone(PlayerList, PlayerPid, ToZone) ->
	Zone = zone_code(ToZone),
	lists:foreach(fun({Pid, _}) ->
		case Pid of
			PlayerPid -> gen_server:cast(Pid, {send, [16#88, 16#40, 1, Zone]});
			_ -> gen_server:cast(Pid, {send, [16#88, 16#40, 0, Zone]})
		end
	end, PlayerList).
	
zone_code(Zone) ->
	case Zone of
		arena_zone -> 0;
		shrine_cards -> 1;
		seal_deck -> 2;
		mystic_deck -> 3;
		hand_cards -> 4;
		remove_zone -> 5;
		support_cards -> 6
	end.
	
response_decide_change_zone(Pid, Data, {PlayerPid, ToZone, ToZoneList}) ->
	case Pid of
		PlayerPid ->
			case Data of
				[0] -> interfere_step:return_play(check_play_step);
				[1] ->
					case ToZone of
						shrine_cards -> shrine_zone:card_to_shrine(PlayerPid, ToZoneList)
					end
			end;
		_ -> gen_server:cast({player_decide_change_zone, PlayerPid, ToZone, ToZoneList})
	end.
	
