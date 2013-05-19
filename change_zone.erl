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
-module(change_zone).
-export(
					[
						check_card_to_other_zone/2,
						check_zone_to_move/0
					]
				).

% เช็คว่าจาก Zone ปัจจุบันที่จะไป มี Effect ระบุให้ต้องย้่ายการ์ด ไป Zone อื่นหรือไม่
check_card_to_other_zone([], _ThisZone) -> [];
check_card_to_other_zone([{TPlayerPid, TCardOrder, TCardID}|Target], ThisZone) ->
	CardFx = card_utility:get_all_card_effect(TPlayerPid, TCardOrder, TCardID),
	smo_logger:fmsg("check to other zone  CardFx are ~p~n", [CardFx]),
	DuraCheck =
	case ThisZone of
		shrine_cards -> when_to_shrine;
		remove_zone -> when_to_remove;
		hand_cards -> when_to_hand;
		library -> when_to_deck
	end,
	case check_to_zone_duration(CardFx, DuraCheck) of
		to_this_zone -> check_card_to_other_zone(Target, ThisZone);
		_OtherZone -> 
			case stack_pool:get_last_stack(self(), zone_to_move) of
				{ok, Zone} -> 
					ZoneRemain = function_utility:del_duplicate(Zone ++ [_OtherZone]),
					stack_pool:set_stack_option(self(), zone_to_move, ZoneRemain);
				_ -> stack_pool:set_stack_option(self(), zone_to_move, [_OtherZone])
			end,
			stack_pool:add_stack_option_field(self(), _OtherZone, [{TPlayerPid, TCardOrder, TCardID}]),
			[{TPlayerPid, TCardOrder, TCardID}] ++ check_card_to_other_zone(Target, ThisZone)
	end.
	
	
	% case mnesia_odbc:is_seal_card(TCardID) of
		% is_seal ->
			% case check_to_zone_duration(CardFx, DuraCheck) of
				% to_this_zone -> check_card_to_other_zone(Target, ThisZone);
				% _OtherZone -> 
					% case stack_pool:get_last_stack(self(), zone_to_move) of
						% {ok, Zone} -> 
							% ZoneRemain = function_utility:del_duplicate(Zone ++ [_OtherZone]),
							% stack_pool:set_stack_option(self(), zone_to_move, ZoneRemain);
						% _ -> stack_pool:set_stack_option(self(), zone_to_move, [_OtherZone])
					% end,
					% stack_pool:add_stack_option_field(self(), _OtherZone, [{TPlayerPid, TCardOrder, TCardID}]),
					% [{TPlayerPid, TCardOrder, TCardID}] ++ check_card_to_other_zone(Target, ThisZone)
			% end;
		% is_not_seal ->
			% case mystic_to_other_zone(CardFx, DuraCheck) of
				% to_this_zone -> [] ++ check_card_to_other_zone(Target, ThisZone);
				% _OtherZone -> 
					% case stack_pool:get_last_stack(self(), zone_to_move) of
						% {ok, Zone} -> 
							% ZoneRemain = function_utility:del_duplicate(Zone ++ [_OtherZone]),
							% stack_pool:set_stack_option(self(), zone_to_move, ZoneRemain);
						% _ -> stack_pool:set_stack_option(self(), zone_to_move, [_OtherZone])
					% end,
					% stack_pool:add_stack_option_field(self(), _OtherZone, [{TPlayerPid, TCardOrder, TCardID}]),
					% [{TPlayerPid, TCardOrder, TCardID}] ++ check_card_to_other_zone(Target, ThisZone)
			% end
	% end.


check_to_zone_duration([], _) -> to_this_zone;
check_to_zone_duration([{_GFx, Fx, Duration}|CardFx], DuraCheck) ->
	case Duration of
		DuraCheck -> 
			case check_move_to_zone_effect(Fx) of
				to_this_zone -> check_to_zone_duration(CardFx, DuraCheck);
				OtherZone -> OtherZone
			end;
		_ -> check_to_zone_duration(CardFx, DuraCheck)
	end.

%[{GFX, [{target_ability,[{<0.709.0>,30,361}]},{fx_target,to_remove_zone},{fx_to_target,[{{<0.709.0>,8,261},[{move,to_remove_zone}], when_to_shrine}]}], Duration}]
mystic_to_other_zone([], _) -> to_this_zone;
mystic_to_other_zone([{_, SubFx, _Duration}|Fx], DuraCheck) -> 
	case mystic_subfx_check(SubFx, DuraCheck) of
		to_this_zone -> mystic_to_other_zone(Fx, DuraCheck);
		OtherZone -> OtherZone
	end.

mystic_subfx_check([], _DuraCheck) -> to_this_zone;
mystic_subfx_check([{_, [{Pid,_,_}]}|SubFx], DuraCheck) when is_pid(Pid) -> mystic_subfx_check(SubFx, DuraCheck);
mystic_subfx_check([{_, Unknow}|SubFx], DuraCheck) when is_atom(Unknow) -> mystic_subfx_check(SubFx, DuraCheck);
mystic_subfx_check([{_, Unknow}|SubFx], DuraCheck) -> 
	case check_to_zone_duration(Unknow, DuraCheck) of
		to_this_zone -> mystic_subfx_check(SubFx, DuraCheck);
		OtherZone -> OtherZone
	end.

check_move_to_zone_effect([]) -> to_this_zone;
check_move_to_zone_effect([{move, ToDo}|Fx]) ->
	case ToDo of
		to_remove_zone -> remove_zone;
		to_hand_zone -> hand_cards;
		_ -> check_move_to_zone_effect(Fx)
	end;
check_move_to_zone_effect([{_, _}|Fx]) -> check_move_to_zone_effect(Fx). 

check_zone_to_move() ->
	ZoneMove = stack_pool:get_last_stack(self(), zone_to_move),
	smo_logger:fmsg("get last stack of Zone to Move ~p~n", [ZoneMove]),
	case ZoneMove of
		{ok, Zone} ->
			case length(Zone) of
				1 ->  
					stack_pool:remove_stack_option(self(), zone_to_move),
					move_to_zone(Zone);
				_ -> select_zone_to_move(Zone)
			end;
		_ -> 
			LastPlay = get(last_play),
			erase(last_play),
			interfere_step:return_play(LastPlay)
	end.

move_to_zone([DestinationZone]) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	case DestinationZone of
		remove_zone -> 
			{ok, RemoveTarget} = stack_pool:get_last_stack(self(), remove_zone),
			smo_logger:fmsg("card to Remove Zone ~p~n", [RemoveTarget]),
			remove_zone:move_to_remove_zone(PlayerPid, RemoveTarget);
		hand_cards ->
			{ok, CardToHand} = stack_pool:get_last_stack(self(), hand_cards),
			move_to_hand:move_card_to_hand(PlayerPid, CardToHand)
	end.
	
select_zone_to_move([ZoneRemain|Zone]) -> 
	stack_pool:set_stack_option(self(), zone_to_move, Zone),
	move_to_zone([ZoneRemain]).
