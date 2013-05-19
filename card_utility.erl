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
-module (card_utility).

-export ([add_card_status/5, add_card_status/4, remove_card_status/5, remove_card_status/4, get_all_card/1, get_all_card/2, get_all_card/3, remove/4]).
-export ([update_card/5, update_card/6, remove_card/4, remove_card/5, check_card_status/5, check_card_status/4]).
-export ([get_card_option_field/5, get_card_option_field/4, set_card_option_field/6, set_card_option_field/5, have_card_on_zone/4, check_card_destroyed/1]).
-export ([update_card_option_field/6, update_card_option_field/5, check_card_zone/3]). %check_card_affect/0, check_card_affect/1, 
-export ([change_card_zone/5, change_card_zone/6, get_all_card_effect/3, get_all_card_effect/4, get_card_option/3, get_card_option/4]).
-export ([get_card_combine_part/4, check_card_active/3, get_cards_have_status/2, check_active_status/3]).
-export ([check_card_effect_duration/3]).
-export ([get_cards_from_multiple_zone/1, update_support_cards/5, get_support_seal/1]).

update ([], _, _, _, _) -> [];
update ([{{PlayerPid, CardOrder, CardID}, _} | T], PlayerPid, CardOrder, CardID, CardOption) -> 
	[{{PlayerPid, CardOrder, CardID}, CardOption}] ++ T;
update ([H | T], PlayerPid, CardOrder, CardID, CardOption) ->
	[H] ++ update (T, PlayerPid, CardOrder, CardID, CardOption).

remove ([], _, _, _) -> [];
remove ([{{PlayerPid, CardOrder, CardID}, _} | T], PlayerPid, CardOrder, CardID) -> T;
remove ([{PlayerPid, CardOrder, CardID} | T], PlayerPid, CardOrder, CardID) -> T;
remove ([H | T], PlayerPid, CardOrder, CardID) -> [H] ++ remove (T, PlayerPid, CardOrder, CardID).

%find_card ([], _, _) -> {error, no_card_on_shrine};
%find_card ([{{PlayerPid, CardOrder, CardID}, CardOption} | _], CardOrder, CardID) -> {ok, {{PlayerPid, CardOrder, CardID}, CardOption}};
%find_card ([_ | T], CardOrder, CardID) -> find_card (T, CardOrder, CardID).

find_card_option ([], _, _, _) -> {error, not_on_zone};
find_card_option ([{{PlayerPid, CardOrder, CardID}, CardOption} | _], PlayerPid, CardOrder, CardID) -> {ok, CardOption};
find_card_option ([_ | T], PlayerPid, CardOrder, CardID) -> find_card_option (T, PlayerPid, CardOrder, CardID).

have_on_zone ([], _, _, _) -> not_on_zone;
have_on_zone ([{PlayerPid, CardOrder, CardID} | _], PlayerPid, CardOrder, CardID) -> on_zone;
have_on_zone ([{{PlayerPid, CardOrder, CardID}, _} | _], PlayerPid, CardOrder, CardID) -> on_zone;
have_on_zone ([_ | T], PlayerPid, CardOrder, CardID) -> have_on_zone (T, PlayerPid, CardOrder, CardID).

get_player_cards ([], _, _) -> [];
get_player_cards ([{PlayerPid, _} | T], CardType, Zone) ->
	{ok, Cards} = mnesia_play:get_player_data(PlayerPid, Zone),
	get_card (Cards, CardType) ++ get_player_cards (T, CardType, Zone);
get_player_cards (PlayerPid, CardType, Zone) ->
	{ok, Cards} = mnesia_play:get_player_data(PlayerPid, Zone),
	get_card (Cards, CardType).

get_card ([], _) -> [];
get_card ([{PlayerPid, CardOrder, CardID} | T], CardType) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CardType -> [{PlayerPid, CardOrder, CardID}] ++ get_card (T, CardType);
		_ -> get_card (T, CardType)
	end;
get_card ([{{PlayerPid, CardOrder, CardID}, CardOption} | T], CardType) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CardType -> [{{PlayerPid, CardOrder, CardID}, CardOption}] ++ get_card (T, CardType);
		_ -> get_card (T, CardType)
	end.

get_card_option(PlayerPid, CardOrder, CardID) ->
	CardZone = check_card_zone(PlayerPid, CardOrder, CardID),
	get_card_option(PlayerPid, CardOrder, CardID, CardZone).
	
get_card_option(PlayerPid, CardOrder, CardID, CardZone) ->
	{ok, Cards} = mnesia_play:get_player_data(PlayerPid, CardZone),
	find_card_option(Cards, PlayerPid, CardOrder, CardID).

add_card_to_zone (PlayerPid, CardOrder, CardID, CardOption, Zone) ->
	{ok, Cards} = mnesia_play:get_player_data (PlayerPid, Zone),
	ZoneUpdate = Cards ++ [{{PlayerPid, CardOrder, CardID}, CardOption}],
	mnesia_play:set_player_data (PlayerPid, Zone, ZoneUpdate).

update_card (PlayerPid, CardOrder, CardID, CardOption, Zone) ->
	update_card (PlayerPid, CardOrder, CardID, CardOption, Zone, PlayerPid).
update_card (PlayerPid, CardOrder, CardID, CardOption, Zone, OwnerZone) ->
	{ok, Cards} = mnesia_play:get_player_data (OwnerZone, Zone),
	ZoneUpdate = update (Cards, PlayerPid, CardOrder, CardID, CardOption),
	mnesia_play:set_player_data (OwnerZone, Zone, ZoneUpdate).

remove_card (PlayerPid, CardOrder, CardID, Zone) ->
	remove_card (PlayerPid, CardOrder, CardID, Zone, PlayerPid).
remove_card (PlayerPid, CardOrder, CardID, Zone, OwnerZone) ->
	{ok, Cards} = mnesia_play:get_player_data (OwnerZone, Zone),
	ZoneUpdate = remove (Cards, PlayerPid, CardOrder, CardID),
	mnesia_play:set_player_data (OwnerZone, Zone, ZoneUpdate).

add_card_status (CardOwner, CardOrder, CardID, StatusAdd) ->
	CardZone = check_card_zone (CardOwner, CardOrder, CardID),
	add_card_status (CardOwner, CardOrder, CardID, StatusAdd, CardZone).
add_card_status (CardOwner, CardOrder, CardID, StatusAdd, Zone) ->
	{CardType, Cards} = get_all_card (CardID, Zone),
	%smo_logger:fmsg("card on ~p are ~p~n", [Zone, Cards]),
	case find_card_option (Cards, CardOwner, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal ->
					UpdateOption = seal_card:set_seal_option (CardOption, card_status, StatusAdd),
					update_card (CardOwner, CardOrder, CardID, UpdateOption, Zone);
				is_not_seal ->
					UpdateOption = mystic_card:set_mystic_option (CardOption, card_status, StatusAdd),
					update_card (CardOwner, CardOrder, CardID, UpdateOption, Zone)
			end;
		{error, Reason} -> io:format("Can not add ~p to ~p : ~p ~p~n", [StatusAdd, {CardOwner, CardOrder, CardID}, Reason, Zone])
	end.

remove_card_status (PlayerPid, CardOrder, CardID, StatusRemove) ->
	Zone = check_card_zone (PlayerPid, CardOrder, CardID),
remove_card_status (PlayerPid, CardOrder, CardID, StatusRemove, Zone).
remove_card_status (PlayerPid, CardOrder, CardID, StatusRemove, Zone) ->
	{CardType, Cards} = get_all_card(CardID, Zone),
	case find_card_option (Cards, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal ->
					OptionUpdate = seal_card:remove_seal_status (CardOption, StatusRemove),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone);
				is_not_seal ->
					OptionUpdate = mystic_card:remove_mystic_status (CardOption, StatusRemove),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone)
			end;
		{error, Reason} ->
			io:format ("Get card option error from ~p~n", [Reason])
	end.

get_cards_from_multiple_zone (Zone) ->
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	get_cards (PlayerList, Zone).

get_cards ([], _) -> [];
get_cards ([{PlayerPid, _} | PlayerList], Zone) ->
	get_cards_form_zone (PlayerPid, Zone) ++ get_cards (PlayerList, Zone).

get_cards_form_zone (_, []) -> [];
get_cards_form_zone (PlayerPid, [Zone | Zones]) ->
	{ok, Cards} = mnesia_play:get_player_data (PlayerPid, Zone),
	Cards ++ get_cards_form_zone (PlayerPid, Zones).

get_all_card (Zone) ->
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	SealCards = get_player_cards (PlayerList, is_seal, Zone),
	MysticCards = get_player_cards (PlayerList, is_not_seal, Zone),
	{ok, SealCards ++ MysticCards}.

get_all_card (CardCheck, Zone) ->
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	case CardCheck of
		seal_card -> get_player_cards (PlayerList, is_seal, Zone);
		mystic_card -> get_player_cards (PlayerList, is_not_seal, Zone);
		_ ->	CardType = mnesia_odbc:is_seal_card (CardCheck),
			{CardType, get_player_cards (PlayerList, CardType, Zone)}
	end.

get_all_card (PlayerPid, CardCheck, Zone) ->
	case CardCheck of
		seal_card -> get_player_cards (PlayerPid, is_seal, Zone);
		mystic_card -> get_player_cards (PlayerPid, is_not_seal, Zone);
		_ ->	CardType = mnesia_odbc:is_seal_card (CardCheck),
			{CardType, get_player_cards (PlayerPid, CardType, Zone)}
	end.

get_cards_have_status ([], _) -> [];
get_cards_have_status ([{{PlayerPid, CardOrder, CardID}, _} | T], StatusCheck) ->
	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	case card_utility:check_card_status (PlayerPid, CardOrder, CardID, StatusCheck, Zone) of
		{ok, have_status} -> [{PlayerPid, CardOrder, CardID}] ++ get_cards_have_status (T, StatusCheck);
		{ok, have_no_status} -> get_cards_have_status (T, StatusCheck)
	end.

check_card_status (PlayerPid, CardOrder, CardID, StatusCheck) ->
	Zone = check_card_zone (PlayerPid, CardOrder, CardID),
	check_card_status (PlayerPid, CardOrder, CardID, StatusCheck, Zone).
check_card_status (PlayerPid, CardOrder, CardID, StatusCheck, Zone) ->
	{CardType, Cards} = get_all_card(CardID, Zone),
	case find_card_option(Cards, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal -> seal_card:check_card_status (CardOption, StatusCheck);
				is_not_seal -> mystic_card:check_card_status (CardOption, StatusCheck)
			end;
		{error, Reason} -> {error, Reason}
	end.
	
check_card_zone (PlayerPid, CardOrder, CardID) ->
	is_card_on_zone(PlayerPid, CardOrder, CardID, [hand_cards, arena_zone, shrine_cards, remove_cards, mystic_deck, seal_deck, support_cards]).

is_card_on_zone (PlayerPid, CardOrder, CardID, []) ->
	io:format("Card ~p no zone~n", [{PlayerPid, CardOrder, CardID}]),
	{error, zone_card_error};
is_card_on_zone (PlayerPid, CardOrder, CardID, [Zone | T]) ->
	{_, Cards} = get_all_card(CardID, Zone),
	case have_on_zone (Cards, PlayerPid, CardOrder, CardID) of
		on_zone -> Zone;
		not_on_zone -> is_card_on_zone (PlayerPid, CardOrder, CardID, T)
	end.

have_card_on_zone (PlayerPid, CardOrder, CardID, Zone) ->
	{_, Cards} = get_all_card(CardID, Zone),
	have_on_zone (Cards, PlayerPid, CardOrder, CardID).

get_card_option_field (PlayerPid, CardOrder, CardID, Field) ->
	Zone = check_card_zone (PlayerPid, CardOrder, CardID),
	get_card_option_field (PlayerPid, CardOrder, CardID, Field, Zone).
get_card_option_field (PlayerPid, CardOrder, CardID, Field, Zone) ->
	{CardType, Cards} = get_all_card (CardID, Zone),
	case find_card_option(Cards, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal -> seal_card:get_seal_option(CardOption, Field);
				is_not_seal -> mystic_card:get_mystic_option (CardOption, Field)
			end;
		{error, Reason} -> {error, Reason}
	end.

update_support_cards (CardOwner, CardOrder, CardID, Cards, Type) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			case get_card_option (CardOwner, CardOrder, CardID) of
				{ok, CardOption} ->
					 seal_card:update_support_seal (CardOption, Cards, Type);
				{error, Reason} ->
					{error, Reason}
			end;
		is_not_seal -> {error, mystic_can_not_combine}
	end.

get_card_combine_part (PlayerPid, CardOrder, CardID, Field) ->
	case get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone) of
		{ok, Combine} ->
			case mnesia_odbc:is_seal_card (CardID) of
				is_seal -> seal_card:get_combine_field (Combine, Field);
				is_not_seal -> {error, mystic_can_not_combine}
			end;
		{error, Reason} -> {error, Reason}
	end.

set_card_option_field (PlayerPid, CardOrder, CardID, Field, Data) ->
	Zone = check_card_zone (PlayerPid, CardOrder, CardID),
	set_card_option_field (PlayerPid, CardOrder, CardID, Field, Data, Zone).
set_card_option_field (PlayerPid, CardOrder, CardID, Field, Data, Zone) ->
	{CardType, Cards} = get_all_card(CardID, Zone),
	case find_card_option(Cards, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal ->
					OptionUpdate = seal_card:set_seal_option (CardOption, Field, Data),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone);
				is_not_seal ->
					OptionUpdate = mystic_card:set_mystic_option (CardOption, Field, Data),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone)
			end;
		{error, Reason} -> {error, Reason}
	end.

update_card_option_field (PlayerPid, CardOrder, CardID, Field, Data) ->
	Zone = check_card_zone (PlayerPid, CardOrder, CardID),
	update_card_option_field (PlayerPid, CardOrder, CardID, Field, Data, Zone).
update_card_option_field (PlayerPid, CardOrder, CardID, Field, Data, Zone) ->
	{CardType, Cards} = get_all_card(CardID, Zone),
	case find_card_option (Cards, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case CardType of
				is_seal ->
					OptionUpdate = seal_card:update_seal_option (CardOption, Field, Data),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone);
				is_not_seal ->
					OptionUpdate = mystic_card:update_mystic_option (CardOption, Field, Data),
					update_card (PlayerPid, CardOrder, CardID, OptionUpdate, Zone)
			end;
		{error, Reason} -> {error, Reason}
	end.

check_card_destroyed (PlayerPid) ->
	SealList = card_utility:get_all_card (seal_card, arena_zone),
	MysticList = card_utility:get_all_card (mystic_card, arena_zone),
	DestroyData = get_destroy_data (SealList ++ MysticList, destroyed),
	case lists:flatlength (DestroyData) of
		0 ->	interfere_step:return_play (check_play_step);
		_ ->	shrine_zone:card_to_shrine (PlayerPid, DestroyData)
	end.

get_destroy_data ([], _) -> [];
get_destroy_data ([{{PlayerPid, CardOrder, CardID}, _} | T], DestroyCheck) ->
	case card_utility:check_card_status (PlayerPid, CardOrder, CardID, DestroyCheck, arena_zone) of
		{ok, have_status} -> [{PlayerPid, CardOrder, CardID}] ++ get_destroy_data (T, DestroyCheck);
		{ok, have_no_status} -> get_destroy_data (T, DestroyCheck)
	end.

% check_card_affect () ->
	% check_card_affect ([]).
% 
% check_card_affect (ManualCardCheck) ->
	% {ok, Cards} = stack_pool:get_last_stack (self(), card_have_ability_check),
	% stack_pool:push_stack (self(), 0, 0, 0, [{play, play_ability_affect}, {card_zone, arena_zone}, {card_affect, Cards ++ ManualCardCheck}]),
	% stack_pool:remove_stack_option (self(), card_have_ability_check),
	% ability_effect:check_card_affect ().

change_card_zone (PlayerPid, CardOrder, CardID, FromZone, ToZone) ->
	change_card_zone (PlayerPid, CardOrder, CardID, FromZone, ToZone, []).
change_card_zone (PlayerPid, CardOrder, CardID, FromZone, ToZone, OptionAdded) ->
	case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, card_status, FromZone) of
		{ok, Result} -> CardStatus = Result;
		{error, _} -> CardStatus = []
	end,
	case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, FromZone) of
		{ok, RResult} -> RFx = RResult;
		{error, _} -> RFx = []
	end,
	% case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, FromZone) of
		% {ok, SResult} -> SFx = SResult;
		% {error, _} -> SFx = []
	% end,
	remove_card(PlayerPid, CardOrder, CardID, FromZone),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> CardOption = seal_card:set_create_option(CardID, OptionAdded ++ transform(CardStatus));
		is_not_seal -> CardOption = mystic_card:set_create_option(CardID, OptionAdded ++ transform(CardStatus))
	end,
	add_card_to_zone(PlayerPid, CardOrder, CardID, CardOption, ToZone),
	card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, RFx, arena_zone).
	%card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, SFx, arena_zone).

transform([]) -> [];
transform([Status | T]) -> [{card_status, Status}] ++ transform(T).

get_all_card_effect(PlayerPid, CardOrder, CardID) ->
	Zone = check_card_zone(PlayerPid, CardOrder, CardID),
	get_all_card_effect(PlayerPid, CardOrder, CardID, Zone).
get_all_card_effect(PlayerPid, CardOrder, CardID, Zone) ->
	case Zone of
		support_cards -> [];
		_ ->
			case get_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, Zone) of
				{ok, ReceiveFx} -> ReceiveFx;
				_ -> []
			end
	end.

check_card_active(PlayerPid, CardOrder, CardID) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> check_active_status(PlayerPid, CardOrder, CardID);
		is_not_seal -> inactive_seal
	end.

check_active_status(PlayerPid, CardOrder, CardID) ->
	CardZone = check_card_zone (PlayerPid, CardOrder, CardID),
	ActiveOption = get_card_option_field(PlayerPid, CardOrder, CardID, active, CardZone),
	case ActiveOption of
		{ok, inactive} -> inactive_seal;
		{ok, active} ->
			AllFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
			case function_utility:is_contain([{action, inactive}], AllFx) of
				[] -> active_seal;
				_ -> inactive_seal
			end
	end.

% check_card_controller (PlayerPid, CardOrder, CardID) - controller_allow เจ้าของเป็นคนเดียวกับผู้ควบคุม | controller_not_allow เจ้าของกับผู้ควมคุมเป็นคนละคนกัน
% check_card_controller (CardOwner, CardOrder, CardID) ->
	% CardZone = check_card_zone (CardOwner, CardOrder, CardID),
	% check_card_controller (CardOwner, CardOrder, CardID, CardZone).
% 
% check_card_controller (CardOwner, CardOrder, CardID, CardZone) ->
	% CardFx = card_utility:get_all_card_effect (CardOwner, CardOrder, CardID, CardZone),
	% check_card_controller (CardOwner, CardFx).
% 
% check_card_controller (_, []) -> controller_allow;
% check_card_controller (CardOwner, [{_, Fx, _} | CardFx]) ->
	% case has_chram_curse (Fx) of
		% {have_curse, CardOwner} ->
			% controller_allow;
		% {have_curse, _} ->
			% controller_not_allow;
		% no_curse ->
			% check_card_controller (CardOwner, CardFx)
	% end.

% has_chram_curse ([]) -> no_curse;
% has_chram_curse ([{curse, {charm_curse, Controller}} | _]) -> {have_curse, Controller};
% has_chram_curse ([_ | Fx]) -> has_chram_curse (Fx).

check_card_effect_duration ([], _, _) -> [];
check_card_effect_duration ([{CardOwner, CardOrder, CardID} | Cards], FxCheck, DurationCheck) ->
	CardFx = get_all_card_effect (CardOwner, CardOrder, CardID),
	case get_card_effect_duration (CardFx, FxCheck, DurationCheck) of
		{ok, have_duration_effect} ->
			[{CardOwner, CardOrder, CardID}] ++ check_card_effect_duration (Cards, FxCheck, DurationCheck);
		{ok, no_duration_effect} ->
			check_card_effect_duration (Cards, FxCheck, DurationCheck)
	end.

get_card_effect_duration ([], _, _) -> {ok, no_duration_effect};
get_card_effect_duration ([{_, Fx, Duration} | CardFx], FxCheck, Duration) ->
	io:format ("Fx ~p Duration ~p FxCheck ~p~n", [Fx, Duration, FxCheck]),
	case FxCheck -- Fx of
		[] ->	{ok, have_duration_effect};
		_ ->	get_card_effect_duration (CardFx, FxCheck, Duration)
	end;
get_card_effect_duration ([_ | CardFx], FxCheck, Duration) ->
	io:format ("FxCheck ~p Duration ~p~n", [FxCheck, Duration]),
	get_card_effect_duration (CardFx, FxCheck, Duration).

% remove_mystic_effect_find_target (CardOwner, CardOrder, CardID) ->
	% {ok, Target} = get_card_option_field (CardOwner, CardOrder, CardID, paste_to),
	% remove_mystic_effect_from_target ({CardOwner, CardOrder, CardID}, Target).
% 
% remove_mystic_effect_from_target (_, []) -> [];
% remove_mystic_effect_from_target (MData, [{CardOwner, CardOrder, CardID} | Cards]) ->
	% CardZone = check_card_zone (CardOwner, CardOrder, CardID),
	% {ok, Mystic} = get_card_option_field (CardOwner, CardOrder, CardID, mystic, CardZone),
	% update_card_option_field (CardOwner, CardOrder, CardID, mystic, Mystic -- [MData], CardZone),
	% remove_mystic_effect (CardOwner, CardOrder, CardID, MData, [receive_effect, skill_effect], CardZone),
	% check_remove_other_effect_type (MData, CardOwner, CardOrder, CardID, CardZone),
	% effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [], update), %เช็ค update info card
	% remove_mystic_effect_from_target (MData, Cards).
% 
% check_remove_other_effect_type ({MOw, MOr, 356}, CardOwner, CardOrder, CardID, CardZone) ->
	% case card_utility:check_card_status (CardOwner, CardOrder, CardID, combine_with_effect, CardZone) of
		% {ok, have_status} ->
			% CardOption = card_utility:update_support_cards (CardOwner, CardOrder, CardID, [{MOw, MOr, 356}], remove_support),
			% card_utility:update_card (CardOwner, CardOrder, CardID, CardOption, CardZone),
			% case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, combine, CardZone) of
				% [] -> gen_server:cast(self(), {card_break_combine, CardOwner, CardOrder, CardID});
				% _ ->	no_action
			% end;
		% {ok, have_no_status} ->
			% no_action
	% end;
% check_remove_other_effect_type (_, _, _, _, _) -> ok.

% remove_mystic_effect (CardOwner, CardOrder, CardID, MData, Effect, CardZone) ->
	% lists:foreach(fun(Field) ->
		% {ok, CardFx} = get_card_option_field(CardOwner, CardOrder, CardID, Field, CardZone),
		% FieldUpdate = remove_mystic_effect_from_data(CardOwner, CardOrder, CardID, CardFx, MData),
		% update_card_option_field(CardOwner, CardOrder, CardID, Field, FieldUpdate, CardZone),
	% end, Effect).

% remove_mystic_effect_from_data (_, _, _, [], _) -> [];
% remove_mystic_effect_from_data (TargetOwner, TargetOrder, TargetID, [{Mdata, Fx, depend_on_s} | CardFx], Mdata) ->
	% effect_activate:send_update_activate_effect(TargetOwner, TargetOrder, TargetID, [{Mdata, Fx}], remove),
	% remove_mystic_effect_from_data (TargetOwner, TargetOrder, TargetID, CardFx, Mdata);
% remove_mystic_effect_from_data (TargetOwner, TargetOrder, TargetID, [Fx | CardFx], Mdata) ->
	% [Fx] ++ remove_mystic_effect_from_data (TargetOwner, TargetOrder, TargetID, CardFx, Mdata).
	
get_support_seal([]) -> [];
get_support_seal([{CardOwner, CardOrder, CardID} | T]) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
				arena_zone ->
					{ok, Support, _MysticSupport} = arena_zone:break_support_seal(CardOwner, CardOrder, CardID),
					% %smo_logger:fmsg("###get supprot seal then Support main seal and support seal are ~p~n", [Support]),
					SupportNoData = exclude_support_data(Support),
					% %smo_logger:fmsg("####support with main are ~p~n", [SupportNoData]),
					SupportNoMain = remove_main_seal(SupportNoData, CardOwner, CardOrder, CardID),
					% %smo_logger:fmsg("Support without main seal is ~p~n", [SupportNoMain]),
					SupportNoMain++ get_support_seal(T);
				_ ->	get_support_seal(T)
			end;
		is_not_seal ->	get_support_seal(T)
	end.
	
remove_main_seal([], _, _, _) -> [];
remove_main_seal([{CardOwner, CardOrder, CardID}|Support], CardOwner, CardOrder, CardID) ->
	remove_main_seal(Support, CardOwner, CardOrder, CardID);
% remove_main_seal([{{PlayerPid, CardOrder, CardID}, _} | Support], PlayerPid, CardOrder, CardID) ->
	% remove_main_seal(Support, PlayerPid, CardOrder, CardID);
remove_main_seal([Card | Support], CardOwner, CardOrder, CardID) ->
	[Card] ++ remove_main_seal(Support, CardOwner, CardOrder, CardID).

exclude_support_data([]) -> [];
exclude_support_data([{{CardOwner, CardOrder, CardID}, _} | T]) ->
	[{CardOwner, CardOrder, CardID}] ++ exclude_support_data(T);
exclude_support_data([{CardOwner, CardOrder, CardID} | T]) ->
	[{CardOwner, CardOrder, CardID}] ++ exclude_support_data(T).