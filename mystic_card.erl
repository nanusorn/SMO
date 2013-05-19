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
-module (mystic_card).

-import (mnesia_table, [do/1]).
-include_lib ("stdlib/include/qlc.hrl").
-include ("record.hrl").
-import (lists, [flatlength/1]).

-compile(export_all).

%% mystic_card_option = {card_status, duration, paste_card, give_effect, receive_effect, skill_effect}
%% %% duration มีตัวเลขแสดงเป็นจำนวน sub-turn
get_default_option() ->  {[], 0, 0, [], [], [], [{card_zone, mystic_deck}]}.

get_default_option(CardID) ->
	[{Name, Type, Subtype, PasteType, CardNaming, Interfere, Mc, Duration}] = get_msytic_default_information(CardID),
	{[{card_name, Name}, {card_type, Type}, {card_subtype, Subtype}, {paste_type, PasteType}, {card_naming, CardNaming}, {interfere, Interfere}, {mp_cast, Mc}, {duration, Duration}], 
	[], 0, 0, [], [], [], [{card_zone, mystic_deck}]}.

get_msytic_default_information(CardID) ->	
	do(qlc:q([{ 
					X#mystic_card.card_name, 
					X#mystic_card.card_type,
					X#mystic_card.card_subtype,
					X#mystic_card.paste_type,
					X#mystic_card.card_naming,
					X#mystic_card.is_interfere, 
					X#mystic_card.mp_cast, 
					X#mystic_card.duration} || X <- mnesia:table(mystic_card), X#mystic_card.card_id =:= CardID])).

create_option (Option) ->
	create_option (get_default_option (), Option).

set_create_option(CardID, Option) ->
	create_option(get_default_option(CardID), Option).

create_option (CardOption, []) -> CardOption;
create_option (CardOption, [{OptionField, OptionData}|T]) -> create_option (set_mystic_option (CardOption, OptionField, OptionData), T).

% give_effect = {effect_target_id, effect, effect_option}
get_mystic_option ({Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options}, Field) ->
	case Field of
		information -> {ok, Information};
		card_status -> {ok, CardStatus};
		duration -> {ok, Duration};
		paste_to -> {ok, PasteTo};
		give_effect -> {ok, GiveEffect};
		receive_effect -> {ok, ReceiveEffect};
		skill_effect -> {ok, SkillFx};
		_ -> get_other_option (Field, Options)
	end.

get_other_option (_, []) -> {error, no_option_field};
get_other_option (GetOption, [{GetOption, Value} | _]) -> {ok, Value};
get_other_option (GetOption, [_ | Option]) -> get_other_option (GetOption, Option).

set_mystic_option ({Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options}, OptionField, OptionData) ->
	case OptionField of
		card_status ->
			{Information, set_status_list (CardStatus, OptionData, []), Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options};
		duration ->
			{Information, CardStatus, OptionData, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options};
		paste_to ->
			{Information, CardStatus, Duration, OptionData, GiveEffect, ReceiveEffect, SkillFx, Options};
		give_effect ->
			{Information, CardStatus, Duration, PasteTo, GiveEffect ++ OptionData, ReceiveEffect, SkillFx, Options};
		receive_effect ->
			{Information, CardStatus, Duration, PasteTo, GiveEffect, OptionData, SkillFx, Options};
		skill_effect ->
			{Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, OptionData, Options};
		card_zone ->
			OptionUpdate = update_option_field (OptionField, OptionData, Options),
			{Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, OptionUpdate};
		_ ->	OptionUpdate = add_option_field (OptionField, OptionData, Options),
			{Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, OptionUpdate}
	end.

add_option_field (OptionField, OptionData, []) -> [{OptionField, OptionData}];
add_option_field (OptionField, OptionData, [{OptionField, DataList} | Option]) ->
	DataAdd = OptionData -- DataList,
	[{OptionField, DataList ++ DataAdd}] ++ Option;
add_option_field (OptionField, OptionData, [Op | Option]) ->
	[Op] ++ add_option_field (OptionField, OptionData, Option).

% ไว้สำหรับใส่ค่าต่างๆลงไปโดยไม่สนใจ ค่าเก่าที่มีอยู่
update_mystic_option ({Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options}, OptionField, OptionData) ->
	case OptionField of
		card_status -> {Information, OptionData, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options};
		duration -> {Information, CardStatus, OptionData, PasteTo, GiveEffect, ReceiveEffect, SkillFx, Options};
		paste_to -> {Information, CardStatus, Duration, OptionData, GiveEffect, ReceiveEffect, SkillFx, Options};
		give_effect -> {Information, CardStatus, Duration, PasteTo, OptionData, ReceiveEffect, SkillFx, Options};
		receive_effect -> {Information, CardStatus, Duration, PasteTo, GiveEffect, OptionData, SkillFx, Options};
		skill_effect -> {Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, OptionData, Options};
		_ ->	OptionUpdate = update_option_field (OptionField, OptionData, Options),
			{Information, CardStatus, Duration, PasteTo, GiveEffect, ReceiveEffect, SkillFx, OptionUpdate}
	end.

update_option_field (OptionField, OptionData, []) -> [{OptionField, OptionData}];
update_option_field (OptionField, OptionData, [{OptionField, _} | Options]) ->
	[{OptionField, OptionData}] ++ Options;
update_option_field (OptionField, OptionData, [Op | Options]) ->
	[Op] ++ update_option_field (OptionField, OptionData, Options).

set_status_list ([], OptionData, StatusList) -> StatusList ++ [OptionData];
set_status_list ([OptionData|T], OptionData, StatusList) -> StatusList ++ [OptionData] ++ T;
set_status_list ([H|T], OptionData, StatusList) -> set_status_list (T, OptionData, StatusList ++ [H]).

remove_mystic_status (CardOption, Status) ->
	{ok, CardStatus} = get_mystic_option (CardOption, card_status),
	CardStatusUpdate = remove_status (CardStatus, Status),
	update_mystic_option (CardOption, card_status, CardStatusUpdate).

remove_status ([], _) -> [];
remove_status ([Status | CardStatus], Status) -> CardStatus;
remove_status ([Other | CardStatus], Status) ->
	[Other] ++ remove_status (CardStatus, Status).

check_card_status (CardOption, StatusCheck) ->
	case get_mystic_option (CardOption, card_status) of
		{ok, CardStatus} ->
			check_status (CardStatus, StatusCheck);
		{error, _} ->
			{error, no_option}
	end.

check_status ([], _) -> {ok, have_no_status};
check_status ([StatusCheck|_], StatusCheck) -> {ok, have_status};
check_status ([_|T], StatusCheck) -> check_status (T, StatusCheck).

destroy([], _) -> [];
destroy([{CancelPid, CancelOrder, CancelID}|CardCancel], DestroyType) ->
	destroy(CancelPid, CancelOrder, CancelID, DestroyType) ++ destroy(CardCancel, DestroyType).
	% lists:foreach(
								% fun({PlayerPid, CardOrder, CardID}) ->
									% destroy(PlayerPid, CardOrder, CardID, DestroyType)
								% end, Card).

% ทำลายมิสติกที่โดน ยกเลิก มิสติก
destroy(PlayerPid, CardOrder, CardID, DestroyType) ->
	MysticPasted = arena_zone:get_mystic_pasted(PlayerPid, CardOrder, CardID),
%	io:format("-- Mystic pasted is ~p~n", [MysticPasted]),
	case DestroyType of
		all_mystic -> set_destroyed_status(MysticPasted), MysticPasted;
		all_opponent ->
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			MysticDestroy = set_destroyed_status(OpponentPid, MysticPasted, all);
		opponent_relic ->
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			MysticDestroy = set_destroyed_status(OpponentPid, MysticPasted, relic);
		self -> []
	end.

set_destroyed_status(DestroyList) ->
	lists:foreach(
								fun({CardOwner, CardOrder, CardID}) ->
									stack_pool:add_stack_option_field(self(), cards_destroy, [{CardOwner, CardOrder, CardID}])
								end, DestroyList).
	

set_destroyed_status(_, [], _) -> [];
set_destroyed_status(PlayerPid, [{CardOwner, CardOrder, CardID}|DestroyList], DestroyType) ->
	case CardOwner of
		PlayerPid -> 
			case DestroyType of
				all -> 
					%stack_pool:add_stack_option_field(self(), cards_destroy, [{CardOwner, CardOrder, CardID}]),
					[{CardOwner, CardOrder, CardID}] ++ set_destroyed_status(PlayerPid, DestroyList, DestroyType);
				relic -> 
					case mnesia_odbc:get_mystic_data(CardID, card_type) of
						{ok, 2} -> 
							%stack_pool:add_stack_option_field(self(), cards_destroy, [{CardOwner, CardOrder, CardID}]);
							[{CardOwner, CardOrder, CardID}] ++ set_destroyed_status(PlayerPid, DestroyList, DestroyType);
						_ -> set_destroyed_status(PlayerPid, DestroyList, DestroyType)
					end
			end;
		_ -> set_destroyed_status(PlayerPid, DestroyList, DestroyType)
	end.
	
get_power_type(_, []) -> {error, 0};	
get_power_type(PowerType, [{PowerType, Power}|_]) -> {ok, Power};
get_power_type(PowerType, [_|CardInfo]) -> get_power_type(PowerType, CardInfo).

paste_to_target(PlayerPid, CardOrder, CardID, TPid, TOrder, Tid) ->
	MysticZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
	Zone = card_utility:check_card_zone(TPid, TOrder, Tid),
	card_utility:set_card_option_field(PlayerPid, CardOrder, CardID, paste_to, [{TPid, TOrder, Tid}], MysticZone),
	card_utility:set_card_option_field(TPid, TOrder, Tid, mystic, [{PlayerPid, CardOrder, CardID}], Zone),
	{ok, mystic_pasted, TPid, TOrder, Tid}.
% % ========= CASTING MYSTIC ===============
% check_paste_mystic_to_target (PlayerPid, CardOrder, CardID) ->
	% {ok, UsingType} = mnesia_odbc:get_mystic_data (CardID, paste_type),
% %	io:format("Using type ~p~n", [UsingType]),
	% case UsingType of
		% 1 -> check_paste_on_seal (PlayerPid, CardOrder, CardID);
		% 2 -> {ok, paste_on_arena};%check_paste_on_arena (PlayerPid, CardOrder, CardID);
		% 3 -> {ok, paste_on_mystic};%check_paste_on_mystic (PlayerPid, CardOrder, CardID);
		% 4 -> {ok, activate_effect_and_move_to_shrine};
		% _ -> io:format("Using type ~p~n", [UsingType])
	% end.
% 
% check_paste_on_seal (PlayerPid, CardOrder, CardID) ->
	% case mystic_effect:get_mystic_target() of
		% [{TPid, TOrder, Tid}] ->
			% verify_seal_ability(PlayerPid, CardOrder, CardID, TPid, TOrder, Tid);
		% Any ->
			% io:format ("no target return ~p~n", [Any])
	% end.
% 
% verify_seal_ability (PlayerPid, CardOrder, CardID, TPid, TOrder, Tid) ->
	% case ability_effect:check_card_effect (TPid, TOrder, Tid, cancel_all_mystic) of
		% {ok, have_effect} -> {ok, cancel_mystic, TPid, TOrder, Tid};
		% {ok, no_effect} ->
			% case ability_effect:check_card_effect (TPid, TOrder, Tid, cancel_opponent_mystic) of
				% {ok, have_effect} ->
					% case PlayerPid of
						% TPid -> paste_to_target (PlayerPid, CardOrder, CardID, TPid, TOrder, Tid);
						% _ ->	{ok, cancel_mystic, TPid, TOrder, Tid}
					% end;
				% {ok, no_effect} -> paste_to_target (PlayerPid, CardOrder, CardID, TPid, TOrder, Tid)
			% end
	% end.

% activate_mystic_effect(TPid, TOrder, Tid) ->
	% {ok, CardFx} = stack_pool:get_last_stack (self(), card_fx),
	% stack_pool:set_stack_option (self(), mystic_target, [{TPid, TOrder, Tid}]),
	% stack_pool:set_stack_option (self(), mystic_effect_list, CardFx),
	% check_activate_mystic_effect ().

% activate_mystic_effect (PlayerPid, CardOrder, CardID, TargetList) ->
	% {ok, CardFx} = stack_pool:get_last_stack (self(), card_fx),
	% case stack_pool:get_last_stack (self(), random_target) of
		% {ok, _} ->
			% TargetFx = random_target (TargetList);
		% {error, _} ->
			% TargetFx = TargetList
	% end,
	% {ok, Player} = stack_pool:get_last_stack(self(), card_player),
	% stack_pool:push_stack (self(), PlayerPid, CardOrder, CardID, [{play, play_activate_mystic_effect}, {card_player, Player}]),
	% stack_pool:set_stack_option (self(), mystic_target_list, TargetFx),
	% stack_pool:set_stack_option (self(), card_fx, CardFx),
	% interfere_step:return_play (check_play_step).

% add_effect_to_target () ->
	% {ok, MysticTarget} = stack_pool:get_last_stack (self(), mystic_target_list),
% %	io:format ("Mystic target ~p~n", [MysticTarget]),
	% case MysticTarget of
		% [] ->	case stack_pool:get_last_stack (self(), target_ability_activated) of
				% {ok, {PlayerPid, CardOrder, CardID}} ->
					% effect_activate:send_update_activate_effect (PlayerPid, CardOrder, CardID, [], add);
				% _ ->	no_target_activate
			% end,
			% interfere_step:return_play ();
		% [{TPid, TOrder, TCid} | TargetList] ->
			% stack_pool:set_stack_option (self(), mystic_target_list, TargetList),			
			% stack_pool:set_stack_option (self(), target_ability_activated, {TPid, TOrder, TCid}),
			% activate_mystic_effect (TPid, TOrder, TCid)
	% end.

% get_effect_list ([]) -> [];
% get_effect_list ([{_, Fx, _} | T]) -> Fx ++ get_effect_list (T).

% random_target (TargetList) ->
	% RandomNumber = random:uniform (50) + 50,
	% CardsSize = lists:flatlength (TargetList),
	% find_card_random (RandomNumber, CardsSize, TargetList).

% find_card_random (RandomNumber, CardsSize, TargetList) ->
% %	io:format ("Random number ~p, Card size ~p, Target ~p~n", [RandomNumber, CardsSize, TargetList]),
	% if
		% RandomNumber >= CardsSize ->
			% find_card_random (RandomNumber - CardsSize, CardsSize, TargetList);
		% true ->
			% get_card_data (TargetList, RandomNumber)
	% end.

% get_card_data ([{PlayerPid, CardOrder, CardID} | _], 0) ->
	% [{PlayerPid, CardOrder, CardID}];
% get_card_data ([_ | Cards], RandomNumber) ->
	% get_card_data (Cards, RandomNumber - 1).

% check_activate_mystic_effect () ->
	% case stack_pool:get_last_stack (self(), mystic_effect_list) of
		% {ok, []} ->
			% interfere_step:return_play (check_play_step);
		% {ok, [{_, [], _} | FxList]} ->
			% stack_pool:set_stack_option (self(), mystic_effect_list, FxList),
			% interfere_step:return_play (check_play_step);
		% {ok, [{GFx, [FxCheck | Fx], Duration} | FxList]} ->
			% stack_pool:set_stack_option (self(), mystic_effect_list, [{GFx, Fx, Duration}] ++ FxList),
			% {ok, TargetCard} = stack_pool:get_last_stack (self(), mystic_target),
			% CardFx = [{GFx, [FxCheck], Duration}],
			% case FxCheck of
				% {curse, _} ->
					% [{TPid, TOrder, Tid}] = TargetCard;
				% _ ->	[{TPid, TOrder, Tid}] = TargetCard,
					% card_utility:set_card_option_field (TPid, TOrder, Tid, receive_effect, CardFx)
			% end,
			% case FxCheck of
				% {combine, double_combine_when_not_combine} ->
					% {ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
					% mystic_effect:check_double_combine_effect (CardOwner, CardOrder, CardID, TPid, TOrder, Tid);
				% {action, Action} ->
					% mystic_effect:set_action_effect (TargetCard, Action);
				% {heal_curse, Curse} ->
					% curse_activation:heal_target_curse(TargetCard, Curse);
				% {curse, _} ->
					% curse_activation:curse_activation_assign (TargetCard, CardFx, receive_effect);
				% _ ->	io:format ("
				% ~p out of range ~n", [FxCheck]),
					% send_update_card_effect (TargetCard, FxCheck),
					% check_activate_mystic_effect ()
			% end;
		% {error, _} -> interfere_step:return_play (check_play_step)
	% end.

% send_update_card_effect ([], _) -> ok;
% send_update_card_effect ([{PlayerPid, CardOrder, CardID} | Cards], Fx) ->
	% effect_activate:send_update_activate_effect (PlayerPid, CardOrder, CardID, [Fx], add),
	% send_update_card_effect (Cards, Fx).

% activate_effect_to_player (PlayerPid, CardOrder, CardID, add_effect_on_card) ->
	% {ok, TargetFx} = stack_pool:get_last_stack (self(), effect_to_player),
	% {ok, AbilityID} = stack_pool:get_last_stack (self(), mystic_ability_id),
	% {ok, [{_, Fx, Duration}]} = stack_pool:get_last_stack (self(), card_fx),

	% CardAbility = {[], Fx, TargetFx, AbilityID, Duration},
	% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
% 
	% {ok, OldAbility} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Zone),
	% AbilityUpdate = check_add_ability (OldAbility, CardAbility),
	% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, give_effect, AbilityUpdate, Zone),
	% mystic_effect:activate_player_effect (PlayerPid, CardOrder, CardID, Fx, AbilityID, Duration);
	
% activate_effect_to_player (_OwnerPid, _CardOrder, _CardID, activate_only) ->
	% {ok, [{_, Fx, _}]} = stack_pool:get_last_stack (self(), card_fx),
	% stack_pool:set_stack_option (self(), activate_effect_verify, Fx),
	% stack_pool:set_stack_option (self(), play, play_activate_effect_to_player),
	% interfere_step:return_play (check_play_step).

% verify_effect_mystic () ->
	% case stack_pool:get_last_stack (self(), activate_effect_verify) of
		% {ok, []} ->
			% stack_pool:set_stack_option (self(), play, casting_mystic_card_11),
			% interfere_step:return_play (check_play_step);
		% {ok, [Fx | AbiFx]} ->
			% stack_pool:set_stack_option (self(), activate_effect_verify, AbiFx),
			% activate_ability_affect (Fx)
	% end.	

% activate_ability_affect ({action, Action}) ->
	% case Action of
		% all_player_discard_mystic ->
			% {ok, PlayerTurn} = mnesia_play:get_game_data (self(), player_turn),
			% OpponentPid = mnesia_play:get_opponent_pid (PlayerTurn),
% 
			% PlayerNumHand = hand_zone:check_card_size (PlayerTurn, is_not_seal),
			% stack_pool:set_stack_option (self(), player_discard_number, PlayerNumHand),
% 
			% OpponentNumHand = hand_zone:check_card_size (OpponentPid, is_not_seal),
			% stack_pool:set_stack_option (self(), opponent_discard_number, OpponentNumHand),
% 
			% if PlayerNumHand > 0 ->
				% stack_pool:push_stack (self(), OpponentPid, 0, 0, [{play, defend_subturn_discard}, {discard_type, mystic_card}]);
			   % true -> player_no_card_to_discard
			% end,
% 
			% if OpponentNumHand > 0 ->
				% stack_pool:push_stack (self(), PlayerTurn, 0, 0, [{play, attack_subturn_discard}, {discard_type, mystic_card}]);
			   % true -> opponent_no_card_to_discard
			% end,
% 
			% interfere_step:return_play (check_play_step);
		% _ ->	io:format ("Action mystic effect ~p~n", [Action])
	% end;
% activate_ability_affect ({draw_mystic, Action}) ->
	% case Action of
		% at_least1_not_over_dis ->
			% {ok, PlayerTurn} = mnesia_play:get_game_data (self(), player_turn),
			% OpponentPid = mnesia_play:get_opponent_pid (PlayerTurn),
			% {ok, PlayerDrawCard} = stack_pool:get_last_stack (self(), player_discard_number),
			% {ok, OpponentDrawCard} = stack_pool:get_last_stack (self(), opponent_discard_number),
% 
			% DrawOption = [{draw_type, mystic_card_upto}, {current_draw, 0}, {draw_card, mystic_card}],
			% case 	stack_pool:get_last_stack (self(), target_player) of
				% {ok, player} ->
					% stack_pool:push_stack (self(), PlayerTurn, 0, 0, [{play, attack_subturn_draw}, {max_draw, PlayerDrawCard}] ++ DrawOption);
				% {ok, opponent} ->
					% stack_pool:push_stack (self(), OpponentPid, 0, 0, [{play, defend_subturn_draw}, {max_draw, OpponentDrawCard}] ++ DrawOption)
			% end,
% 
			% interfere_step:return_play (check_play_step);
		% DrawAmount ->
			% {ok, PlayerTurn} = mnesia_play:get_game_data (self(), player_turn),
			% OpponentPid = mnesia_play:get_opponent_pid (PlayerTurn),
			% draw_card:draw(OpponentPid, is_not_seal, DrawAmount)
	% end;
% activate_ability_affect ({mp_max, Value}) ->
	% set_mystic_ability_affect ({mp_max, Value}),
	% interfere_step:return_play (check_play_step);	
% activate_ability_affect ({hand_max, Value}) ->
	% set_mystic_ability_affect ({hand_max, Value}),
	% interfere_step:return_play (check_play_step);
% activate_ability_affect (Fx) ->
	% {ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack (self()),
	% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% case stack_pool:get_last_stack (self(), target_player) of
		% {ok, player} -> TargetPid = PlayerPid;
		% {ok, opponent} -> TargetPid = OpponentPid;
		% _ ->	io:format ("Fx doesn't target to player~n"),
			% TargetPid = 0
	% end,
	% case Fx of
		% {mp, CheckValue} ->
			% Value = ability_activate:check_value_data (PlayerPid, 0, 0, CheckValue),
			% set_effect (TargetPid, mp_rest, Value),
			% {ok, SelfMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
			% {ok, OpponentMp} = mnesia_play:get_player_data(OpponentPid, mp_rest),
			% effect_activate:send_update_other_data (PlayerPid, mp, [SelfMp, OpponentMp]),
			% effect_activate:send_update_other_data (OpponentPid, mp, [OpponentMp, SelfMp]);			
		% _ ->	io:format ("Other mystic effect ~p~n", [Fx])
	% end,
	% interfere_step:return_play (check_play_step).

% set_mystic_ability_affect (Fx) ->
% %	io:format ("Last stack ~p~n", [stack_pool:get_last_stack (self())]),
	% case stack_pool:get_last_stack (self(), card_fx) of
		% {ok, [{{CardOwner, CardOrder, CardID}, _, Duration}]} ->
			% {ok, RFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, receive_effect),
			% effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [Fx], add),
			% card_utility:update_card_option_field (CardOwner, CardOrder, CardID, receive_effect, RFx ++ [{{CardOwner, CardOrder, CardID}, [Fx], Duration}]);
		% _ ->	io:format ("Card fx out of bound~n")
	% end.

% set_effect (Receiver, PlayerField, Value) ->
	% {ok, PlayerValue} = mnesia_play:get_player_data (Receiver, PlayerField),
	% if
		% PlayerValue + Value < 0 ->
			% mnesia_play:set_player_data (Receiver, PlayerField, 0);
		% true ->
			% mnesia_play:set_player_data (Receiver, PlayerField, PlayerValue + Value)
	% end.

% player_select_mystic_effect (PlayerPid, FxId, FxSelect) ->
	% {ok, {PlayerPid, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
	% case FxId of
		% 1 -> mystic_effect:combination_with_select_element (PlayerPid, CardOrder, CardID, FxId, FxSelect)
	% end.

% check_add_ability ([], Ability) -> [Ability];
% check_add_ability ([{_, _, _, AbilityID, _} | T], {ConditionAbility, Effect, TargetType, AbilityID, Duration}) ->
	% [{ConditionAbility, Effect, TargetType, AbilityID, Duration}] ++ T;
% check_add_ability ([H | T], Ability) -> [H] ++ check_add_ability (T, Ability).

% ----------------------------------------------------------------------
% move_mystic_to_shrine (PlayerPid, CardOrder, CardID) ->
	% stack_pool:set_stack_option (self(), play, destroyed_mystic_card),
	% stack_pool:push_stack (self(), PlayerPid, CardOrder, CardID, [{play, destroyed}, {card_zone, arena_zone}, {card_destroyed, [{PlayerPid, CardOrder, CardID}]}]),
	% destroy:check_card_destroyed ().

% act_select_effect (ActivateID) ->
	% case ActivateID of
		% select_element_for_support -> gen_server:cast(self(), {act_select_effect, 1}) %%% <-- [McDuck] unknown process, check this out later
	% end.

% select_mystic_effect (PlayerPid, CardOrder, CardID, EffectID, Select) ->
	% case EffectID of
		% 1 -> mystic_effect:combination_with_select_element (PlayerPid, CardOrder, CardID, EffectID, Select)
	% end.

% activate_mystic_to_shrine () ->
	% {ok, {PlayerPid, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
	% shrine_zone:card_to_shrine (PlayerPid, CardOrder, CardID, arena_zone).

% destroyed_mystic_resume(PlayerPid, CardOrder, CardID) ->
	% gen_server:cast(self(), {update_mystic_to_shrine, PlayerPid, CardOrder, CardID}),
	% stack_pool:pop_stack_out(self()),
	% case stack_pool:get_last_stack (self(), play) of
		% {ok, PlayResume}  -> interfere_step:return_play (PlayResume);
		% _ -> stack_empty
	% end.
