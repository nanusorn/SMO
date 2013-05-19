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
-module (mnesia_play).

-include_lib ("stdlib/include/qlc.hrl").
-include ("play_record.hrl").

-import (mnesia_table, [do/1]).
-import (lists, [foreach/2, sum/1, append/2, min/1]).

-compile (export_all).

q_transaction (Row) ->
	F = fun() ->
			mnesia:write(Row)
		end,
	mnesia:transaction(F).

remove_game_data (RoomPid) ->
	Oid = {game_data, RoomPid},
	F = fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

reply_join_room(OwnerPid, OwnerName, JoinerPid, JoinerName, RoomName, HeaderReply, JoinFunction, SubTime) ->
	PguardPid = get(smo_player_guardian),
	case gen_server:call(PguardPid, {get_user_data_pid, JoinerPid, [player_name_utf, avatar]}) of
	%case lib_lobby_protocol:get_user_data_pid (JoinerPid, [player_name_utf, avatar]) of
		[PlayerName, {AvatarSet, AvatarID}] ->
			PlayerJoinName = PlayerName,
			JoinerAvatarSet = AvatarSet,
			JoinerAvatarID = AvatarID;
		_ ->	
			smo_logger:fmsg("Can not find joiner name ~w", [JoinerName]),
			PlayerJoinName = " ",
			JoinerAvatarSet = 0,
			JoinerAvatarID = 0
	end,
	case gen_server:call(PguardPid, {get_user_data_pid, OwnerPid, [player_name_utf, avatar]}) of
	%case lib_lobby_protocol:get_user_data_pid (OwnerPid, [player_name_utf, avatar]) of
		[PlayerOName, {OAvatarSet, OAvatarID}] ->
			PlayerOwnName = PlayerOName,
			OwnerAvatarSet = OAvatarSet,
			OwnerAvatarID = OAvatarID;
		_ ->	
			smo_logger:fmsg("Can not find owner name ~w", [OwnerName]),
			PlayerOwnName = " ",
			OwnerAvatarSet = 0,
			OwnerAvatarID = 0
	end,
	OwnerReply = [16#81, 1, string:len(PlayerJoinName), PlayerJoinName, JoinerAvatarSet, JoinerAvatarID],
	JoinerReply = [16#01, JoinFunction, HeaderReply, string:len(RoomName), RoomName,
		string:len(PlayerOwnName), PlayerOwnName, OwnerAvatarSet, OwnerAvatarID, string:len(PlayerJoinName), PlayerJoinName, SubTime],
	%%smo_logger:fmsg("OwnerReply contain = ~w and OwnerPid = ~w",[OwnerReply, OwnerPid]),
	%%smo_logger:fmsg("JoinerReply contain = ~w and JoinerPid = ~w",[JoinerReply, JoinerPid]),
	gen_server:cast(OwnerPid, {send, OwnerReply}),
	gen_server:cast(JoinerPid, {send, JoinerReply}).

% GameData = {game_state, player_turn_pid, seal_checkup, max_mp, max_shrine, player_list, log_process_id}
set_game_data(RoomPID, GameState, PlayerTurnPID, SealCheckUp, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi) ->
	Row = #game_data{	room_pid		= RoomPID,
					game_state	= GameState,
					player_turn_pid	= PlayerTurnPID,
					seal_check_up	= SealCheckUp,
					max_mp		= MaxMp,
					max_shrine	= MaxShrine,
					player_list		= PlayerList,
					log_process_id	= LogProcessID,
					continue_ability = ContAbility,
					deck_ability_card = DeckAbi},
	q_transaction (Row).

set_game_data(RoomPid, {GameState, PlayerTurnPID, SealCheckUp, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi}) ->
	set_game_data(RoomPid, GameState, PlayerTurnPID, SealCheckUp, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi).

set_game_data (RoomPid) ->
	stack_pool:remove_stack (RoomPid),
	case get_game_data (RoomPid, log_process_id) of
		{ok, LPid} ->
			set_game_data (RoomPid, wait, 0, [], 8, 15, [], LPid, [], []);
		_ ->	set_game_data (RoomPid, wait, 0, [], 8, 15, [], 0, [], [])
	end.

get_game_data (RoomPID) ->
	Query = 
		qlc:q([{
			X#game_data.game_state, 
			X#game_data.player_turn_pid, 
			X#game_data.seal_check_up, 
			X#game_data.max_mp,
			X#game_data.max_shrine, 
			X#game_data.player_list, 
			X#game_data.log_process_id, 
			X#game_data.continue_ability,
			X#game_data.deck_ability_card}||X <- mnesia:table(game_data), X#game_data.room_pid =:= RoomPID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_game_data (RoomPID, FieldData) ->
	case get_game_data(RoomPID) of
		{ok, Result} ->
%			io:format("Room pid ~p Game data result ~p~n", [RoomPID, Result]),
			return_field_data (Result, FieldData);
		{error} ->
			io:format("<mnesia play> Get game data error from Room Pid ~p !!!~n", [RoomPID])
	end.

return_field_data ({GameStep, PlayerTurnPid, ConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi}, Field) ->
	case Field of
		game_step -> {ok, GameStep};
		player_turn -> {ok, PlayerTurnPid};
		seal_checkup -> get_condition_data (ConditionData, Field);
		hand_attack -> get_condition_data (ConditionData, Field);
		max_mp -> {ok, MaxMp};
		max_shrine -> {ok, MaxShrine};
		player_list -> {ok, PlayerList};
		log_process_id -> {ok, LogProcessID};
		continue_ability -> {ok, ContAbility};
		deck_ability_card -> {ok, DeckAbi};
		_ -> io:format("<mnesia play> Get game data field ~p not found~n", [Field])
	end.

get_condition_data ([], _) -> {ok, no_data};
get_condition_data ([{Field, Data}|_], Field) -> {ok, Data};
get_condition_data ([_|T], Field) -> get_condition_data (T, Field).

get_opponent_pid () ->
	{ok, PlayerPid} = get_game_data (self(), player_turn),
	case get_game_data (self(), player_list) of
		{ok, PlayerList} -> play_utility:get_opponent (PlayerPid, PlayerList);
		_ -> io:format("Get game data for opponent error~n")
	end.

get_opponent_pid (PlayerPid) ->
%	io:format("get opponent pid ~p~n", [PlayerPid]),
	case get_game_data (self(), player_list) of
		{ok, PlayerList} -> play_utility:get_opponent (PlayerPid, PlayerList);
		_ -> io:format("Get game data for opponent error~n")
	end.

set_game_data (RoomPid, Field, FieldData) ->
	case get_game_data(RoomPid) of
		{ok, Result} ->
			GameData = update_data_field (Field, FieldData, Result),
			set_game_data (RoomPid, GameData);
		{error} -> 
			io:format("<mnesia play> Get game data error!!~n")
	end.

update_data_field(Field, FieldData, {GameStep, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi}) ->
	case Field of
		game_step -> {FieldData, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi};
		player_turn -> {GameStep, FieldData, ConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi};
		seal_checkup ->	 UpdateConditionData = set_condition_data (ConditionData, Field, FieldData, []),
					{GameStep, PlayerTurnPID, UpdateConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi};
		hand_attack ->	UpdateConditionData = set_condition_data (ConditionData, Field, FieldData, []),
					{GameStep, PlayerTurnPID, UpdateConditionData, MaxMp, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi};
		max_mp -> {GameStep, PlayerTurnPID, ConditionData, FieldData, MaxShrine, PlayerList, LogProcessID, ContAbility, DeckAbi};
		max_shrine -> {GameStep, PlayerTurnPID, ConditionData, MaxMp, FieldData, PlayerList, LogProcessID, ContAbility, DeckAbi};
		player_list -> {GameStep, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, FieldData, LogProcessID, ContAbility, DeckAbi};
		log_process_id -> {GameStep, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, PlayerList, FieldData, ContAbility, DeckAbi};
		continue_ability -> {GameStep, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, PlayerList, FieldData, FieldData, DeckAbi};
		deck_ability_card -> {GameStep, PlayerTurnPID, ConditionData, MaxMp, MaxShrine, PlayerList, FieldData, ContAbility, FieldData};
		_ -> {error, "<mnesia play> Set game data field not found"}
	end.

set_condition_data ([], Field, FieldData, ConditionData) -> ConditionData ++ [{Field, FieldData}];
set_condition_data ([{Field, _}|T], Field, FieldData, ConditionData) -> ConditionData ++ [{Field, FieldData}] ++ T;
set_condition_data ([H|T], Field, FieldData, ConditionData) -> set_condition_data (T, Field, FieldData, ConditionData ++ [H]).

%% PlayerData = {playerpid, first_turn, sealdeck, mysticdeck, hand_cards, arena_cards, shrine_cards, shrine_lv, mp_rest}
set_player_data (PlayerPid, PlayerStatus, FirstTurn, SealDeck, MysticDeck, Hand, Arena, Support, Shrine, Remove, ShrineLevel, MpRest, HandMax, PlayerFx) ->
	Row = #player_data{	player_pid	= PlayerPid,
					player_status	= PlayerStatus,
					first_turn		= FirstTurn,
					seal_deck		= SealDeck,
					mystic_deck		= MysticDeck,
					hand_cards		= Hand,
					arena_cards		= Arena,
					support_cards = Support,
					shrine_cards	= Shrine,
					remove_cards	= Remove,
					shrine_level	= ShrineLevel,
					mp_rest			= MpRest,
					hand_max = HandMax,
					player_effect = PlayerFx},
	%%%smo_logger:fmsg("################## set_plyaer_data with parameters = ~w", [Row]),
	q_transaction(Row).

set_player_data (PlayerPid, {PlayerStatus, FirstTurn, SealDeck, MysticDeck, Hand, Arena, Support, Shrine, Remove, ShrineLevel, MpRest, HandMax, PlayerFx}) ->
	set_player_data (PlayerPid, PlayerStatus, FirstTurn, SealDeck, MysticDeck, Hand, Arena, Support, Shrine, Remove, ShrineLevel, MpRest, HandMax, PlayerFx).

set_player_data (PlayerPid, Field, Value) ->
	case get_player_data(PlayerPid) of
		{ok, Result} ->
			set_data (PlayerPid, Result, Field, Value);
		{error} ->
			io:format("[mnesia play] Set player mp rest Error!!!~n")
	end.

set_data (PlayerPid, {PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx}, Field, Value) ->
	case Field of
		player_status ->
			set_player_data (PlayerPid, PlayerStatus ++ [Value], First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		first_turn ->
			set_player_data (PlayerPid, PlayerStatus, Value, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		seal_deck -> 
			set_player_data (PlayerPid, PlayerStatus, First, Value, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		mystic_deck -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, Value, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		hand_cards -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Value, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		arena_zone -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Value, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		support_cards -> 
			set_player_data(PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Value, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		shrine_cards -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, Value, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		remove_cards -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Value, ShLevel, MpRest, HandMax, PlayerFx);
		shrine_level -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, Value, MpRest, HandMax, PlayerFx);
		mp_rest -> 
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, Value, HandMax, PlayerFx);
		hand_max ->
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, Value, PlayerFx);
		player_effect ->
			set_player_data (PlayerPid, PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, Value);
		_ -> {error, "<mnesia play> Set player data field not found"}
	end.

create_new_player_data (PlayerPid) ->
	set_player_data(PlayerPid, [], first, [], [], [], [], [], [], [], 0, 0, 0, []).

update_player_status (PlayerPid, StatusUpdate) ->
	case get_player_data(PlayerPid) of
		{ok, Result} ->
			{_, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx} = Result,
			set_player_data (PlayerPid, StatusUpdate, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx);
		{error} ->
			io:format("[mnesia play] Set player mp rest Error!!!~n")
	end.

set_mp_rest (PlayerPid, Mp) ->
	set_player_data (PlayerPid, mp_rest, Mp).

%% {First, SDeck, MDeck, Hand, Arena, ShCard, ShLevel, MpRest}
get_player_data (PlayerPID) ->
	Query = qlc:q([{
										X#player_data.player_status, 
										X#player_data.first_turn, 
										X#player_data.seal_deck, 
										X#player_data.mystic_deck, 
										X#player_data.hand_cards,
										X#player_data.arena_cards, 
										X#player_data.support_cards, 
										X#player_data.shrine_cards, 
										X#player_data.remove_cards, 
										X#player_data.shrine_level, 
										X#player_data.mp_rest,
										X#player_data.hand_max,
										X#player_data.player_effect} ||
			X <- mnesia:table(player_data), X#player_data.player_pid =:= PlayerPID]),
	case do(Query) of
		[Result] -> 
			%%%smo_logger:fmsg("mnesia_play:get_player_data return = ~w",[Result]),
			{ok, Result};
		[] -> {error}
	end.

get_player_data (PlayerPid, FieldData) ->
	case get_player_data (PlayerPid) of
		{ok, Result} -> return_player_field(Result, FieldData);
		{error} -> io:format("Get player data error from ~p~n", [PlayerPid]), {error, "[mnesia play] Get player data Error!!!"}
	end.

return_player_field ({PlayerStatus, First, SDeck, MDeck, Hand, Arena, Support, ShCard, Remove, ShLevel, MpRest, HandMax, PlayerFx}, FieldData) ->
	
	case FieldData of
		player_status -> {ok, PlayerStatus};
		first_turn -> {ok, First};
		seal_deck -> {ok, SDeck};
		mystic_deck -> {ok, MDeck};
		hand_cards -> {ok, Hand};
		arena_zone -> {ok, Arena};
		support_cards -> {ok, Support};
		shrine_cards -> {ok, ShCard};
		remove_cards -> {ok, Remove};
		shrine_level -> {ok, ShLevel};
		mp_rest -> {ok, MpRest};
		hand_max -> {ok, HandMax};
		player_effect -> {ok, PlayerFx};
		_ ->	smo_logger:fmsg("Get player data field ~p not found~n", [FieldData]),
			{error, field_data_not_found}
	end.

is_card_on_hand (PlayerPid, CardOrder, CardID) ->
	case get_player_data(PlayerPid, hand_cards) of
		{ok, Hand} -> check_card_on_hand (PlayerPid, CardOrder, CardID, Hand);
		{error} ->	{no}
	end.

check_card_on_hand (PlayerPid, CardOrder, CardID, [{PlayerPid, CardOrder, CardID}|_]) -> {yes};
check_card_on_hand (PlayerPid, CardOrder, CardID, [_|T]) -> check_card_on_hand (PlayerPid, CardOrder, CardID, T);
check_card_on_hand (_, _, _, []) -> {no}.

update_mp (PlayerPid, Mp) ->
	case get_player_data(PlayerPid) of
		{ok, {PlayerStatus, First, SealDeck, MysticDeck, Hand, Arena, Support, SCard, Remove, SLevel, _, HandMax, PlayerFx}} ->
			set_player_data(PlayerPid, PlayerStatus, First, SealDeck, MysticDeck, Hand, Arena, Support, SCard, Remove, SLevel, Mp, HandMax, PlayerFx);
		{error} ->
			io:format ("<mnesia play> Update Mp error no result~n")
	end.

remove_player_data (PlayerPID) ->
	Oid = {player_data, PlayerPID},
	F =	fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

fill_up_mp (RoomPid, PlayerPid) ->
	MaxMp = get_game_data (RoomPid, max_mp),
	set_mp_rest (PlayerPid, MaxMp),
	MaxMp.

check_player_status (PlayerPid, StatusCheck) ->
	case get_player_data (PlayerPid, player_status) of
		{ok, PlayerStatus} -> check_have_status (PlayerStatus, StatusCheck);
		_ -> io:format("Get player data for check status error~n")
	end.

check_have_status ([], _) -> {ok, have_no_status};
check_have_status ([StatusCheck | _], StatusCheck) -> {ok, have_status};
check_have_status ([_ | T], StatusCheck) -> check_have_status (T, StatusCheck).

remove_player_status (PlayerPid, StatusRemove) ->
	case get_player_data (PlayerPid, player_status) of
		{ok, PlayerStatus} ->
			StatusUpdate = remove_player_status (PlayerStatus, StatusRemove, []),
%%			io:format("Player status is ~p and update is ~p~n", [PlayerStatus, StatusUpdate]),
			update_player_status (PlayerPid, StatusUpdate);
		_ -> io:format("Get player data for check status error~n")
	end.

remove_player_status ([], _, PlayerStatus) -> PlayerStatus;
remove_player_status ([StatusRemove | T], StatusRemove, PlayerStatus) -> PlayerStatus ++ T;
remove_player_status ([H | T], StatusRemove, PlayerStatus) -> remove_player_status (T, StatusRemove, PlayerStatus ++ [H]).
