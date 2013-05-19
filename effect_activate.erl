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
-module (effect_activate).

-export ([send_update_activate_effect/5, send_update_other_data/3]).

send_update_activate_effect (CardOwner, CardOrder, CardID, Effect, UpdateType) ->
	case UpdateType of
		add ->
			{FxSize, FxData} = get_add_effect_reply (CardOwner, CardOrder, CardID, Effect, 0, []);
		remove ->
			{FxSize, FxData} = get_remove_effect_reply(CardOwner, CardOrder, CardID, Effect, 0, []);
		update ->
			FxSize = 0,
			FxData = []
	end,
%	[{2,10},{3,10},{4,4},{99,3},{6,3},{5,4},{7,0}] = card_info:check_update_card_info_data (OwnerPid, CardOrder, CardID),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			FxCardInfo = card_info:check_update_card_info_data(CardOwner, CardOrder, CardID);
			%smo_logger:fmsg("FxCardInfo ~p~n", [FxCardInfo]);
		is_not_seal ->
			FxCardInfo = []
	end,
	[CardInfoSize | CardInfoData] = get_reply_info_data (FxCardInfo, 0),
%	io:format ("Card info size ~p Card info data ~p~n", [CardInfoSize, CardInfoData]),
	case FxSize + CardInfoSize of
		0 -> no_fx_activate;
		_ -> send_activate_effect (CardOwner, CardOrder, CardID, FxSize + CardInfoSize, FxData ++ CardInfoData)
	end.

get_reply_info_data ([], FxSize) -> [FxSize];
get_reply_info_data ([Data | FxCardInfo], FxSize) ->
	get_reply_info_data (FxCardInfo, FxSize + 1) ++ tuple_to_list (Data).

send_activate_effect (OwnerPid, CardOrder, CardID, FxSize, FxData) ->
	%erase(ability), %ลบ ability เก่าเก็บไว้เช็คใน shrine zone
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	%smo_logger:fmsg("Send ~w ~w [effect_activate.erl]", [FxSize, FxData]),
	lists:foreach(	fun({PlayerPid, _}) ->
		FxReply = map_player_data (FxData, PlayerPid),
%		io:format ("Fx reply ~p~n", [FxReply]),
		case OwnerPid of
			PlayerPid ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#78] ++ [1] ++ [CardOrder, <<CardID:16>>] ++ [FxSize] ++ FxReply});
			_ ->	
				gen_server:cast(PlayerPid, {send, [16#88, 16#78] ++ [0] ++ [CardOrder, <<CardID:16>>] ++ [FxSize] ++ FxReply})
		end
	end, PlayerList).

map_player_data ([], _) -> [];
map_player_data ([Data | FxData], PlayerPid) when is_pid (Data) ->
	case Data of
		PlayerPid -> [1] ++ map_player_data (FxData, PlayerPid);
		_ ->	[0] ++ map_player_data (FxData, PlayerPid)
	end;
map_player_data ([Data | FxData], PlayerPid) ->
	[Data] ++ map_player_data (FxData, PlayerPid).

get_curse_duration (PlayerPid, CardOrder, CardID, Curse) ->
	CardFx = card_utility:get_all_card_effect (PlayerPid, CardOrder, CardID),
	check_duration_curse (CardFx, Curse).

check_duration_curse ([], _) -> io:format("Fx check send error!! No curse on card~n [effect_activate.erl]");
check_duration_curse ([{GFx, Fx, Duration} | CardFx], Curse) ->
	case check_curse (Fx, Curse) of
		found_curse -> {ok, GFx, Duration};
		curse_not_found -> check_duration_curse (CardFx, Curse)
	end.

check_curse ([], _) -> curse_not_found;
check_curse ([{curse, Curse} | _], Curse) -> found_curse;
check_curse ([_ | Fx], Curse) -> check_curse (Fx, Curse).

check_string_duration ({PlayerPid, CardOrder, CardID}, depend_on_s)  ->
	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	case card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, duration, Zone) of
		{ok, Duration} -> Duration;
		_ -> 0
	end;
check_string_duration (GFx, Duration)  ->
	io:format ("Give fx card ~p [effect_activate.erl]~n", [GFx]),
	io:format ("String duration out of bound ~p [effect_activate.erl]~n", [Duration]),
	0.

get_curse_fx_data (MainFxID, Curse, PlayerPid, CardOrder, CardID) ->
	case get_curse_duration (PlayerPid, CardOrder, CardID, Curse) of
		ok -> CurseDuration = 0;
		{ok, GFx, Duration} -> 
			case is_integer (Duration) of
				true -> CurseDuration = Duration;
				false ->
					CurseDuration = check_string_duration (GFx, Duration)
			end
	end,
	case Curse of
		stone_curse -> [MainFxID, 1, CurseDuration];
		poison_curse -> [MainFxID, 2, CurseDuration];
		{last_dance_curse, _} -> [MainFxID, 3, CurseDuration];
		{charm_curse, _} -> [MainFxID, 4, CurseDuration];
		death_curse -> [MainFxID, 5, CurseDuration];
		freeze_curse -> [MainFxID, 6, CurseDuration];
		dimension_curse -> [MainFxID, 7, CurseDuration];
		_ -> io:format("Receive other curse ~p~n", [Curse]), []
	end.

get_player_effect_value (CardOwner, CardOrder, CardID, MainFxID, MaxData, {Receiver, Value}, ValueType) ->
	%case ability_activate:check_value_data (CardOwner, CardOrder, CardID, Value) of
	% case effect_value:check_value(CardOwner, CardOrder, CardID, Value) of
		% 0 -> io:format ("-- Sub fx return value 0 from ~p~n", [Value]), [];
		% Value ->
			case Receiver of
				player ->
					ReceiverPid = CardOwner;
				opponent ->
					ReceiverPid = mnesia_play:get_opponent_pid (CardOwner)
			end,
			case ValueType of
				add -> [MainFxID, MaxData + Value, ReceiverPid];
				remove -> [MainFxID, MaxData - Value, ReceiverPid]
			end.
	% end.

get_add_effect_reply (_, _, _, [], EffectSize, EffectData) -> {EffectSize, EffectData};
get_add_effect_reply (PlayerPid, CardOrder, CardID, [{MainFx, SubFx} | T], EffectSize, EffectData) ->
	case MainFx of
		curse -> FxData = get_curse_fx_data (1, SubFx, PlayerPid, CardOrder, CardID);
		hand_max -> FxData = get_player_effect_value (PlayerPid, CardOrder, CardID, 8, 7, SubFx, add);
		mp_max -> FxData = get_player_effect_value (PlayerPid, CardOrder, CardID, 9, 8, SubFx, add);
		move ->
			case SubFx of
				to_deck -> FxData = [11, 1];
				_ ->	io:format ("### move effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		at ->	case SubFx of
				to_df ->  FxData = [11, 18];
				from_df -> FxData = [11, 19];
				to_df_more_sp -> FxData = [11, 20];
				_ ->	io:format ("### at effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		disallow ->
			case SubFx of
				assign_line_to_df ->  FxData = [11, 29];
				_ ->	io:format ("### assign_line effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		cancel_curse ->
			FxData = get_cancel_curse_effect (11, SubFx);
		protect ->
			FxData = get_sub_effect (11, SubFx);
		cancel_mystic ->
			FxData = get_sub_effect (11, SubFx);
		_ ->	io:format ("### update effect ~p not send~n", [{MainFx, SubFx}]),
			FxData = []
	end,
	case lists:flatlength (FxData) of
		0 -> get_add_effect_reply (PlayerPid, CardOrder, CardID, T, EffectSize, EffectData);
		_ -> get_add_effect_reply (PlayerPid, CardOrder, CardID, T, EffectSize + 1, EffectData ++ FxData)
	end.

get_remove_effect_reply (_, _, _, [], EffectSize, EffectData) -> {EffectSize, EffectData};
get_remove_effect_reply(CardOwner, CardOrder, CardID, [{_GFx, []}| T], EffectSize, EffectData) -> get_remove_effect_reply(CardOwner, CardOrder, CardID,  T, EffectSize, EffectData);
get_remove_effect_reply (CardOwner, CardOrder, CardID, [{GFx, Fx, _Duration}| T], EffectSize, EffectData) -> get_remove_effect_reply(CardOwner, CardOrder, CardID, [{GFx, Fx}| T], EffectSize, EffectData);
get_remove_effect_reply (CardOwner, CardOrder, CardID, [{GFx, [{MainFx, SubFx}|SubT]}| T], EffectSize, EffectData) ->
	case MainFx of
		curse -> FxData = get_curse_fx_data (10, SubFx, CardOwner, CardOrder, CardID);
		hand_max -> FxData = get_player_effect_value (CardOwner, CardOrder, CardID, 8, 7, SubFx, remove);
		mp_max -> FxData = get_player_effect_value (CardOwner, CardOrder, CardID, 9, 8, SubFx, remove);
		move ->
			case SubFx of
				to_deck -> FxData = [12, 1];
				_ ->	io:format ("### move effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		at ->	case SubFx of
				to_df -> FxData = [12, 18];
				from_df -> FxData = [12, 19];
				to_df_more_sp -> FxData = [12, 20];
				_ ->	io:format ("### at effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		assign_line ->
			case SubFx of
				disallow_to_df ->  FxData = [12, 29];
				_ ->	io:format ("### assign_line effect ~p not send~n", [SubFx]),
					FxData = []
			end;
		cancel_curse ->
			FxData = get_cancel_curse_effect (12, SubFx);
		protect ->
			FxData = get_sub_effect (12, SubFx);
		cancel_mystic ->
			FxData = get_sub_effect (12, SubFx);
		special ->
			FxData = [],
			{ok, CombineState} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, combine, arena_zone),
					Update = seal_card:remove_support_cards (CombineState, SubFx),
					CombineUpdate = seal_card:check_update_data (Update),
					card_utility:update_card_option_field (CardOwner, CardOrder, CardID, combine, CombineUpdate);
		action ->
			FxData =
			case SubFx of
				inactive -> 
					case  card_utility:get_card_option_field(CardOwner, CardOrder, CardID, active) of
						{ok, active} -> arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID), [];
						_ -> []
					end;
				_ -> []
			end;
		swap_power ->
			FxData = [],
			seal_card:swap_card_power(CardOwner, CardOrder, CardID, SubFx);
		elem ->
			FxData = [],
			seal_card:rechange_element(CardOwner, CardOrder, CardID, GFx);
		_ ->	io:format ("### update effect ~p not send~n", [{MainFx, SubFx}]),
			FxData = []
	end,
	case lists:flatlength(FxData) of
		0 -> get_remove_effect_reply(CardOwner, CardOrder, CardID, [{GFx, SubT}|T], EffectSize, EffectData);
		_ -> get_remove_effect_reply(CardOwner, CardOrder, CardID, [{GFx, SubT}|T], EffectSize + 1, EffectData ++ FxData)
	end.

send_update_other_data (PlayerPid, UpdateType, Data) ->
	case UpdateType of
		mp ->
			[SelfMp, OpponentMp] = Data,
			gen_server:cast(PlayerPid, {send, [16#88, 16#76, 0, SelfMp, OpponentMp]});
		_ -> io:format ("Other update type ~p~n", [UpdateType])
	end.

get_cancel_curse_effect (_, []) -> [];
get_cancel_curse_effect (MainId, [Fx | SubFx]) ->
	case Fx of
		stone_curse -> [MainId, 21] ++ get_cancel_curse_effect (MainId, SubFx);
		freeze_curse -> [MainId, 22] ++ get_cancel_curse_effect (MainId, SubFx);
		charm_curse -> [MainId, 23] ++ get_cancel_curse_effect (MainId, SubFx);
		poison_curse -> [MainId, 24] ++ get_cancel_curse_effect (MainId, SubFx);
		death_curse -> [MainId, 25] ++ get_cancel_curse_effect (MainId, SubFx);
		last_dance_curse -> [MainId, 26] ++ get_cancel_curse_effect (MainId, SubFx);
		dimension_curse -> [MainId, 27] ++ get_cancel_curse_effect (MainId, SubFx);
		all -> [MainId, 35] ++ get_cancel_curse_effect (MainId, SubFx)
	end.

get_sub_effect (_, []) -> [];
get_sub_effect (MainId, [Fx | SubFx]) ->
	case Fx of
		all -> [MainId, 3] ++ get_cancel_curse_effect (MainId, SubFx);
		attack_all -> [MainId, 17] ++ get_sub_effect (MainId, SubFx);
		all_opponent -> [MainId, 39] ++ get_sub_effect (MainId, SubFx);
		_ ->	io:format ("Other sub fx ~p~n", [Fx]),
			get_sub_effect (MainId, SubFx)
	end.
