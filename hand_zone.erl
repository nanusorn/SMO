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
-module (hand_zone).

-import (lists, [append/2, flatlength/1]).

-export ([get_opponent_hand_data/2, get_opponent_hand_card/3, req_hand_info/2]).
-export ([get_hand_data/2, check_ability/4, set_hand_card_option/5]).
-export ([get_option_field/4, update_card/4, get_card_option/3, get_card/3]).
-export ([check_card_size/1, check_card_size/2, update_hand_data/0]).

get_hand_data(SealHand, MysticHand) ->
	{get_card_data (SealHand), get_card_data (MysticHand)}.

get_card_data ([]) -> [];
get_card_data ([{PlayerPid, CardOrder, CardID} | T]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> CardOption = seal_card:set_create_option(CardID, []);
		is_not_seal -> CardOption = mystic_card:set_create_option(CardID, [])
	end,
	[{{PlayerPid, CardOrder, CardID}, CardOption}] ++ get_card_data (T);
get_card_data ([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> CardOption = seal_card:set_create_option(CardID, []);
		is_not_seal -> CardOption = mystic_card:set_create_option(CardID, [])
	end,
	[{{PlayerPid, CardOrder, CardID}, CardOption}] ++ get_card_data (T).

get_data ([], ReplyData) -> ReplyData;
get_data ([{{_, CardOrder, CardID}, _} | T], ReplyData) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			get_data (T, append(ReplyData, [0, CardOrder]) );
		is_not_seal ->
			get_data (T, append(ReplyData, [1, CardOrder]) )
	end.

get_opponent_hand_data (PlayerPid, PlayerList) ->
	OpponentPid = play_utility:get_opponent(PlayerPid, PlayerList),
	{ok, OppHand} = mnesia_play:get_player_data (OpponentPid, hand_cards),
	HandSort = function_utility:qsort(OppHand),
	{check_card_size (OpponentPid), get_data (HandSort, [])}.

get_opponent_hand_card (PlayerPid, PlayerList, CardOrder) ->
	OpponentPid = play_utility:get_opponent(PlayerPid, PlayerList),
	{ok, OppHand} = mnesia_play:get_player_data (OpponentPid, hand_cards),
	get_card_data (OppHand, CardOrder).

get_card_data ([], _) -> io:format ("Hand's card not found~n");
get_card_data ([{{PlayerPid, CardOrder, CardID}, _}|_], CardOrder) -> {PlayerPid, CardID};
get_card_data ([_|T], CardOrder) -> get_card_data (T, CardOrder).

find_card ([], _, _, _) -> io:format ("Hand's card not found~n");
find_card ([{{PlayerPid, CardOrder, CardID}, CardOption}|_], PlayerPid, CardOrder, CardID) -> {ok, {{PlayerPid, CardOrder, CardID}, CardOption}};
find_card ([_|T], PlayerPid, CardOrder, CardID) -> find_card (T, PlayerPid, CardOrder, CardID).

find_card_option ([], _, _, _) -> io:format ("Hand's card not found~n");
find_card_option ([{{PlayerPid, CardOrder, CardID}, CardOption}|_], PlayerPid, CardOrder, CardID) -> {ok, CardOption};
find_card_option ([_|T], PlayerPid, CardOrder, CardID) -> find_card_option (T, PlayerPid, CardOrder, CardID).

get_card_option (PlayerPid, CardOrder, CardID) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	find_card_option (Hand, PlayerPid, CardOrder, CardID).

get_card (PlayerPid, CardOrder, CardID) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	find_card (Hand, PlayerPid, CardOrder, CardID).

update ([], _, _, _, _) -> [];
update ([{{PlayerPid, CardOrder, CardID}, _} | T], PlayerPid, CardOrder, CardID, CardOption) -> 
	[{{PlayerPid, CardOrder, CardID}, CardOption}] ++ T;
update ([H|T], PlayerPid, CardOrder, CardID, CardOption) -> [H] ++ update (T, PlayerPid, CardOrder, CardID, CardOption).

update_card (PlayerPid, CardOrder, CardID, CardOption) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	HandUpdate = update (Hand, PlayerPid, CardOrder, CardID, CardOption),
	mnesia_play:set_player_data (PlayerPid, hand_cards, HandUpdate).

req_hand_info (ReplyPid, RequestPid) ->
	{ok, HandCards} = mnesia_play:get_player_data (RequestPid, hand_cards),
	{NumSeal, NumMystic} = get_reply_hand_infomation (HandCards, 0, 0),
	ReplyData = [16#8b, 16#01, NumSeal, NumMystic],
	gen_server:cast(self(), {res_hand_info, ReplyPid, ReplyData}).

get_reply_hand_infomation ([], NumSeal, NumMystic) -> {NumSeal, NumMystic};
get_reply_hand_infomation ([{{_, _, CardID}, _} | T], NumSeal, NumMystic) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal -> get_reply_hand_infomation (T, NumSeal + 1, NumMystic);
		is_not_seal -> get_reply_hand_infomation (T, NumSeal, NumMystic + 1)
	end.

get_option_field (PlayerPid, CardOrder, CardID, Option) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	case find_card_option (Hand, PlayerPid, CardOrder, CardID) of
		{ok, CardOption} ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> seal_card:get_seal_option (CardOption, Option);
				is_not_seal -> mystic_card:get_mystic_option (CardOption, Option)
			end;
		{error, Reason} ->
			{error, Reason}
	end.

set_hand_card_option (PlayerPid, CardOrder, CardID, Option, Data) ->
	{ok, Hand} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	case find_card_option (Hand, PlayerPid, CardOrder, CardID) of
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

check_ability(PlayerPid, CardOrder, CardID, AbilityCheck) ->
	case get_option_field (PlayerPid, CardOrder, CardID, receive_effect) of
		{ok, RFx} -> 
			case check_hand_effect (RFx, AbilityCheck) of
				{ok, {GFx, Value}} ->
					%McValue = ability_activate:check_value_data (PlayerPid, CardOrder, CardID, Value),
					McValue = 
					case is_integer(Value) of
						true -> Value;
						_ -> 
							{GOwner, GOrder, GID, _} = GFx,
							[{_, ReValue}] = effect_value:check_value(GOwner, GOrder, GID, {ignore, Value}, {PlayerPid, CardOrder, CardID}),
							ReValue
					end,
					{ok, McValue};
				{no_effect} -> {no_effect}
			end;
		_ -> {no_effect}
	end.

check_hand_effect ([], _) -> {no_effect};
check_hand_effect ([{GFx, Effect, _} | T], AbilityCheck) ->
	case check_ability_match (Effect, AbilityCheck) of
		{ok, ValueType} -> {ok, {GFx, ValueType}};
		{no_effect} -> check_hand_effect (T, AbilityCheck)
	end.

check_ability_match ([], _) -> {no_effect};
check_ability_match ([{AbilityCheck, ValueType} | _], AbilityCheck) -> {ok, ValueType};
check_ability_match ([_ | T], AbilityCheck) -> check_ability_match (T, AbilityCheck).

check_card_size (PlayerPid) ->
	{ok, Hands} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	lists:flatlength (Hands).

check_card_size (PlayerPid, CardType) ->
	{ok, Hands} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	count_card_type (Hands, CardType).

count_card_type ([], _) -> 0;
count_card_type ([{{_, _, CardID}, _} | Hands], CardType) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CardType -> 1 + count_card_type (Hands, CardType);
		_ -> count_card_type (Hands, CardType)
	end.

update_hand_data() ->
	{ok, PlayerList} = mnesia_play:get_game_data(self(), player_list),
	lists:foreach(fun({PlayerPid, _}) ->
		Opponent = mnesia_play:get_opponent_pid (PlayerPid),
		SealSize = check_card_size (Opponent, is_seal),
		MysSize = check_card_size (Opponent, is_not_seal),
		gen_server:cast(PlayerPid, {send, [16#88, 16#76, 16#02, SealSize, MysSize]})
	end, PlayerList).