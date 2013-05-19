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
-module(lib_deck_edit).
-compile (export_all).
-import (lists, [split/2, sublist/3, flatlength/1]).

%%%================================================================================================
%%% Request all deck
%%%================================================================================================
request_all_decks(SPlayer) ->
	%% smo_logger:fmsg("What's that = ~w", [SPlayer]),
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			%%{ok,StrPlayerName} = lib_utf8:from_binary(PlayerName),
			%%SQLCommand = "CALL smno.sp_get_active_deck ('" ++ StrPlayerName ++ "')",
			SQLCommand = "CALL smno.sp_get_active_deck ('" ++ LoginName ++ "')",
			QResult = lib_database:get_query(SQLCommand),
			%smo_logger:fmsg("~p <===> ~p~n", [SQLCommand, QResult]),
			case QResult of
				%[{selected,[[97,99,116,105,118,101,68,101,99,107]],[{1925}]
				%[{selected, ["activeDeck"], [{1921}]}, {updated,0}]
				[{selected, _, [{ActiveDeckOrder}]}, _] ->
					response_all_decks (SPlayer, LoginName, ActiveDeckOrder);
				_Error ->
					response_all_decks (SPlayer, LoginName, 0)
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.

response_all_decks (SPlayer, LoginName, ActiveDeckOrder) ->
	SQLCommand = "CALL smno.sp_get_player_decks ('" ++ LoginName ++ "')",
	QResult = lib_database:get_query(SQLCommand),
	%smo_logger:fmsg("~p <===> ~p~n", [SQLCommand, QResult]),
	case QResult of
		[{selected, _, Result}, _] ->
			DataDeck = get_data_decks(Result, [], ActiveDeckOrder),
%			smo_logger:fmsg ("Data deck ~p~n", [DataDeck]),
			%smo_logger:fmsg("Data deck ~w", [DataDeck]),
			%io:format ("5555~p~n",[DataDeck]),
			gen_server:cast(SPlayer, {send, [16#0c, 16#00] ++ DataDeck});
		_Error -> 
			smo_logger:msg("Request all decks error")
	end.

get_data_decks ([], DataDecks, _) ->
	change_to_list (DataDecks, [flatlength(DataDecks)]);
get_data_decks ([Data | T], DataDecks, ActiveDeckOrder) -> 
	DataDecksUpdate = get_decks_reply (Data, [], DataDecks, ActiveDeckOrder),
	get_data_decks(T, DataDecksUpdate, ActiveDeckOrder).
	
change_to_list ([], DataDecksReply) -> DataDecksReply;
change_to_list ([{DeckID, DeckName, IsActive, [SealSize, SData, MysticSize, MData]} | T], DataDecksReply) ->
	%%ASCIIDeckName = lib_ascii_unicode:unicode_to_ascii (DeckName),
	%%{ok,ASCIIDeckName} = lib_utf8:from_binary(DeckName),
	%%DataDeck = {<<DeckID:32>>, size(DeckName), ASCIIDeckName, IsActive, [<<SealSize:32>>, SData, <<MysticSize:32>>, MData]},
	
	% {ok,UNCDeckname} = lib_utf8:from_binary(DeckName),
	% NEWUNCDeckName = lib_utf8:trim_unicode(UNCDeckname),
	
	%DNameUTF = binary_to_list(DeckName),
	
	%DataDeck = {<<DeckID:32>>, length(DNameUTF), DNameUTF, IsActive, [<<SealSize:32>>, SData, <<MysticSize:32>>, MData]},
	DataDeck = {<<DeckID:32>>, length(DeckName), DeckName, IsActive, [<<SealSize:32>>, SData, <<MysticSize:32>>, MData]},
	%smo_logger:fmsg("~w",[DataDeck]),
	change_to_list (T, DataDecksReply ++ tuple_to_list(DataDeck)).

get_decks_reply ({ActiveDeckOrder, DeckName, CardSet, CardNo, CardDateRemain, IsSeal}, Decks, [], ActiveDeckOrder) ->
	DeckDataUpdate = add_deck_data ([0, [], 0, []], CardSet, CardNo, CardDateRemain, IsSeal),
	Decks ++ [{ActiveDeckOrder, DeckName, 1, DeckDataUpdate}];
get_decks_reply ({DeckID, DeckName, CardSet, CardNo, CardDateRemain, IsSeal}, Decks, [], _) ->
	DeckDataUpdate = add_deck_data ([0, [], 0, []], CardSet, CardNo, CardDateRemain, IsSeal),
	Decks ++ [{DeckID, DeckName, 0, DeckDataUpdate}];
get_decks_reply ({ActiveDeckOrder, DeckName, CardSet, CardNo, CardDateRemain, IsSeal}, Decks, [{ActiveDeckOrder, DeckName, IsActive, DeckData} | T], ActiveDeckOrder) ->
	DeckDataUpdate = add_deck_data (DeckData, CardSet, CardNo, CardDateRemain, IsSeal),
	Decks ++ [{ActiveDeckOrder, DeckName, IsActive, DeckDataUpdate}] ++ T;
get_decks_reply ({DeckID, DeckName, CardSet, CardNo, CardDateRemain, IsSeal}, Decks, [{DeckID, DeckName, IsActive, DeckData} | T], _) ->
	DeckDataUpdate = add_deck_data (DeckData, CardSet, CardNo, CardDateRemain, IsSeal),
	Decks ++ [{DeckID, DeckName, IsActive, DeckDataUpdate}] ++ T;
get_decks_reply (Data, Decks, [H | T], ActiveDeckOrder) -> get_decks_reply (Data, Decks ++ [H], T, ActiveDeckOrder).

add_deck_data ([SealSize, SealList, MysticSize, MysticList], null,null,_ ,null) ->
%add_deck_data ([SealSize, SealList, MysticSize, MysticList], undefined, undefined, undefined) ->
	[SealSize, SealList, MysticSize, MysticList];
add_deck_data ([SealSize, SealList, MysticSize, MysticList], CardSet, CardNo, CardDateRemain, 0) ->
	case list_to_integer(CardDateRemain)+2 >= 0 of
		true ->	[SealSize, SealList, MysticSize + 1, MysticList ++ [CardSet, CardNo, list_to_integer(CardDateRemain)+2]];
		false -> [SealSize, SealList, MysticSize + 1, MysticList ++ [CardSet, CardNo, 1]]
	end;	
add_deck_data ([SealSize, SealList, MysticSize, MysticList], CardSet, CardNo, CardDateRemain, 1) ->
	case list_to_integer(CardDateRemain)+2 >= 0 of
		true -> [SealSize + 1, SealList ++ [CardSet, CardNo,  list_to_integer(CardDateRemain)+2], MysticSize, MysticList];
		false -> [SealSize + 1, SealList ++ [CardSet, CardNo,  1], MysticSize, MysticList]
	end.
%%%================================================================================================
%%% Request Player Cards
%%%================================================================================================
query_player_deck(LoginName) ->
	SQLCommand = "CALL smno.sp_get_active_cards ('" ++ LoginName ++ "');",
	QueryResult =  lib_database:get_query(SQLCommand),
	case QueryResult of
		[{selected, _, []} , _]-> [];
		[{selected, _, Result}, _]-> Result;
		[{error, Reason}] -> smo_logger:fmsg("Query error : ~w", [Reason]), []
	end.

request_player_cards(SPlayer) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			SQLCommand = "CALL smno.sp_get_player_card ('" ++ LoginName ++ "')",
			case lib_database:sql_query(SQLCommand) of
				[{selected, _, Result}, _] -> 
					DataReply = get_card_reply_data (Result, 0, [], 0, []),
					gen_server:cast(SPlayer, {send, [16#0c, 16#07] ++ DataReply});
				_Error -> 
					smo_logger:fmsg("Get player card seal error from ~w", [_Error])
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.
	
%{1897, [30,14,35,14,48,14, 35,14,48, 14,48,14, 48,14,48, 14], 1,67,1},

request_player_cards(SPlayer, DeckOrder) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			SQLCommand = "CALL smno.sp_get_player_card ('" ++ LoginName ++ "')",
			case lib_database:sql_query(SQLCommand) of
				[{selected, _, Result}, _] -> 
					DataReply = get_card_reply_data (Result, 0, [], 0, []),
					%smo_logger:fmsg("Data deck reply ~w", [DataReply]),
					gen_server:cast(SPlayer, {send, [16#0c, 16#01, 1, <<DeckOrder:32>>] ++ DataReply});
					%%smo_logger:fmsg("~w",[Result]);
				_Error ->
					smo_logger:fmsg("Get player card seal error from ~w", [_Error])
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.
	

get_card_reply_data ([], SealSize, SealList, MysticSize, MysticList) -> [<<SealSize:32>>, SealList, <<MysticSize:32>>, MysticList];
get_card_reply_data ([{SetID, CardNo, CardDateRemain, 1} | T], SealSize, SealList, MysticSize, MysticList) ->
	case list_to_integer(CardDateRemain)+2 >= 0 of
		true ->	get_card_reply_data (T, SealSize + 1, SealList ++ [SetID, CardNo, list_to_integer(CardDateRemain)+2], MysticSize, MysticList);
		false -> get_card_reply_data (T, SealSize + 1, SealList ++ [SetID, CardNo, 1], MysticSize, MysticList)
	end;
get_card_reply_data ([{SetID, CardNo, CardDateRemain, 0} | T], SealSize, SealList, MysticSize, MysticList) ->
	case list_to_integer(CardDateRemain)+2 >= 0 of
		true ->	get_card_reply_data (T, SealSize, SealList, MysticSize + 1, MysticList ++ [SetID, CardNo, list_to_integer(CardDateRemain)+2]);
		false -> get_card_reply_data (T, SealSize, SealList, MysticSize + 1, MysticList ++ [SetID, CardNo, 1])
	end.

%%%================================================================================================
%%% Save Player Deck
%%%================================================================================================
request_save_deck (SPlayer, Data) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			case Data of
				[DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4, DeckListSize | DataDeck] ->
					<<DeckOrder:32>> = list_to_binary([DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4]),
					save_player_deck (SPlayer, LoginName, DeckOrder, DeckListSize, DataDeck);
				R ->	smo_logger:fmsg("Return ~p~n", [R]),
					gen_server:cast(SPlayer, {send, [16#0c, 16#05, 0]})
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.
	

save_player_deck (SPlayer, LoginName, DeckOrder, DeckListSize, DataDeck) ->
	DataSize = flatlength(DataDeck),
	DeckSize = DeckListSize * 3,
%	smo_logger:fmsg("Save deck by ~p Data size ~p Deck size ~p~n", [LoginName, DataSize, DeckSize]),
	case DataSize of
		DeckSize ->
			DeckOrderString = erlang:integer_to_list (DeckOrder, 10),
			case remove_deck (DeckOrderString, LoginName, DeckListSize, DataDeck) of
				{updated} ->
					gen_server:cast(SPlayer, {send, [16#0c, 16#05, 1]});
				_ ->
					gen_server:cast(SPlayer, {send, [16#0c, 16#05, 0]})
			end;
		_ -> smo_logger:msg("Save deck size error")
	end.

remove_deck (DeckOrderString, LoginName, DeckListSize, DataDeck) ->
	SQLCommand = "CALL smno.sp_clear_deck (" ++ DeckOrderString ++ ", '" ++ LoginName ++ "')",
	case lib_database:sql_query(SQLCommand) of
		{updated, _} ->
			insert_deck_rows("CALL smno.sp_save_deck ('" ++ LoginName ++ "', " ++ DeckOrderString ++ ", '", DeckListSize, DataDeck);
		{error, R} ->
			smo_logger:fmsg("Remove deck error from ~w", [R])
	end.

insert_deck_rows(SQLCommand, DeckListSize, []) ->
	CommandQuery = SQLCommand ++ "', " ++ erlang:integer_to_list (DeckListSize, 10) ++ ");",
	%smo_logger:fmsg ("~p", [CommandQuery]),
	%%{updated};
	case lib_database:sql_query(CommandQuery) of
		[{selected, _, [{1}]}, _] -> 
			{updated};
		[{selected, _, [{0}]}, _] -> 
			smo_logger:msg("Update deck date error");
		[{error, R}] -> 
			smo_logger:fmsg("Update deck date error from ~w", [R]);
		R2 -> 
			smo_logger:fmsg("Return other~w", [R2])
	end;
insert_deck_rows(SQLCommand, DeckListSize, [CardSet, CardNo, CardDateRemain | DataDeck]) ->
	CSString = erlang:integer_to_list (CardSet, 10),
	CNString = erlang:integer_to_list (CardNo, 10),
	CDString = erlang:integer_to_list (CardDateRemain, 10),
	case DataDeck of
		[] ->	
			CommandUpdate = SQLCommand ++ CSString ++ ", " ++ CNString ++ ", " ++ CDString;
		_ ->	
			CommandUpdate = SQLCommand ++ CSString ++ ", " ++ CNString ++ ", " ++ CDString ++ ", "
	end,
	insert_deck_rows(CommandUpdate, DeckListSize, DataDeck).

%%%================================================================================================
%%% Change deck's name
%%%================================================================================================
request_change_deck_name (SPlayer, [DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4, DeckNameSize | DeckName]) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			<<DeckOrder:32>> = <<DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4>>,
			SizeVerify = lists:flatlength (DeckName),
			case DeckNameSize of
				SizeVerify ->
					verify_change_name (SPlayer, LoginName, DeckOrder, DeckNameSize, DeckName);
				R ->	
					smo_logger:fmsg("Can not change deck name : ~w", [R]),
					gen_server:cast(SPlayer, {send, [16#0c, 16#02, 0]})
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.

verify_change_name (SPlayer, LoginName, DeckOrder, DeckNameSize, DeckName) ->
	SQLCommand = "CALL smno.sp_change_deck_name (" ++ integer_to_list(DeckOrder) ++  ", '" ++ DeckName ++ "', '" ++ LoginName ++ "')",
	QResult = lib_database:sql_query(SQLCommand),
	%smo_logger:fmsg("~p <===> ~p~n", [SQLCommand, QResult]),
	case QResult of
		[{selected, _, [{1}]}, _] ->
			case lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]) of
				[LoginName] ->
					gen_server:cast(SPlayer, {send, [16#0c, 16#02, 1, length(DeckName)] ++ DeckName});
				_ -> gen_server:cast(SPlayer, {send, [16#0c, 16#02, 0]})
			end;
		[{selected, _, [{0}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#0c, 16#02, 0]});
		R ->	
			smo_logger:fmsg("Return ~w", [R]),
			gen_server:cast(SPlayer, {send, [16#0c, 16#02, 0]})
	end.
%%%================================================================================================
%%% Delete specific deck
%%%================================================================================================
request_delete_deck (SPlayer, Data) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			case list_to_binary(Data) of
				<<A, B, C, D>> ->	
					<<DeckOrder:32>> = <<A, B, C, D>>,
					delete_deck(SPlayer, LoginName, DeckOrder);
				_Why ->
					smo_logger:fmsg("Can not delete deck ~w", [_Why]),
					gen_server:cast(SPlayer, {send, [16#0c, 16#06, 0]})
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.

delete_deck (SPlayer, LoginName, DeckOrder) ->
	case is_integer (DeckOrder) of
		true ->
			DeckOrderString = erlang:integer_to_list (DeckOrder, 10),
			SQLCommand = "CALL smno.sp_delete_deck (" ++ DeckOrderString ++ ", '" ++ LoginName ++ "')",
			case lib_database:sql_query(SQLCommand) of
				{updated, _} ->
					find_deck_active (SPlayer, LoginName);
				{error, R} ->
					smo_logger:fmsg("Get player card seal error from ~w", [R])
			end;
		false ->
			smo_logger:msg("Data deck order error")
	end.

find_deck_active (SPlayer, LoginName) ->
	SQLCommand = "CALL smno.sp_get_active_deck ('" ++ LoginName ++ "')",
	case lib_database:sql_query (SQLCommand) of
		[{selected, _, [{0}]}, _] ->
			ActiveDeckOrder = 9999999,
			gen_server:cast(SPlayer, {send, [16#0c, 16#06, 1, <<ActiveDeckOrder:32>>]});
		[{selected, _, [{ActiveDeckOrder}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#0c, 16#06, 1, <<ActiveDeckOrder:32>>]});
		[{error, _}, _] ->
			gen_server:cast(SPlayer, {send, [16#0c, 16#06, 0]});
		R ->	io:format("Query result ~p~n", [R]),
		gen_server:cast(SPlayer, {send, [16#0c, 16#06, 0]})
	end.

%%%================================================================================================
%%% Create Deck
%%%================================================================================================
request_create_deck (SPlayer, [DeckNameSize | T]) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			DeckName = sublist(T, 1, DeckNameSize),
			SQLCommand = "SELECT smno.fc_create_deck ('" ++ DeckName ++ "', '" ++ LoginName ++ "')",
			%smo_logger:fmsg("SQLCommand ~w",[SQLCommand]),
			case lib_database:sql_query(SQLCommand) of
				{selected, _, [{0}]} ->
					gen_server:cast(SPlayer, {send, [16#0c, 16#01, 0]});
				{selected, _, [{DeckOrder}]} ->
					get_player_cards(SPlayer, LoginName, DeckOrder);
				{error, R} ->
					smo_logger:fmsg("Errer from ~w", [R]), 
					gen_server:cast(SPlayer, {send, [16#0c, 16#01, 0]})
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.

get_player_cards(SPlayer, LoginName) ->
	SQLCommand = "CALL smno.sp_get_player_card ('" ++ LoginName ++ "')",
	case lib_database:sql_query(SQLCommand) of
		[{selected, _, Result}, _] -> 
			DataReply = get_card_reply_data (Result, 0, [], 0, []),
			gen_server:cast(SPlayer, {send, [16#0c, 16#07] ++ DataReply});
		[{error, R}, _] -> 
			smo_logger:fmsg("Get player card seal error from ~w", [R])
	end.

get_player_cards(SPlayer, LoginName, DeckOrder) ->
	SQLCommand = "CALL smno.sp_get_player_card ('" ++ LoginName ++ "')",
	case lib_database:sql_query(SQLCommand) of
		[{selected, _, Result}, _] -> 
			DataReply = get_card_reply_data (Result, 0, [], 0, []),
			%smo_logger:fmsg("Data deck reply ~w", [DataReply]),
			gen_server:cast(SPlayer, {send, [16#0c, 16#01, 1, <<DeckOrder:32>>] ++ DataReply});
		[{error, R}, _] ->
			smo_logger:fmsg("Get player card seal error from ~w", [R])
	end.

%%%================================================================================================
%%% Set Active Deck
%%%================================================================================================
set_active_deck(SPlayer, Data) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id, playing_status]) of
		[LoginName, Status] ->
			case Data of
				[DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4] ->
					<<DeckOrder:32>> = list_to_binary([DeckOrder1, DeckOrder2, DeckOrder3, DeckOrder4]),
					check_deck_active (SPlayer, LoginName, DeckOrder);
				R ->	
					smo_logger:fmsg("Return ~w", [R]),
					gen_server:cast(SPlayer, {send, [16#0c, 16#08, 0]})
			end;
		_ ->
			smo_logger:msg("Found nothing")
	end.


check_deck_active(SPlayer, LoginName, DeckOrder) ->
	SQLCommand = "CALL smno.sp_set_active_deck ('" ++ LoginName ++ "', " ++ integer_to_list(DeckOrder) ++ ")",
	%smo_logger:fmsg("SQLCommand <<==>> ~p~n", [SQLCommand]),
	case lib_database:sql_query(SQLCommand) of
		[{selected, _, [{1}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#0c, 16#08, 1, <<DeckOrder:32>>]});
		[{selected, _, [{0}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#0c, 16#08, 0]});
		R ->	
			smo_logger:fmsg("Get player card seal error from ~w", [R]),
			gen_server:cast(SPlayer, {send, [16#0c, 16#08, 0]})
	end.
	
	