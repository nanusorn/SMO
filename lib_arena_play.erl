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
-module (lib_arena_play).

-import(lists, [flatlength/1, split/2, reverse/1]).
-import(random, [uniform/1, seed/0, seed/3]).

-compile (export_all).

%% ------------------ ส่วนของการทำงาน เป่ายิงฉุบ --------------------- %%
check_ying_chub ([], Pid, Selected) -> [{Pid, Selected}];
check_ying_chub ([{Pid, S}|_], Pid, _) -> [{Pid, S}];
check_ying_chub ([H|_], Pid, Selected) ->
	smo_logger:msg("in check ying chub routine"),
	case H of
		{FirstID, FirstSelected} -> 
			case	check_ying_chub_result (FirstSelected, Selected) of
				win ->
					%smo_logger:msg("sending back to client 1"),
					gen_server:cast(FirstID, {send, [16#83, 2, 1, FirstSelected, Selected]}),
					gen_server:cast(Pid, {send, [16#83, 2, 0, Selected, FirstSelected]}),
					[{FirstID, win}, {Pid, lost}];
				lost ->
					%smo_logger:msg("sending back to client 2"),
					gen_server:cast(FirstID, {send, [16#83, 2, 0, FirstSelected, Selected]}),
					gen_server:cast(Pid, {send, [16#83, 2, 1, Selected, FirstSelected]}),
					[{FirstID, lost}, {Pid, win}];
				draw ->
					%smo_logger:msg("sending back to client 3"),
					gen_server:cast(FirstID, {send, [16#83, 2, 2, FirstSelected, Selected]}),
					gen_server:cast(Pid, {send, [16#83, 2, 2, Selected, FirstSelected]}),
					[];
				{F, S} -> smo_logger:fmsg("check_ying_chub_result ~p~n", [{F, S}]),
					[{FirstID, F}, {Pid, S}];
				_error -> smo_logger:fmsg("check_ying_chub_result ~p~n", [_error])
			end;
		_ ->
			""
	end.

check_ying_chub_result (F, S) ->
	case {F, S} of
		{[1], [1]} -> draw;
		{[2], [2]} -> draw;
		{[3], [3]} -> draw;
		{[1], [2]} -> win;
		{[1], [3]} -> lost;
		{[2], [1]} -> lost;
		{[2], [3]} -> win;
		{[3], [1]} -> win;
		{[3], [2]} -> lost;
		{win, _} -> {win, lost};
		{draw, _} -> {draw, draw};
		{lost, _} -> {lost, win};
		_ -> io:format("Other case First is ~p, Seconde is ~p~n", [F, S])
	end.
%% ------------------------------------------------------------------------------ %%

%% ------------------ ส่วนของการทำงาน ของการ สลับการ์ด --------------------- %%
shuffer_deck (PlayerPid, SD, MD) ->
	%smo_logger:fmsg("all intit seal are ~p~n", [SD]),
	[A, B, C] = tuple_to_list(now()),
	seed(A, B, C),
	
	% สุ่มจำนวนการ์ด ที่จะ หยิบ ในแต่ละครั้ง
	SealCount = length(SD),
	MysticCount = length(MD),
	
	% สุ่มจำนวนครั้งมี่จะหยิบการ์ด
	SShufferTime = SealCount * 4,%(uniform(SealCount) + 5)  * 2 ,
	MShufferTime = MysticCount * 4,%(uniform(MysticCount) + 5) * 2,
	% smo_logger:fmsg("SShufferTime ~p~n", [SShufferTime]),
	% smo_logger:fmsg("MShufferTime ~p~n", [MShufferTime]),

	%SSD = shuffer(SD, SShufferTime),
	% smo_logger:fmsg("all intit seal are ~p~n", [SD]),
	% smo_logger:fmsg("all seal after shuffle are ~p~n", [SSD]),
	%SealOption = seal_card:get_default_option (),
	%{ONum, SOSD} = set_cards_order(SD, 1, PlayerPid, SealOption),
	SOSD = set_seals_order(PlayerPid, 1, SD),
	
	SSD = shuffer(SOSD, SShufferTime),
	
	%SMD = shuffer(MD, MShufferTime),
	% smo_logger:fmsg("all intit mystic are ~p~n", [MD]),
	% smo_logger:fmsg("all seal after mystic are ~p~n", [SMD]),
	%MysticOption = mystic_card:get_default_option (),
	SOMD = set_mystics_order(PlayerPid, SealCount + 1, MD),
	SMD = shuffer(SOMD, MShufferTime),
	
	%{SOSD, SOMD}.
	{SSD, SMD}.


shuffer(Deck, 0) -> Deck;
shuffer(Deck, ShufferTime) ->
	DeckCount = length(Deck),
	Sampling = round(uniform(DeckCount) / 2),
	%smo_logger:fmsg("Sampling is ~p~n", [Sampling]),
	Sample =function_utility:random_select(Sampling, Deck),
	%smo_logger:fmsg("sample card are ~p~n", [Sample]),
	Shuffered = Sample ++ (Deck -- Sample),
	shuffer(Shuffered, ShufferTime - 1).

set_seals_order(_, _, []) -> [];
set_seals_order(PlayerPid, OrderNumber, [CardID|Card]) ->
	CardOption = seal_card:get_default_option(CardID),
	[{{PlayerPid, OrderNumber, CardID}, CardOption}] ++ set_seals_order(PlayerPid, OrderNumber + 1, Card).
	%set_cards_order(Deck, OrderNumber, PlayerPid).
	
set_mystics_order(_, _, []) -> [];
set_mystics_order(PlayerPid, OrderNumber, [CardID|Card]) ->
	CardOption = mystic_card:get_default_option(CardID),
	[{{PlayerPid, OrderNumber, CardID}, CardOption}] ++ set_mystics_order(PlayerPid, OrderNumber + 1, Card).
	
% set_cards_order(Deck, OrderNumber, PlayerPid, CardOption) ->
	% set_card_order(Deck, OrderNumber, CardOption, PlayerPid, []).
% 
% set_card_order ([], ONum, _, ODeck) -> {ONum, ODeck};
% set_card_order ([H | T], ONum, PlayerPid, ODeck) ->
	% set_card_order (T, ONum + 1, PlayerPid, ODeck ++ [{PlayerPid, ONum, H}]).
% 
% set_card_order ([], ONum, _, _, ODeck) -> {ONum, ODeck};
% set_card_order ([H | T], ONum, CardOption, PlayerPid, ODeck) ->
	% set_card_order (T, ONum + 1, CardOption, PlayerPid, ODeck ++ [{{PlayerPid, ONum, H}, CardOption}]).
% ------------------------------------------------------------------------------ %

% ------------------ ส่วนของการทำงาน ของการ Initialize Data --------------------- %
reply_play_order (YingChubData, S, [Data], PlayerLists) ->
	%%smo_logger:fmsg("YingChubData = ~w", [YingChubData]),
	case check_win_choose (YingChubData, S, Data, PlayerLists) of
		{permitted, PlayerPid} ->
			update_game_data (YingChubData, S, Data, PlayerLists, PlayerPid);
		_ ->
			io:format ("Selected Player not permitted to choose~n")
	end.

update_game_data (YingChubData, S, Data, PlayerLists, PlayerPid) ->
	case mnesia_play:get_game_data(self()) of
		{ok, {GameState, _, _, MaxMp, MaxShrine, PList, LPid, ContAbility, DeckAbi}} ->
			mnesia_play:set_game_data(self(), GameState, PlayerPid, [], MaxMp, MaxShrine, PList, LPid, ContAbility, DeckAbi),
			update_n_replys(YingChubData, S, Data, PlayerLists, MaxMp, MaxShrine);
		{error} ->
			io:format ("[room play] Get game data for update error~n")
	end.

% เช็คว่าผู้ที่เป่ายิงฉุบชนะเลือกที่จะเล่นก่อนหรือหลัง
check_win_choose ([], _, _, _) -> {not_permitted};
check_win_choose ([H|T], S, Data, PlayerLists) ->
	case H of
		{S, win} ->
			{permitted, set_player_turn (S, Data, PlayerLists)};
		_ -> 
			check_win_choose (T, S, Data, PlayerLists)
	end.

set_player_turn (S, Data, PlayerLists) ->
	case Data of
		0 ->	set_loser_turn (S, PlayerLists);
		1 ->	set_winer_turn (S, PlayerLists)
	end.

set_winer_turn (S, [{PlayerPid, _} | T]) ->
	case PlayerPid of
		S -> PlayerPid;
		_ -> set_winer_turn (S, T)
	end.

set_loser_turn (S, [{PlayerPid, _} | T]) ->
	case PlayerPid of
		S -> set_loser_turn (S, T);
		_ -> PlayerPid
	end.

get_reply_data ([], _, _, ReplyData) -> ReplyData;
get_reply_data ([{PlayerPid, _}|T], S, Data, ReplyData) ->
	case mnesia_play:get_player_data(PlayerPid) of
		{ok, {PlayerStatus, FTurn, Seal, Mystic, Hand, Arena, Support, ShrineCards, RemoveCards, ShrineLV, _, HandMax, PlayerFx}} ->
			%smo_logger:fmsg("PlayerStatus 	= ~w",[PlayerStatus]),
			%smo_logger:fmsg("FTurn        	= ~w",[FTurn]),
			%smo_logger:fmsg("Seal     		= ~w",[Seal]),
			%smo_logger:fmsg("Mystic 		= ~w",[Mystic]),
			%smo_logger:fmsg("Hand			= ~w",[Hand]),
			%smo_logger:fmsg("Arena			= ~w",[Arena]),
			%smo_logger:fmsg("ShrineCards	= ~w",[ShrineCards]),
			%smo_logger:fmsg("RemoveCards	= ~w",[RemoveCards]),
			%smo_logger:fmsg("ShrineLV		= ~w",[ShrineLV]),
			update_player_data (PlayerPid, S, Data, PlayerStatus, FTurn, Seal, Mystic, Hand, Arena, Support, ShrineCards, RemoveCards, ShrineLV, HandMax, PlayerFx, T, ReplyData);
		Result ->
			io:format ("[room play] Get player data for update error from result is ~p~n", [Result])
	end.

% ทำการอัพเดทข้อมูลผู้เล่น
update_player_data (PlayerPid, S, Data, PlayerStatus, FTurn, Seal, Mystic, Hand, Arena, Support, ShrineCards, RemoveCards, ShrineLV, HandMax, PlayerFx, T, ReplyData) ->
	case PlayerPid of
		S ->	PD = [flatlength(Seal), flatlength(Mystic), flatlength(Hand), flatlength(Arena), flatlength(ShrineCards), ShrineLV, get_mp(Data), cards_id_two_byte(Hand)],
			mnesia_play:set_player_data (PlayerPid, PlayerStatus, FTurn, Seal, Mystic, Hand, Arena, Support, ShrineCards, RemoveCards, ShrineLV, get_mp(Data), HandMax, PlayerFx),
			get_reply_data (T, S, Data, PD ++ ReplyData);
		_ ->	PD = [flatlength(Seal), flatlength(Mystic), flatlength(Hand), flatlength(Arena), flatlength(ShrineCards), ShrineLV, get_mp(opponent_data(Data)), cards_id_two_byte(Hand)],
			get_reply_data (T, S, Data, ReplyData ++ PD)
	end.

update_n_replys ([], _, _, _, _, _) -> {all_reply};
update_n_replys ([H|T], PlayerPid, Data, PlayerLists, MaxMp, MaxShrine) ->
	case H of
		{PlayerPid, _} ->
			%smo_logger:fmsg("************> update and reply to ~w",[PlayerPid]),
			update_n_reply (PlayerLists, PlayerPid, Data, MaxMp, MaxShrine);
		{Other, _} ->
			%smo_logger:fmsg("************> update and reply to ~w",[Other]),
			update_n_reply (PlayerLists, Other, opponent_data(Data), MaxMp, MaxShrine)
	end,
	update_n_replys (T, PlayerPid, Data, PlayerLists, MaxMp, MaxShrine).

update_n_reply (PlayerLists, PlayerPid, Data, MaxMp, MaxShrine) ->
	ReplyData = get_reply_data (PlayerLists, PlayerPid, Data, []),
	SendReply = [16#83, 16#03, Data, MaxMp, MaxShrine, ReplyData],
	%smo_logger:fmsg("Reply 8303 Data = ~w",[Data]),
	%smo_logger:fmsg("Reply 8303 MaxMp = ~w",[MaxMp]),
	%smo_logger:fmsg("Reply 8303 MaxShrine = ~w",[MaxShrine]),
	%smo_logger:fmsg("Reply 8303 ReplyData = ~w",[ReplyData]),
	gen_server:cast(PlayerPid, {send, SendReply}).

cards_id_two_byte (CardList) ->
	transform (CardList, []).

transform ([], TBCardList) -> TBCardList;
transform ([{_, Order, CardID} | T], TBCardList) ->
	transform (T, TBCardList ++ [Order, <<CardID:16>>]);
transform ([{{_, Order, CardID}, _} | T], TBCardList) ->
	transform (T, TBCardList ++ [Order, <<CardID:16>>]).

get_mp (Data) ->
	case Data of
		0 -> 8;
		1 -> 5
	end.

opponent_data (Data) ->
	case Data of
		0 -> 1;
		1 -> 0
	end.
% ------------------------------------------------------------------------------ %