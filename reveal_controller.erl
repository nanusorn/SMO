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
-module (reveal_controller).

-import (server_lib, [send/2]).

-compile (export_all).

activate_player_reveal_hand (Receiver, HandRevealor, CardType) ->
	{ok, RevealorHand} = mnesia_play:get_player_data (HandRevealor, hand_cards),
	case CardType of
		seal ->	MatchCards = seal_filter(RevealorHand, []);
		mystic -> MatchCards = mystic_filter(RevealorHand, []);
		_ ->  MatchCards = RevealorHand
	end,
	RevealData = get_reveal_reply_data (MatchCards, 0),
	%io:format("--------------RevealData, ~p~n", [RevealData]),
	gen_server:cast(self(), {update_cards_reveal, Receiver, [1] ++ RevealData}).
	%send_reveal_hand_data (Receiver, HandRevealor, RevealData).

% send_reveal_hand_data ([], _, _) -> [];
% send_reveal_hand_data ([HandRevealor | Receiver], HandRevealor, RevealData) when is_pid (HandRevealor) ->
	% io:format ("Send update reveal data ~p~n", [0]),
	%gen_server:cast(self(), {update_cards_reveal, HandRevealor, 0}),
	% send_reveal_hand_data (Receiver, HandRevealor, RevealData);
% send_reveal_hand_data ([PlayerPid | Receiver], HandRevealor, RevealData) when is_pid (PlayerPid) ->
	% io:format ("Send update reveal data ~p~n", [[1] ++ RevealData]),
	%gen_server:cast(self(), {update_cards_reveal, PlayerPid, [1] ++ RevealData}),
	% send_reveal_hand_data (Receiver, HandRevealor, RevealData).
	 
% CardReveal คือการ์ดที่จะถูก PlayerPid นั้นดู จะดูได้เฉพาะเจ้าของเท่านั้น
activate_consider_card(CardReveal) ->
	RevealData = get_reveal_reply_data(CardReveal, 0),
	[{OwnerPid, _, _}|_] = CardReveal,
	gen_server:cast(self(), {update_cards_reveal, OwnerPid, [1] ++ RevealData}).
% --------------------------------- General Function -------------------------------
get_reveal_reply_data ([], CardSize) -> [CardSize];
get_reveal_reply_data ([{_, _, CardID}| Cards], CardSize) ->
	get_reveal_reply_data (Cards, CardSize + 1) ++ [<<CardID:16>>];
get_reveal_reply_data ([{{_, _, CardID}, _} | Cards], CardSize) ->
	get_reveal_reply_data (Cards, CardSize + 1) ++ [<<CardID:16>>].
	
activate_player_reveal(FstTargetGot, PlayerPid) ->
	RevealData = get_reveal_reply_data(FstTargetGot, 0),
	gen_server:cast(self(), {update_cards_reveal, PlayerPid, [1] ++ RevealData}).
	
seal_filter([Cards|T], Result) ->
	{{_, _, CardID}, _} = Cards,
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> NewResult = Result ++ [Cards];
		_ -> NewResult = Result
	end,
	seal_filter(T,NewResult);
seal_filter([],Result) -> Result.

mystic_filter([Cards|T], Result) ->
	{{_, _, CardID}, _} = Cards,
	case mnesia_odbc:is_seal_card(CardID) of
		is_not_seal -> NewResult = Result ++ [Cards];
		_ -> NewResult = Result
	end,
	mystic_filter(T,NewResult);
mystic_filter([],Result) -> Result.

