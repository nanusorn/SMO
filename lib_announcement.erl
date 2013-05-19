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
-module(lib_announcement).

-export([request_announcement/1]).

request_announcement(LoginName) ->
	SQLCommand = "CALL smno.sp_get_announce()",
	QResult = lib_database:get_query(SQLCommand),
	%[{selected, _, Result}, _] = QResult,
	%[A|T] = QResult,
	%io:format ("5555~p~n",[Result]).
	case QResult of
		[{selected, _, Result}, _] ->
			%DataDeck = get_data_decks(Result, [], ActiveDeckOrder),
			MessageCount = lists:flatlength(Result),
			%io:format ("-----------5555~p~n",lists:flatlength(Result));
			AnnouncementList = get_announcement_list(Result),
			gen_server:cast(LoginName, {send, [16#01, 16#e0, MessageCount] ++ AnnouncementList});
		_Error -> 
			smo_logger:msg("Request all decks error")
	end.
get_announcement_list([]) -> [];
get_announcement_list([{Message,Status}|Tail]) -> MessageLength = lists:flatlength(Message),
						[<<MessageLength:16>>,Message,Status] ++ get_announcement_list(Tail).
