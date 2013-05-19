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
-module(smo_report_handler).
-behaviour(gen_event).
-created_by('Scrooge McDuck at Bangkok').

%% gen_event callbacks
-export([code_change/3]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(Request, State) ->
	smo_logger:fmsg("report_handler: unknown call, request=~w", [Request]),
	{ok, noreply, State}.

handle_event(Event, State) ->
	case Event of
	{error_report, _Gleader, _Data} -> smo_logger:fmsg("*** ~w ***", [Event]);
	%%%{error, _Gleader, _Data} -> smo_logger:fmsg("*** ~w ***", [Event]);
	{info_report, _Gleader, _Data} -> smo_logger:fmsg("*** ~w ***", [Event]);
	%%%{info_msg, _Gleader, _Data} -> smo_logger:fmsg("*** ~w ***", [Event]);
	%%%{info, _Gleader, _Data} -> smo_logger:fmsg("*** ~w ***", [Event]);
	_Other -> ok
	end,
	{ok, State}.

handle_info(Info, State) ->
	smo_logger:fmsg("report_handler: unknown message, info=~w", [Info]),
	{noreply, State}.

init(_Args) ->
	smo_logger:msg("report_handler: initializing"),
	State = {},
	{ok, State}.

terminate(Reason, _State) ->
	smo_logger:fmsg("report_handler: terminating, reason=~w", [Reason]),
	ok.
