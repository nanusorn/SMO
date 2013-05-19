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
-module(smo_acceptor_sup).
-behaviour(supervisor_bridge).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule exports
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([terminate/2]).
-export([init/1]).

start_link() -> 
	supervisor_bridge:start_link({local, smo_acceptor_sup}, smo_acceptor_sup, []).

init(_Args) ->
	smo_logger:fmsg("acceptor_sup: initializing, pid=~w", [self()]),
	{ok, AcceptorPid} = smo_acceptor:start(),
	{ok, AcceptorPid, AcceptorPid}.

terminate(Reason, State) ->
	AcceptorPid = State,
	smo_logger:fmsg("acceptor_sup: terminating, reason=~w", [Reason]),
	exit(AcceptorPid, Reason).
