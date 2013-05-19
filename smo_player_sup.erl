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
-module(smo_player_sup).
-behaviour(supervisor).
-created_by('Scrooge McDuck @PlayPal.co.thailand').

%% user interface
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).

start_link() ->
	smo_logger:msg("player_sup:start_link()"),
	supervisor:start_link({local, smo_player_sup}, smo_player_sup, []).

init(Args) ->
	smo_logger:fmsg("player_sup:init(~w)", [Args]),
	PlayerSpec = {smo_player, {smo_player, start_link, []}
		, temporary, brutal_kill, worker, [smo_player]},
	StartSpecs = {{simple_one_for_one, 0, 1}, [PlayerSpec]},
	smo_logger:msg("player_sup:init() returning"),
	{ok, StartSpecs}.
