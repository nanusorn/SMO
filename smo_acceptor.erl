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
-module(smo_acceptor).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule interface
-export([start/0]).

%% internal exports
-export([init/1]).

port_number() ->
	case application:get_env(port) of
	{ok, Port} ->
		smo_logger:fmsg("acceptor: Port specified in app environment: ~w", [Port]),
		Port;
	undefined ->
		smo_logger:msg("acceptor: Port unspecified in app environment. Using default."),
		5679 % default port
	end.

start() ->
	Port = port_number(),
	DefaultOpts = [binary, {nodelay, true}, {packet, 2}, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, DefaultOpts) of
	{ok, LSock} ->
		smo_logger:fmsg("acceptor: listening on port ~B", [Port]),
		Pid = spawn_link(smo_acceptor, init, [LSock]),
		{ok, Pid};
	{error, Reason} -> {error, Reason}
	end.

init(LSock) ->
	smo_logger:fmsg("acceptor: loop initializing, pid=~w, socket=~w"
		, [self(), LSock]),
	loop(LSock).

loop(LSock) ->
	case gen_tcp:accept(LSock) of
	{ok, Socket} ->
				inet:setopts(Socket, 
						[{packet,2},
						binary,
						{nodelay,true},
						{active, false}]),
		smo_logger:fmsg("acceptor: accepted connection, socket=~w", [Socket]), 
		smo_player:start(Socket),
		loop(LSock);
	{error, Reason} ->
		smo_logger:fmsg("acceptor: stopping, reason=~w", [Reason]),
		exit(Reason)
	end.
