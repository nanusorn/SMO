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
-module(smo_supervisor).
-behaviour(supervisor).
-created_by('Scrooge McDuck at playpal.co.th').
%%-export([start_in_shell_for_testing/0, start_link/0, init/1]).
-export([start_link/0, init/1]).

%start_in_shell_for_testing() ->
%	{ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
%	unlink(Pid).
	
start_link() ->
	smo_logger:print_msg("smo_supervisor:start_link()"),
	supervisor:start_link({local, smo_supervisor}, smo_supervisor, []).
	
init(_Args) ->		
	smo_logger:print_fmsg("smo_supervisor: initializing, pid=~w", [self()]),
	% give error logger 10 seconds to shut down
	ErrorLoggerSpec = {smo_logger
		, {smo_logger, start_link, []}
		, permanent, 10, worker, [smo_logger]},
	PlayerSupervisorSpec = {smo_player_sup
		, {smo_player_sup, start_link, []}
		, permanent, infinity, supervisor, [smo_player_sup]},
	ArenaSupervisorSpec = {smo_arena_sup
		, {smo_arena_sup, start_link, []}
		, permanent, infinity, supervisor, [smo_arena_sup]},
	ArenaGuardianSpec = {smo_arena_guardian
		, {smo_arena_guardian, start_link, []}
		, permanent, brutal_kill, worker, [smo_arena_guardian]},
	PlayerGuardianSpec = {smo_player_guardian
		, {smo_player_guardian, start_link, []}
		, permanent, brutal_kill, worker, [smo_player_guardian]},
	DispatcherSpec = {smo_dispatcher
		, {smo_dispatcher, start_link, []}
		, permanent, brutal_kill, worker, [smo_dispatcher]},
	DatabaseSpec = {smo_database
		, {smo_database, start_link, []}
		, permanent, brutal_kill, worker, [smo_database]},
	AcceptorSupervisorSpec = {smo_acceptor_sup
		, {smo_acceptor_sup, start_link, []}
		, permanent, infinity, supervisor, [smo_acceptor_sup]},
	% FriendSpec = {smo_friend
		% , {smo_friend, start_link, []}
		% , permanent, brutal_kill, worker, [smo_friend]},
	IntervalSpec = {smo_interval_activity
		, {smo_interval_activity, start_link, []}
		, permanent, brutal_kill, worker, [smo_interval_activity]},
	RankinSpec = {smo_ranking_guardian
		, {smo_ranking_guardian, start_link, []}
		, permanent, brutal_kill, worker, [smo_ranking_guardian]},
	ChildSpecs = [ErrorLoggerSpec, 
	  			  %FriendSpec,
				  IntervalSpec,
				  DispatcherSpec, 
				  DatabaseSpec,
				  AcceptorSupervisorSpec, 
				  PlayerSupervisorSpec,
				  PlayerGuardianSpec,
				  ArenaSupervisorSpec,
  				  ArenaGuardianSpec,
				  RankinSpec
				  ],
	ok = supervisor:check_childspecs(ChildSpecs),
	StartSpecs = {{one_for_one, 10, 10}, ChildSpecs},
	smo_logger:print_msg("smo_supervisor:init() returning"),
	{ok, StartSpecs}.
	