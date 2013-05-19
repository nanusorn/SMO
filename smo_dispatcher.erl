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

-module(smo_dispatcher).
-behaviour(gen_server).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule exports
-export([start_link/0]).  		% called by supervisor
-export([connected/1]).   		% called by player
-export([disconnected/1]).		%  "
-export([string/2]).    		%  "
-export([update_new_arena/1]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% API used by Player
connected(Pid) -> gen_server:cast(smo_dispatcher, {connected, Pid}).
disconnected(Pid) -> gen_server:cast(smo_dispatcher, {disconnected, Pid}).
string(String, Pid) ->
	{AvatarSet, AvatarID} = lib_lobby_protocol:get_user_data_pid (Pid, avatar),
	FlatString = [AvatarSet ,AvatarID] ++ lists:flatten(String),
	%%%smo_logger:fmsg("pid=~w calls dispatcher:string(\"~s\")", [self(), FlatString]),
	gen_server:cast(smo_dispatcher, {string, FlatString}). % ok
update_new_arena(Param) ->
	gen_server:cast(smo_dispatcher, {broadcast_new_arena, Param}).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%% used to start the dispatcher gen_server
start_link() ->
	ServerName = {local, smo_dispatcher}, % name gets register()ed
	Module = smo_dispatcher,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	%RecipientList = State,
	{Pid,_Tag} = From,
	case Request of
		Other ->
			smo_logger:fmsg("dispatcher: unknown call, from=~w, request=~w", [From, Other]),
			{noreply,State}
	end.

handle_cast(Request, State) ->
	RecipientList = State,
	case Request of
		{connected, Pid} ->
			%false = lists:member(Pid, RecipientList),
			smo_logger:fmsg("dispatcher: new recipient, pid=~w", [Pid]),
			RecipientList2 = RecipientList ++ [Pid],
			{noreply, RecipientList2};
		{disconnected, Pid} ->
			%true = lists:member(Pid, RecipientList),
			smo_logger:fmsg("dispatcher: deleting recipient, pid=~w", [Pid]),
			RecipientList2 = RecipientList -- [Pid],
			{noreply, RecipientList2};
		{string, String} ->
			%smo_logger:fmsg("dispatcher: \"~s\"", [String]),
			%% McDuck said : make message dispatch controller here...
			lists:foreach( fun(Pid) -> smo_player:send_message([16#82, 16#00] ++ String,Pid) end
				, RecipientList),
			{noreply,State};
		{broadcast_new_arena, Param} ->
			%smo_logger:fmsg("broadcast new arena ~w", [Param]),
			lists:foreach( fun(Pid) -> smo_player:send_message(Param, Pid) end, RecipientList),
			{noreply,State};
		Other ->
			smo_logger:fmsg("dispatcher: unknown cast, request=~w", [Other]),
			{noreply,State}
	end.

handle_info(Info, State) -> 
	smo_logger:fmsg("dispatcher: unknown message, info=~w", [Info]),
	{noreply, State}.

init(_Args) ->
	State = [],
	{ok, State}.

% terminate is called if a handle_* call returns stop
% dispatcher is brutally killed by smo_supervisor on shutdown
terminate(Reason, _State) ->
	smo_logger:fmsg("dispatcher: terminating, reason=~w", [Reason]),
	ok.

