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
-module (stack_pool).

-include_lib ("stdlib/include/qlc.hrl").
-include ("play_record.hrl").

-import (lists, [append/2]).
-import (mnesia_play, [q_transaction/1]).
-import (mnesia_table, [do/1]).

-export ([push_stack/4, push_stack/5, get_last_stack/1, get_last_stack/2, remove_stack/1, get_all_stack/1]).
-export ([set_stack_option/3, pop_stack_out/1, pop_stack/0, pop_stack/1, add_stack_option_field/3]).
-export ([remove_stack_option/2, set_last_stack/3, check_can_play/1, remove_stack_option_field/3]).
-export ([check_stack_size/1]).

%% stack_data = [{player_pid, card_order, card_id, option}, ...]
set_stack (RoomPid, Stack) ->
	Row = #play_stack {	room_pid = RoomPid, play_stack_list = Stack},
	q_transaction (Row).

set_last_stack (RoomPid, Field, Data) ->
	case get_stack (RoomPid) of
		{ok, GameStack} -> 
			GameStackUpdate = set_stack_field_data (GameStack, Field, Data),
			set_stack (RoomPid, GameStackUpdate);
		{error, no_stack} -> {error, no_stack}
	end.

set_stack_field_data ([{PlayerPid, CardOrder, CardID, StackOption} | GameStack], Field, Data) ->
	case Field of
		player_pid -> [{Data, CardOrder, CardID, StackOption}] ++ GameStack;
		card_order -> [{PlayerPid, Data, CardID, StackOption}] ++ GameStack;
		card_id -> [{PlayerPid, CardOrder, Data, StackOption}] ++ GameStack;
		option -> [{PlayerPid, CardOrder, CardID, Data}] ++ GameStack
	end.

remove_stack (RoomPid) ->
	Oid = {play_stack, RoomPid},
	F =	fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

get_stack (RoomPid) ->
	case do(qlc:q([X#play_stack.play_stack_list || X <- mnesia:table(play_stack), X#play_stack.room_pid =:= RoomPid])) of
		[] -> {error, no_stack};
		[Result] ->
%			io:format("Stack result is ~p~n", [Result]),
			case Result of
				[] -> {error, no_stack};
				_ ->	{ok, Result}
			end
	end.

get_field (Stack, Field) ->
	case Stack of
		{PlayerPid, CardOrder, CardID, Option} ->
			case Field of
				player_pid -> {ok, PlayerPid};
				card_order -> {ok, CardOrder};
				card_id -> {ok, CardID};
				option -> {ok, Option};
				_ -> get_option_field (Option, Field)
			end;
		_ ->	io:format ("Stack error from ~p~n", [Stack]),
			{error, stack_option_error}
	end.

get_option_field([], _) -> {error, no_option_field};
get_option_field([{Field, Data}|_], Field) -> {ok, Data};
get_option_field([_|T], Field) -> get_option_field(T, Field).

% Option = [{play, play_status}, {...}, ...}]
% play_status = {casting_seal, casting_mystic, ...}
push_stack (CardOwner, CardOrder, CardID, StackOption) ->
	push_stack (self(), CardOwner, CardOrder, CardID, StackOption).
push_stack (RoomPid, CardOwner, CardOrder, CardID, StackOption) ->
	case get_stack (RoomPid) of
		{ok, Result} ->
			case check_stack_card_player (StackOption) of
				have_card_player ->
					set_stack (RoomPid, [{CardOwner, CardOrder, CardID, StackOption}] ++ Result);
				no_card_player ->
					case stack_pool:get_last_stack (self(), card_player) of
						{ok, PlayerPid} ->
							StackUpdate = [{CardOwner, CardOrder, CardID, StackOption ++ [{card_player, PlayerPid}]}] ++ Result;
						_ ->	StackUpdate = [{CardOwner, CardOrder, CardID, StackOption}] ++ Result
					end,
					set_stack (RoomPid, StackUpdate)
			end;
		{error, no_stack} ->
			set_stack (RoomPid, [{CardOwner, CardOrder, CardID, StackOption}])
	end.

% [{play, skill_effect_affect}, {card_give_effect, CardGive}, {fx_affect, Fx}, {duration, Duration}, {target, TargetSkill}, {skill_id, SkillId}, {card_player, PlayerPid}]
check_stack_card_player ([]) -> no_card_player;
check_stack_card_player ([{card_player, _} | _]) -> have_card_player;
check_stack_card_player ([_ | StackOption]) -> check_stack_card_player (StackOption).

pop_stack () -> pop_stack (self()).
pop_stack (RoomPid) ->
	case get_stack (RoomPid) of
		{ok, [StackPop | StackPush]} ->
			set_stack (RoomPid, StackPush),
			{ok, StackPop};
		{error, no_stack} ->
			{ok, []}
	end.

pop_stack_out (RoomPid) ->
	case get_stack (RoomPid) of
		{ok, [_ | StackPush]} ->
			set_stack (RoomPid, StackPush);
		{error, no_stack} -> no_stack
	end.

get_all_stack (RoomPid) ->
	case get_stack (RoomPid) of
		{ok, Stack} -> {ok, Stack};
		{error, no_stack} -> {error, no_stack}
	end.
	

get_last_stack (RoomPid) ->
	case get_stack (RoomPid) of
		{ok, [LastStack | _]} -> {ok, LastStack};
		{error, no_stack} -> {error, no_stack}
	end.

get_last_stack (RoomPid, Field) ->
	case get_stack (RoomPid) of
		{ok, [LastStack | _]} ->
			get_field (LastStack, Field);
		{error, no_stack} -> {error, no_stack}
	end.

set_last_stack (RoomPid, StackSet) ->
	case StackSet of
		{_, _, _, _} ->
			case get_stack (RoomPid) of
				{ok, [_ | Stack]} ->
					set_stack (RoomPid, [StackSet] ++ Stack);
				{error, no_stack} ->
					{error, no_stack}
			end;
		_ ->	io:format ("Set last stack error from ~p~n", [StackSet])
	end.

set_option ([], OptionField, OptionData) -> [{OptionField, OptionData}];
set_option ([{OptionField, _}|T], OptionField, OptionData) -> [{OptionField, OptionData}] ++ T;
set_option ([H|T], OptionField, OptionData) -> [H] ++ set_option (T, OptionField, OptionData).

remove_option ([], _) -> [];
remove_option ([{OptionField, _} | T], OptionField) -> T;
remove_option ([H | T], OptionField) -> [H] ++ remove_option (T, OptionField).

set_stack_option (RoomPid, OptionField, OptionData) ->
%	io:format("<< SET --- ~p ~p --->>~n", [OptionField, OptionData]),
	case get_last_stack (RoomPid) of
		{ok, Stack} ->
			{PlayerPid, CardOrder, CardID, Option} = Stack,
			OptionUpdate = set_option (Option, OptionField, OptionData),
			set_last_stack (RoomPid, {PlayerPid, CardOrder, CardID, OptionUpdate});
		{error, no_stack} ->
			no_stack_append
	end.

remove_stack_option (RoomPid, OptionField) ->
	case get_last_stack (RoomPid) of
		{ok, Stack} ->
			{PlayerPid, CardOrder, CardID, Option} = Stack,
			OptionUpdate = remove_option (Option, OptionField),
			set_last_stack (RoomPid, {PlayerPid, CardOrder, CardID, OptionUpdate});
		{error, no_stack} ->
			no_stack_append
	end.

add_stack_option_field (RoomPid, OptionField, OptionData) ->
	case get_last_stack (RoomPid, OptionField) of
		{ok, StackField} ->
			OptionAdd = lists:subtract (OptionData, StackField),
			OptionUpdate = StackField ++ OptionAdd,
%			io:format ("Option update ~p~n", [OptionUpdate]),
			set_stack_option (RoomPid, OptionField, OptionUpdate);
		{error, no_option_field} ->
			set_stack_option (RoomPid, OptionField, OptionData);
		{error, no_stack} ->
			no_stack_append
	end.

remove_stack_option_field (RoomPid, OptionField, OptionData) ->
	case get_last_stack (RoomPid, OptionField) of
		{ok, StackField} ->
			OptionUpdate =
			case is_list(StackField) of
				true -> lists:subtract (StackField, OptionData);
				false -> lists:subtract ([StackField], OptionData)
			end,
			set_stack_option (RoomPid, OptionField, OptionUpdate);
		{error, no_stack} ->
			no_stack_field_remove
	end.

check_can_play (PlayerPid) ->
	case get_last_stack (self(), player_pid) of
		{ok, PlayerPid} -> {ok, can_play};
		_ -> case smo_arena:is_your_turn(PlayerPid) of
				1 -> {ok, can_play};
				0 -> {ok, can_not_play}
			end
	end.

check_stack_size (RoomPid) ->
	case get_stack (RoomPid) of
		{ok, Result} -> lists:flatlength (Result);
		{error, _} -> 0
	end.