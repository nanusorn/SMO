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
-module(destroy_support_seal).
-export([
						support_to_destroy/2,
						each_support_destroy/0,
						support_seal_destroyed/0
					]).
					
support_to_destroy(PlayerPid, DestroySupport) ->
	stack_pool:push_stack(self(), PlayerPid, 0, 0, [{support_destroy, DestroySupport}, {support_to_destroy, DestroySupport}]),
	each_support_destroy().
	
each_support_destroy() ->
	case stack_pool:get_last_stack(self(), support_destroy) of
		{ok, []} -> 
			stack_pool:set_stack_option(self(), play, support_seal_destroy),
			{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
			{ok, ToDestroy} = stack_pool:get_last_stack(self(), support_to_destroy),
			destroy:check_card_destroyed(PlayerPid, ToDestroy, support_destroy);
		{ok, [{Supowner, SubOrder, SubID}|Destroy]} ->
			stack_pool:set_stack_option(self(), support_destroy, Destroy),
			{MainOwner, MainOrder, MainID} = look_for_main_seal(Supowner, SubOrder, SubID),
			separate_seal(MainOwner, MainOrder, MainID),
			card_utility:remove_card_status(MainOwner, MainOrder, MainID, be_combine, arena_zone),
			effect_activate:send_update_activate_effect(MainOwner, MainOrder, MainID, [], update)
	end.
	
support_seal_destroyed() ->
	stack_pool:pop_stack_out(self()),
	interfere_step:return_play(check_play_step).
	
look_for_main_seal(SupOwner, SubOrder, SubID) ->
	{ok, Result} = card_utility:get_card_option_field(SupOwner, SubOrder, SubID, card_status, support_cards),
	{value,{support_to, MainOwner, MainOrder, MainID}} = lists:keysearch(support_to, 1, Result),
	{MainOwner, MainOrder, MainID}.
	
separate_seal(CardOwner, CardOrder, CardID) -> 
	{ok, ArenaReply, _MysticSupport} = arena_zone:break_support_seal(CardOwner, CardOrder, CardID),
	case _MysticSupport of
		[] -> 
		%Zone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
		%case card_utility:check_card_status (CardOwner, CardOrder, CardID, combine_with_effect, Zone) of
		%	{ok, have_status} ->
		case force_break:get_mystic_data(arena_zone:get_mystic_pasted(CardOwner, CardOrder, CardID)) of
			{already} ->
				MysticSupport = get(support_target),
				Value = lists:keysearch({CardOwner, CardOrder, CardID}, 1, MysticSupport),
				smo_logger:fmsg("mystic support check to card  ~p ~n", [Value]),
				case Value of
					{value, {{CardOwner, CardOrder, CardID}, OptionNo}} ->
					{ok, PowerChange} =  mnesia_odbc:get_power_change_data(CardID, OptionNo),
					OptionChange = [{combine_support, {{CardOwner, CardOrder, CardID}, {}}}, {combine_power, PowerChange}],
					ArenaCardOption = arena_zone:get_card_option(CardOwner, CardOrder, CardID),
					CardOption =
					case ArenaCardOption of
						{ok, TCardOption} -> TCardOption;
						_ -> []
					end,
					mystic_effect:update_option_effect(CardOwner, CardOrder, CardID, CardOption, OptionChange);
				_ ->""
					end;
			_-> ""
		end;
		[{MOnwerPid, MCardOrder, MCardID}|_] -> 
			MysticSupport = get(support_target),
			Value = lists:keysearch({CardOwner, CardOrder, CardID}, 1, MysticSupport),
			smo_logger:fmsg("mystic support check to card  ~p ~n", [Value]),
			case Value of
				{value, {{CardOwner, CardOrder, CardID}, OptionNo}} ->
					{ok, PowerChange} =  mnesia_odbc:get_power_change_data(CardID, OptionNo),
					OptionChange = [{combine_support, {{MOnwerPid, MCardOrder, MCardID}, {}}}, {combine_power, PowerChange}],
					ArenaCardOption = arena_zone:get_card_option(CardOwner, CardOrder, CardID),
					CardOption =
					case ArenaCardOption of
						{ok, TCardOption} -> TCardOption;
						_ -> []
					end,
					mystic_effect:update_option_effect(CardOwner, CardOrder, CardID, CardOption, OptionChange);
				_ ->""
			end
	end,
	ArenaSize = length(ArenaReply),
	ArenaDataReply = get_data_reply(ArenaReply),
	%{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	stack_pool:set_stack_option(self(), play, attack_to_support_play_break_seal),
	gen_server:cast(self(), {update_arena_cards, CardOwner, [ArenaSize] ++ ArenaDataReply}).
	
get_data_reply([]) -> [];
get_data_reply([{{CardOwner, CardOrder, CardID}, CardOption} | Cards]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			{ok, Line} = seal_card:get_seal_option (CardOption, line),
			stack_pool:add_stack_option_field (self(), break_combine_to_arena_cards, [{CardOwner, CardOrder, CardID}]),
			[CardOrder, <<CardID:16>>, Line] ++ get_data_reply (Cards);
		is_not_seal ->
			get_data_reply (Cards)
	end.