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
-module (set_growth).

-include_lib ("stdlib/include/qlc.hrl").
-include ("play_record.hrl").

-import(server_lib, [send/2, controller/2]).
-import (mnesia_table, [do/1]).
-import (lists, [foreach/2, sum/1, append/2, min/1, flatlength/1, split/2]).

-compile (export_all).

check_growth_ability () ->
	{ok, {PlayerPid, CardOrder , CardID, _}} = stack_pool:get_last_stack(self()),
	stack_pool:set_stack_option (self(), play, check_growth_ability),
	case search_growth:get_growth_data(CardID) of
		{ok, _} -> 
			case search_growth:get_growth_option(PlayerPid, CardOrder, CardID) of
				{can_growth, _} ->
					gen_server:cast(self(), {select_card_growth, PlayerPid});
				{can_not_growth} ->
					interfere_step:return_play(check_play_step)
			end;
		{error, _} -> interfere_step:return_play(check_play_step)
	end.

player_select_growth (PlayerPid, GrowthSelected) ->
	{ok, {PlayerPid, CardOrder , CardID, _}} = stack_pool:get_last_stack(self()),
	stack_pool:set_stack_option (self(), growth_selected, GrowthSelected),
	stack_pool:set_stack_option (self(), player_select_activation_ability, casting_card),
		case GrowthSelected of
			1 ->
				{ _, GrowthOption} = search_growth:get_growth_option(PlayerPid, CardOrder, CardID),
				SetSize = lists:flatlength(GrowthOption), 
					case SetSize of
						0 -> 
							interfere_step:return_play(check_play_step);
						_ ->
							CompleteOption = get_option_reply (GrowthOption),
							gen_server:cast(self(), {update_select_growth, PlayerPid, [CardOrder, <<CardID:16>>]++[SetSize]++CompleteOption, GrowthOption})
					end;
			0 -> 
				 interfere_step:return_play(check_play_step)
		end.
	
select_growth_option (PlayerPid, OptionChoose, GrowthOption) ->
	case search_option (OptionChoose, GrowthOption) of
		{OptionNumber, Growth_Option} ->
			stack_pool:set_stack_option (self(), growth_option, {OptionNumber, Growth_Option}),
			case check_growth_option (PlayerPid, OptionNumber, Growth_Option) of
				{no_support_set} ->
					gen_server:cast(self(), {rejected_growth, PlayerPid});
				{ok, GrowthSendData, GrowthCheckData} ->
					gen_server:cast(self(), {act_growth_set_option, [16#88, 16#39], GrowthSendData, GrowthCheckData})
			end;
		ok ->
			stack_pool:pop_stack_out (self())
	end.
	
check_growth_option (CardOwner, OptionNumber, Growth_Option) ->
	{ok, {_, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
	search_growth:get_set_support_list (CardOwner, CardOrder, CardID, OptionNumber, Growth_Option).

search_option ([OptionNumber], []) ->
	io:format("-combination- Option ~p not found~n", [OptionNumber]);
search_option ([OptionNumber], [{OptionNumber, SubSealList}|_]) ->
	{OptionNumber, SubSealList};
search_option ([OptionNumber], [_|T]) ->
	search_option ([OptionNumber], T).

get_option_reply ([]) -> [];
get_option_reply ([{OptionNumber, SupportList} | T]) ->
			[OptionNumber] ++ get_option_reply (T).


%get_option_reply ([]) -> [];
%get_option_reply ([{OptionNumber, SupportSize, SupportList} | T]) ->
%	case SupportList of
%		[[{Group, Data}]] ->
%			SizeSort = length(SupportList)*3,
%			case Group of
%				elem -> 
%					Kind = 1,
%					Value = <<Data:16>>;
%				naming -> 
%					Kind = 2,
%					Value = check_card_type:get_naming (Data);
%				type -> 
%					Kind = 3,
%					Value = check_card_type:get_type (Data);
%				name ->
%					Kind = 4,
%					Value = check_card_type:get_name (Data);
%				level ->
%					Kind = 5,
%					case Data of
%						{_, Num} ->
%							Value = <<Num:16>>;
%						_ ->
%							Value = <<Data:16>>
%					end
%			end,
%			[OptionNumber, SupportSize, SizeSort, Kind, Value] ++ get_option_reply (T);
%
%		[[{Group, Data}|More]|Tail] ->
%			[OptionNumber, SupportSize] ++ get_option_other ([[{Group, Data}|More]|Tail])++ get_option_reply (T)
%	end.
%	
%get_option_other(N) ->
%	get_option_other(N, []).
%get_option_other([[]|Tail], Lists) ->
%	SizeSort = round(length(Lists)*3/2),
%	[SizeSort] ++ Lists ++ get_option_other(Tail, []);
%get_option_other([[{Group, Data}|More]|Tail], Lists) ->
%	case Group of
%		elem -> 
%			Kind = 1,
%			Value = <<Data:16>>;
%		naming -> 
%			Kind = 2,
%			Value = check_card_type:get_naming (Data);
%		type -> 
%			Kind = 3,
%			Value = check_card_type:get_type (Data);
%		name ->
%			Kind = 4,
%			Value = check_card_type:get_name (Data);
%		level ->
%			Kind = 5,
%			case Data of
%				{_, Num} ->
%					Value = <<Num:16>>;
%				_ ->
%					Value = <<Data:16>>
%			end
%	end,
%	GrowthData = [Kind, Value],
%	get_option_other([More|Tail], Lists ++ GrowthData);
%get_option_other([], []) -> []

player_select_growth_set (CardOwner, GrowthSelect, GrowthCheck) ->
	{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	stack_pool:set_stack_option (self(), growth_set_check, {GrowthSelect, GrowthCheck}),
	set_growth_status (CardOwner, CardOrder, CardID).

set_growth_status (PlayerPid, CardOrder, CardID) ->
	case get(re_select) of
		undefined ->
			stack_pool:set_stack_option (self(), play, check_growth_ability),
			interfere_step:return_play (check_play_step);
		_ ->
			erase(re_select),
			check_support (PlayerPid, CardOrder, CardID)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%
%หลัง interfere การเลือกline ลงมาในสนาม
%%%%%%%%%%%%%%%%%%%%%%%%%
check_support (CardOwner, CardOrder, CardID) ->
	{ok, {GrowthSelect, GrowthCheck}} = stack_pool:get_last_stack (self(), growth_set_check),
	{GrowthOption, GrowthCheckList} = split(1, GrowthCheck),
	case get_growth_seal (GrowthSelect, GrowthCheckList) of
		{ok, GrowthSet} ->
			stack_pool:set_stack_option (self(), growth_support, {GrowthOption, GrowthSet}),
			check_support_seal_condition (CardOwner, CardOrder, CardID);
		_ -> io:format ("Growth set not found~n")
	end.

check_support_seal_condition (CardOwner, CardOrder, CardID) ->
	{ok, {_, GrowthSet}} = stack_pool:get_last_stack (self(), growth_support),
	case check_support_seal (GrowthSet) of
		support_ready -> 
			stack_pool:set_stack_option(self(), play, growth_condition_removed),
			remove_zone:move_to_remove_zone (CardOwner, GrowthSet);
		support_not_ready ->
			re_select_growth_option (CardOwner) %เลือก Growth ใหม่กรณ๊เงื่อนไขไม่ถูกต้อง
	end.
	
growth_completed (CardOwner, CardOrder, CardID) ->
	{OptionNumber, Growth_Option} = stack_pool:get_last_stack (self(), growth_option),
	{ok, {_, GrowthSet}} = stack_pool:get_last_stack (self(), growth_support),
	% Growth_Option = {1, [[type, "Dragon"}, {elem4}]]}  
	%GrowthSet = [{<0796.0>, 7, 543}]
	GrowthOption = [Growth_Option, GrowthSet],
	stack_pool:set_stack_option (self(), play, send_update_growth),
	card_utility:add_card_status (CardOwner, CardOrder, CardID, growth, arena_zone),
	card_utility:update_card_option_field (CardOwner, CardOrder, CardID, growth, GrowthOption, arena_zone),
	interfere_step:return_play(check_play_step).

check_support_seal ([]) -> support_ready;
check_support_seal ([{CardOwner, CardOrder, CardID}|T] ) ->
	case card_utility:check_card_zone (CardOwner, CardOrder, CardID) of
		arena_zone ->
			check_support_seal (T);
		_ ->
			support_not_ready
	end.

re_select_growth_option (PlayerPid) ->
	{ok, {OptionNumber, Growth_Option}} = stack_pool:get_last_stack (self(), growth_option),
		case check_growth_option (PlayerPid, OptionNumber, Growth_Option) of
			{no_support_set} ->
				casting_card:activate_into_arena_effect();
				%gen_server:cast(self(), {rejected_growth, PlayerPid});
			{ok, GrowthSendData, GrowthCheckData} ->
				put(re_select, GrowthSendData),
				gen_server:cast(self(), {act_growth_set_option, [16#88, 16#39], GrowthSendData, GrowthCheckData})
		end.

get_growth_seal (_, []) -> {error, growth_set_not_found};
get_growth_seal ([GrowthSelect], [{GrowthSelect, GrowthSet}|_]) -> {ok, tuple_to_list(GrowthSet)};
get_growth_seal ([GrowthSelect], [_|T]) -> get_growth_seal ([GrowthSelect], T).

get_reply_support ([], SealListSize, RSS) -> {SealListSize, RSS};
get_reply_support ([{_, CardOrder, CardID} | T], SealListSize, RSS) ->
	get_reply_support (T, SealListSize+1, append(RSS, [CardOrder, <<CardID:16>>])).	










	
	
	
	
