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
-module (ability_utility).

-import(server_lib, [send/2, controller/2]).
-import(lists, [foreach/2]).

-export ([
						get_update_data/2, 
						select_ability_target/9,
						update_ability_target/4
						%check_ability_select_target/3,
						%check_update_ability_target/1, 
						%activate_card_ability_affect/3,
						%activate_select_ability_affect/1, 
						%send_activate_select_affect/2 
						%select_card_ability_affect/3
						]).

get_update_data (_, []) -> [];
get_update_data (PlayerPid, [{PlayerPid, CardOrder, CardID} | T]) -> [1, CardOrder, <<CardID:16>>] ++ get_update_data (PlayerPid, T);
get_update_data (PlayerPid, [{_, CardOrder, CardID} | T]) -> [0, CardOrder, <<CardID:16>>] ++ get_update_data (PlayerPid, T).

select_ability_target(PlayerList, PlayerPid, CardOwner, CardOrder, CardID, TargetAmountType, TargetAmount, DisplayCode, TargetAbility) ->
	foreach(fun({Pid, _}) ->		
		TargetSize = lists:flatlength(TargetAbility),
		%MsgHeader = [16#88, 16#0c],
		OwnerAbility = [CardOrder, <<CardID:16>>],
		case TargetAmountType of
			1 -> TargetReply = [1] ++ [TargetAmount] ++ [TargetSize] ++ get_update_data(PlayerPid, TargetAbility);
			_ -> TargetReply = [0] ++ [TargetAmount] ++ [TargetSize] ++ get_update_data(PlayerPid, TargetAbility)
		end,
		case Pid of
			PlayerPid -> gen_server:cast(Pid, {send, [16#88, 16#0c, 1, OwnerAbility, TargetReply]});
			_ ->	gen_server:cast(Pid, {send, [16#88, 16#0c, 0, OwnerAbility]})				
		end			
	end, PlayerList).
			
update_ability_target(PlayerList, PlayerPid, DisplayCode, [TargetSize | TargetAbility]) ->
	foreach(fun ({Pid, _}) ->
						case Pid of
							PlayerPid -> gen_server:cast(Pid, {send, [16#88, 16#0d, 204]});
							_ ->	gen_server:cast(Pid, {send, [16#88, 16#0d, DisplayCode, TargetSize] ++ TargetAbility})
						end
					end, PlayerList).
		
% ส่งไปบอกผู้เล่นให้ทำการเลือกเป้าหมายของ ความสามารถ
% check_ability_select_target (PlayerPid, CardOrder, CardID) ->
	% TargetAmountType = ability_affect:get_card_effect (PlayerPid, CardOrder, CardID, target_amount_type),
	% TargetAmount = ability_affect:get_card_effect (PlayerPid, CardOrder, CardID, target_amount),
	% TargetAbility = ability_affect:get_card_effect (PlayerPid, CardOrder, CardID, target_ability),
	% io:format("~n TargetAbility ~p~n", [TargetAbility]),
	% stack_pool:set_stack_option (self(), card_select_target_ability, {PlayerPid, CardOrder, CardID}),
	% gen_server:cast(self(), {select_ability_target, PlayerPid, CardOrder, CardID, TargetAmountType, TargetAmount, TargetAbility}).

% อัพเดท เป้าหมายของ ความสามารถ ให้ผู้เล่น
% check_update_ability_target (PlayerPid) ->
	% {ok, {PlayerPid, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
	% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% case card_utility:check_card_status (PlayerPid, CardOrder, CardID, have_ability_target, Zone) of
		% {ok, have_status} ->
			% TargetAbility = ability_affect:get_card_effect (PlayerPid, CardOrder, CardID, target_ability),
			% gen_server:cast(self(), {update_ability_target, TargetAbility});
		% {ok, have_no_status} -> gen_server:cast(self(), {act_select_line, PlayerPid})
	% end.

% % ส่งไปบอกเพื่อให้เลือกการ์ดที่ต้องการให้เกิดผลก่อนใบอื่น
% activate_select_ability_affect (PlayerPid) ->
	% CardsActivateAbility = stack_pool:get_last_stack (self(), cards_activate_ability),
% %	io:format ("CardsActivateAbility ~p~n", [CardsActivateAbility]),
	% case CardsActivateAbility of
		% {ok, []} -> interfere_step:return_play();
		% {ok, Cards} ->
			% case Cards of
				% [{PlayerPid, CardOrder, CardID}] ->
					% gen_server:cast(self(), {activate_card_ability_affect, PlayerPid, CardOrder, CardID});
				% _ ->	ActivateMsg = activate_select_card_affect (0, Cards),
					% gen_server:cast(self(), {activate_select_card_affect, PlayerPid, ActivateMsg})
			% end
	% end.

% activate_select_card_affect (CardSize, []) -> [CardSize];
% activate_select_card_affect (CardSize, [{_, CardOrder, CardID} | Cards]) ->
	% activate_select_card_affect (CardSize + 1, Cards) ++ [CardOrder, <<CardID:16>>].

% send_activate_select_affect (PlayPid, ActivateMsg) ->
	% {ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	% foreach (	fun ({PlayerPid, _}) ->
		% case PlayerPid of
			% PlayPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#31, 1] ++ ActivateMsg});
			% _ -> gen_server:cast(PlayerPid, {send, [16#88, 16#31, 0]})
		% end
	% end, PlayerList).

% select_card_ability_affect (PlayerPid, PlayPlayerPid, Data) ->	
	% case PlayPlayerPid of
		% PlayerPid ->
			% case list_to_binary (Data) of
				% <<CardOrder, CardID:16>> ->
					% gen_server:cast(self(), {activate_card_ability_affect, PlayerPid, CardOrder, CardID});
				% _ ->	io:format("Send select ability affect data error <~p>~n", [Data])
			% end;
		% _ ->	play_utility:out_of_turn (PlayerPid, select_card_ability_affect)
	% end.

% activate_card_ability_affect (CardOwner, CardOrder, CardID) ->
	% {ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	% {ok, Cards} = stack_pool:get_last_stack (self(), cards_activate_ability),
	% CardsUpdate = Cards -- [{CardOwner, CardOrder, CardID}],
	% stack_pool:set_stack_option (self(), cards_activate_ability, CardsUpdate),
	% stack_pool:set_stack_option (self(), card_ability_affect, {CardOwner, CardOrder, CardID}),
	% foreach (	fun ({PlayerPid, _, _}) ->
		% case PlayerPid of
			% CardOwner ->
				% gen_server:cast(PlayerPid, {send, [16#88, 16#32, 1, CardOrder, <<CardID:16>>]});
			% _ ->	gen_server:cast(PlayerPid, {send , [16#88, 16#32, 0, CardOrder, <<CardID:16>>]})
		% end
	% end, PlayerList).
