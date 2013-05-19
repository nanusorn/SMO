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
-module (casting_controller).

-import(server_lib, [send/2, controller/2]).
-import(lists, [foreach/2, append/2]).

-compile (export_all).

request_cast_card(PlayerPid, Data) ->
	case stack_pool:check_can_play (PlayerPid) of
		{ok, can_play} -> player_request_cast_card (PlayerPid, Data);
		{ok, can_not_play} -> play_utility:out_of_turn (PlayerPid, req_cast_card)
	end.

player_request_cast_card (PlayerPid, Data) ->
	case list_to_binary(Data) of
		<<WhoCard, CardOrder, CardID:16>> ->
			CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
			casting_card:check_card_cast (PlayerPid, CardOwner, CardOrder, CardID);
		_ ->	io:format("Cast data error : ~p~n", [Data])
	end.	

update_card_casting(PlayerList, CardOwner, CardOrder, CardID, MpRest) ->
	foreach (	fun ({PlayerPid, _}) -> 
				case CardOwner of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#05, 1, CardOrder, <<CardID:16>>, MpRest]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#05, 0, CardOrder, <<CardID:16>>, MpRest]})
				end
			end, PlayerList).

player_select_activation_ability (PlayerPid, PlayPid, Data) ->
	io:format ("Select activation ability ~n"),
	case PlayerPid of
		PlayPid -> 
			case Data of
				[ActivationSelected] ->
					ability_activate:player_select_activation_ability (ActivationSelected);
				_ ->	io:format("player_select_activation_ability data error : ~p~n", [Data])
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_activation_ability)
	end.

update_select_activation (PlayerLists, ActivationSelected) ->
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#04, ActivationSelected]})
			end, PlayerLists).

select_card_growth (PlayerList, PlayPid) ->
%	io:format("Activate select card growth ~n"),
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					PlayPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#06, 1]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#06, 0]})
				end
			end, PlayerList).

player_select_growth (PlayerPid, PlayPid, Data) ->
%	io:format("Activate select card growth ~n"),
	case PlayerPid of
		PlayPid -> case Data of
				[GrowthSelected] -> set_growth:player_select_growth (PlayerPid, GrowthSelected);
				_ -> io:format("Cast data error : ~p~n", [Data])
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_growth)
	end.

update_select_growth (PlayerList, PlayPid, Data) ->
%	io:format("Activate select card growth ~n"),
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					PlayPid -> 
						%gen_server:cast(PlayerPid, {send, [16#88, 16#07, 1, GrowthSelected]});
						gen_server:cast(PlayerPid, {send, [16#88, 16#38] ++ [1] ++ [Data]});
					_ -> 
						%gen_server:cast(PlayerPid, {send, [16#88, 16#07, 0, GrowthSelected]})
						gen_server:cast(PlayerPid, {send, [16#88, 16#38, 204]  ++ [Data]})
				end
			end, PlayerList).

update_select_growth (PlayerList, Selected) ->
	foreach (	fun ({PlayerPid, _}) -> gen_server:cast(PlayerPid, {send, [16#88, 16#05, Selected]}) end, PlayerList).

activate_select_line (PlayerList, CardOwner) ->
	foreach (	fun ({PlayerPid, _}) ->
		case CardOwner of
			PlayerPid ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#08, 1]});
			_ ->	
				gen_server:cast(PlayerPid, {send, [16#88, 16#08, 0]})
		end
	end, PlayerList).

select_line_cast(PlayerPid, {PlayPid, ModuleCall}, Data) ->
	io:format ("Player select line ~p ~p~n", [PlayerPid, Data]),
	case PlayerPid of
		PlayPid ->	
			case ModuleCall of
				casting_card -> casting_card:select_line(Data);
				move_to_arena -> move_to_arena:select_line(Data)
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, select_line_cast)
	end.

update_select_line (PlayerList, CardOwner, Line) ->
	foreach(fun ({PlayerPid, _}) ->
%		io:format ("~p Casting seal to ~p~n", [PlayerPid, Line]),
		case PlayerPid of
			CardOwner -> gen_server:cast(PlayerPid, {send, [16#88, 16#09, 1, Line]});
			_ ->	gen_server:cast(PlayerPid, {send, [16#88, 16#09, 0, Line]})
		end
	end, PlayerList).

%activate_select_material_growth (PlayerList, PlayPid, OptionCanSelected) ->
%	foreach (
%		fun ({PlayerPid, _}) ->
%			case PlayerPid of
%				PlayPid -> 
%					gen_server:cast(PlayerPid, {send, [16#88, 16#38, 1] ++ OptionCanSelected});
%				_ -> 
%					gen_server:cast(PlayerPid, {send, [16#88, 16#38]})
%			end
%		end, PlayerList).

player_select_growth_option (PlayerPid, PlayPid, Data) ->
%	io:format("Activate select growth option~n"),
	case PlayerPid of
		PlayPid -> case Data of
				[GrowthOption] -> growth:player_select_growth_option (PlayerPid, GrowthOption);
				_ -> io:format("Growth option data error : ~p~n", [Data])
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_growth_option)
	end.

activate_select_growth_set (PlayerList, PlayPid, GrowthMaterialSet) ->
	foreach (
		fun ({PlayerPid, _}) ->
			case PlayerPid of
				PlayPid -> 
					gen_server:cast(PlayerPid, {send, [16#88, 16#39] ++ GrowthMaterialSet});
				_ -> 
					gen_server:cast(PlayerPid, {send, [16#88, 16#39]})
			end
		end, PlayerList).

%player_select_growth_set (PlayerPid, PlayPid, Data) ->
%	io:format("Activate select growth set~n"),
%	case PlayerPid of
%		PlayPid -> case Data of
%				[SetNumber] -> growth:player_select_growth_set (PlayerPid, SetNumber);
%				_ -> io:format("Growth set data error : ~p~n", [Data])
%			end;
%		_ ->	play_utility:out_of_turn (PlayerPid, player_select_growth_set)
%	end.

send_update_growth (PlayerList, PlayPid, CardOrder, CardID) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					PlayPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#3a, 1] ++ [CardOrder, <<CardID:16>>]});
					 _ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#3a, 0] ++ [CardOrder, <<CardID:16>>]})
				 end
			end, PlayerList).
			
reject_casting_card (PlayerPid, Reason, LockMsg) ->
	io:format ("Reject casting card by reason ~p~n", [Reason]),
	case stack_pool:get_last_stack (self(), play) of
		{ok, PlayStep} ->
			gen_server:cast(PlayerPid, {send, [16#88, 16#ff, 0, Reason]}),
			interfere_step:return_play (PlayStep),
			LockMsg;
		_ ->	
			gen_server:cast(PlayerPid, {send, [16#88, 16#ff, 1, Reason]}),
			[fe]
	end.

