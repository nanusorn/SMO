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
-module (mystic_card_controller).

-import (lists, [foreach/2, append/2]).

-compile (export_all).

act_select_mystic_ability (PlayerLists, PlayPid, Header, Message) ->
	foreach(	fun({PlayerPid, _}) ->
				case PlayerPid of
					PlayPid -> 
						gen_server:cast(PlayerPid, {send, Header ++ Message});
					_ -> 
						gen_server:cast(PlayerPid, {send, Header ++ [16#cc]})
				end
			end, PlayerLists).

select_mystic_ability (PlayerPid, {SelectCase, PlayPid}, Data) ->
	case PlayerPid of
		PlayPid ->	case list_to_binary(Data) of
				<<_CardOrder:8, _CardID:16, AbilitySelect:8>> ->
					case SelectCase of
						paste_to_other -> other_active_effect:select_mystic_ability(AbilitySelect);
						casting -> casting_card:selected_mystic_ability(AbilitySelect);
						move_to_arena -> move_to_arena:selected_mystic_ability(AbilitySelect)
					end;
				_ -> io:format("Select mystic ability data ~p error !!! ~n", [Data])
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, select_mystic_ability)
	end.

update_casting_mystic (PlayerLists, OwnerPid, Header, UpdateData) ->
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					OwnerPid -> 
						gen_server:cast(PlayerPid, {send, Header ++ [1] ++ UpdateData});
					_ ->	
						gen_server:cast(PlayerPid, {send, Header ++ [0] ++ UpdateData})
				end
			end, PlayerLists).

activate_select_mystic_target(PlayerLists, OwnerPid, TargetType, TargetNumber, TargetSize, TargetReply, DisplayCode) ->
	NumberType = 0,
%	io:format ("ASMT ~p ~p ~p ~p ~p~n", [OwnerPid, TargetType, TargetNumber, TargetSize, TargetReply]),
	foreach (	fun ({PlayerPid, _}) -> 
				case PlayerPid of
					OwnerPid ->
						case TargetType of
							0 ->	
								gen_server:cast(PlayerPid, {send, [16#88, 16#52] ++ [TargetType] ++ [NumberType] ++ [TargetNumber] ++ [TargetSize] ++ TargetReply++[DisplayCode]});
							1 ->	
								gen_server:cast(PlayerPid, {send, [16#88, 16#52] ++ [TargetType] ++ [NumberType] ++ [TargetNumber]++[DisplayCode]});
							_ ->	
								io:format("Target type error ~p~n", [TargetType])
						end;
					_ ->	gen_server:cast(PlayerPid, {send, [16#88, 16#52, 16#cc]})
				end
			end, PlayerLists).
		
player_select_mystic_target (PlayerPid, Data) ->
	case stack_pool:check_can_play (PlayerPid) of
		{ok, can_play} ->
			case list_to_binary(Data) of
				<<WhoCard:8, CardOrder:8, CardID:16>> ->
					CardOwner = play_utility:get_owner_pid(PlayerPid, WhoCard),
					case WhoCard of
						0 ->	player_select_mystic_target (CardOwner, CardOrder, CardID);
						1 ->	player_select_mystic_target (CardOwner, CardOrder, CardID);
						100 ->	player_select_mystic_target (CardOwner);
						101 ->	player_select_mystic_target (CardOwner);
						_ ->	io:format ("Select who card ~p out of range ~n", [WhoCard])
					end;
				_ -> io:format("Select ps target data ~p error !!! ~n", [Data])
			end;
		{ok, can_not_play} -> play_utility:out_of_turn (PlayerPid, player_select_mystic_target)
	end.

player_select_mystic_target (PlayerPid, CardOrder, CardID) ->
	case stack_pool:get_last_stack (self(), play) of
		play_move_card_to_arena_5 ->
			move_to_arena:player_select_mystic_target (PlayerPid, CardOrder, CardID);
		_ ->	casting_card:player_select_mystic_target (PlayerPid, CardOrder, CardID)
	end.

player_select_mystic_target (TargetPid) ->
	case stack_pool:get_last_stack (self(), play) of
		play_move_card_to_arena_5 ->
			move_to_arena:player_select_mystic_target (TargetPid, 0, 0);
		_ ->	casting_card:player_select_mystic_target (TargetPid, 0, 0)
	end.

update_select_mystic_target (PlayerLists, UpdateType, TargetType, TargetPid, CardOrder, CardID) ->
	foreach (	fun ({PlayerPid, _}) ->
				case TargetType of
					0 ->	TargetMsg = get_msg_cards_target (PlayerPid, TargetPid, CardOrder, CardID);
					1 ->	case PlayerPid of
							TargetPid -> TargetMsg = [1];
							_ -> TargetMsg = [0]
						end
				end,
				case UpdateType of
					0 -> Msg = [16#88, 16#53, 0, TargetType];
					1 -> Msg = [16#88, 16#53, 1, TargetType] ++ TargetMsg
				end,
				gen_server:cast(PlayerPid, {send, Msg})
			end, PlayerLists).
			
move_mystic_to_other_target(PlayerList, PlayerPid, {CardOwner, CardOrder, CardID}, {PreOwner, PreOrder, PreID}, {TarOwner, TarOrder, TarID}) ->
	foreach(fun({Pid, _}) ->
				[COwner, POwner, TOwner] = check_id(Pid, [CardOwner, PreOwner, TarOwner]),
				gen_server:cast(Pid, 	{send, [16#88, 16#b0, COwner, <<CardOrder:8>>, <<CardID:16>>, POwner, <<PreOrder:8>>, <<PreID:16>>, TOwner, <<TarOrder:8>>, <<TarID:16>>]})
	end, PlayerList).

check_id(_, []) -> [];
check_id(Pid, [CheckPid|Remain]) ->
	case CheckPid of
		Pid -> [1] ++ check_id(Pid, Remain);
		_ -> [0] ++ check_id(Pid, Remain)
	end.
	
	
get_msg_cards_target (PlayerPid, TargetPid, CardOrder, CardID) ->
	case PlayerPid of
		TargetPid -> [1, CardOrder, <<CardID:16>>];
		_ -> [0, CardOrder, <<CardID:16>>]
	end.

update_cast_mystic_success (PlayerLists, Header, MysticOwner, CardOrder, CardID, TPid, TOrder, Tid) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					MysticOwner -> MysticData = [1, CardOrder, <<CardID:16>>];
					_ ->  MysticData = [0, CardOrder, <<CardID:16>>]
				end,
				case PlayerPid of
					TPid -> TargetData = [1, TOrder, <<Tid:16>>];
					_ ->  TargetData = [0, TOrder, <<Tid:16>>]
				end,
				gen_server:cast(PlayerPid, {send, Header ++ MysticData ++ TargetData})
			end, PlayerLists).

activate_select_mystic_option (PlayerLists, PlayPid, MysticFxId, Data) ->
	foreach (fun ({PlayerPid, _}) ->
		case PlayerPid of
			PlayPid ->
				case MysticFxId of
					16#01 ->
						case Data of
							[PlayerPid, CardOrder, CardID] ->
								gen_server:cast(PlayerPid, {send, [16#88, 16#55, MysticFxId] ++ [1, CardOrder, <<CardID:16>>]});
							[_, CardOrder, CardID] ->
								gen_server:cast(PlayerPid, {send, [16#88, 16#55, MysticFxId] ++ [0, CardOrder, <<CardID:16>>]})
						end;
					16#02 ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#55, MysticFxId, 1]});
					_ ->	io:format ("Mystic fx id ~p not prodess ~n", [MysticFxId])
				end;
			_ ->	gen_server:cast(PlayerPid, {send, [16#88, 16#55]})
		end
	end, PlayerLists).

player_select_mystic_effect (PlayerPid, PlayPid, Data) ->
	case PlayPid of
		PlayerPid ->
			mystic_effect:player_select_mystic_effect (PlayerPid, Data);
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_mystic_effect)
	end.

update_select_mystic_fx (PlayerLists, CardOwner, MysticFxId, Data) ->
	foreach (	fun ({PlayerPid, _}) ->
		case PlayerPid of
			CardOwner ->
				case MysticFxId of
					16#01 ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#56, MysticFxId, 1] ++ Data});
					16#02 ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#56, MysticFxId, 1] ++ Data})
				end;
			_ ->	case MysticFxId of
					16#01 ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#56, MysticFxId, 0] ++ Data});
					16#02 ->
						[HSize, SDSize, MDSize, _, _] = Data,
						gen_server:cast(PlayerPid, {send, [16#88, 16#56, MysticFxId, 0, HSize, SDSize, MDSize]})
				end
		end
	end, PlayerLists).