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
-module (using_skill_controller).

-import (lists, [foreach/2]).

-compile (export_all).

request_use_skill (PlayerPid, Data) ->
	case stack_pool:check_can_play (PlayerPid) of
		{ok, can_play} ->
			case list_to_binary(Data) of
				<<WhoCard:8, CardOrder:8, CardID:16>> ->
					CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),					
					check_skill_from (PlayerPid, CardOwner, CardOrder, CardID);
				_ -> io:format("Request use skill data error from data ~p~n", [Data])
			end;
		{ok, can_not_play} ->
			play_utility:out_of_turn (PlayerPid, req_assign_use_skill)
	end.

check_skill_from (PlayerPid, CardOwner, CardOrder, CardID) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			%seal_skill:assign_use_skill (PlayerPid, CardOwner, CardOrder, CardID);
			new_seal_skill:assign_use_skill(PlayerPid, CardOwner, CardOrder, CardID);
		is_not_seal ->
			%seal_skill:assign_use_skill (PlayerPid, CardOwner, CardOrder, CardID)
			new_seal_skill:assign_use_skill(PlayerPid, CardOwner, CardOrder, CardID)
	end.

update_card_use_skill(PlayerLists, PlayerPid, CardOwner, CardOrder, CardID, PlayerMp, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}) ->
	foreach(fun ({Pid, _}) ->
				case Pid of
					PlayerPid -> 
						case Pid of
							% ผู้ใช้ สกิล เป็น เจ้าของการ์ด
							CardOwner -> gen_server:cast(Pid, {send, [16#88, 16#68, 1, CardOrder, <<CardID:16>>, 1, PlayerMp, 204]});
							_ -> gen_server:cast(Pid, {send, [16#88, 16#68, 0, CardOrder, <<CardID:16>>, 1, PlayerMp, 204]})
						end;
					_ -> 
						case Pid of
							%CardOwner -> gen_server:cast(Pid, {send, [16#88, 16#68, WhoCard, CardOrder, <<CardID:16>>, WhoseMp, PlayerMp,  SkillNo]});
							% ฝั่งตรงข้ามผู้ผู้ใช้ สกิล เป็น เจ้าของการ์ด
							CardOwner -> 
								case SkillOwnerPid of
									% การ์ดเจ้าของ สกิล เป็น เจ้าของการ์ด
									CardOwner ->	gen_server:cast(Pid, {send, [16#88, 16#68, 1, CardOrder, <<CardID:16>>, 0, PlayerMp,  1, SkillOwnerOrder, <<SkillOwnerID:16>>, SkillNo]});
									_ -> gen_server:cast(Pid, {send, [16#88, 16#68, 1, CardOrder, <<CardID:16>>, 0, PlayerMp,  0, SkillOwnerOrder, <<SkillOwnerID:16>>, SkillNo]})
								end;
							_ -> %gen_server:cast(Pid, {send, [16#88, 16#68, 0, CardOrder, <<CardID:16>>, 0, PlayerMp, SkillNo]})
								case SkillOwnerPid of
									% การ์ดเจ้าของ สกิล เป็น เจ้าของการ์ด
									CardOwner -> gen_server:cast(Pid, {send, [16#88, 16#68, 0, CardOrder, <<CardID:16>>, 0, PlayerMp, 1, SkillOwnerOrder, <<SkillOwnerID:16>>, SkillNo]});
									_ -> gen_server:cast(Pid, {send, [16#88, 16#68, 0, CardOrder, <<CardID:16>>, 0, PlayerMp, 0, SkillOwnerOrder, <<SkillOwnerID:16>>, SkillNo]})
								end
						end
				end
			end, PlayerLists).

activate_select_skill(PlayerLists, PlayerUseSkill, CardOwner, SkillList) ->
	SkillSize = lists:flatlength(SkillList),
	CardSkill = convert_card_skill(CardOwner, SkillList),
	io:format("all card Skill ~p~n", [CardSkill]),
	foreach(fun ({PlayerPid, _}) ->
				io:format("send to ~p player uses kill ~p~n", [PlayerPid, PlayerUseSkill]),
				case PlayerPid of
					PlayerUseSkill ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#67, SkillSize] ++ CardSkill});
					_ ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#67, 204]})
				end
			end, PlayerLists).

convert_card_skill(_, []) -> [];
convert_card_skill(CardOwner, [{CardOwner, CardOrder, CardID, SkillNo}|CardSkill]) ->
	[1, CardOrder, <<CardID:16>>, SkillNo] ++ convert_card_skill(CardOwner, CardSkill);
convert_card_skill(CardOwner, [{_, CardOrder, CardID, SkillNo}|CardSkill]) ->
	[0, CardOrder, <<CardID:16>>, SkillNo] ++ convert_card_skill(CardOwner, CardSkill).
			
			
player_select_skill(PlayerPid, {PlayerUseSkill, CardOwner}, Data) ->
	case PlayerPid of
		%PlayerUseSkill -> seal_skill:player_select_skill(PlayerPid, Data);
		PlayerUseSkill -> 
			ReverseData = select_skill_no(Data, CardOwner),
			new_seal_skill:player_select_skill(PlayerPid, CardOwner,  ReverseData);
		%CardOwner -> seal_skill:player_select_skill(PlayerPid, Data);
		_ -> play_utility:out_of_turn (PlayerPid, player_select_skill)
	end.

select_skill_no([WhoCard, CardOrder, CardSet, CardNo, No], CardOwner) ->
	<<CardOrder:8, CardID:16>> = list_to_binary([CardOrder, CardSet, CardNo]),
	case WhoCard of
		1 -> [{CardOwner, CardOrder, CardID, No}];
		_ ->
			OppPid = mnesia_play:get_opponent_pid(CardOwner),
			[{OppPid, CardOrder, CardID, No}]
	end.
	
% update_player_select_skill (PlayerLists, SkillUser, SkillNumber) ->
	% foreach (	fun ({PlayerPid, _}) ->
							% case PlayerPid of
								% SkillUser -> 
									% gen_server:cast(PlayerPid, {send, [16#88, 16#68, 204]});
								% _ -> 
									% gen_server:cast(PlayerPid, {send, [16#88, 16#68, SkillNumber]})
							% end
						% end, PlayerLists).

update_player_mp (PlayerLists, MpOwner, MpRest) ->
	foreach (	fun ({PlayerPid, _}) ->
							case PlayerPid of
								MpOwner -> gen_server:cast(PlayerPid, {send, [16#88, 16#69, 1, MpRest]});
								_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#69, 0, MpRest]})
							end
						end, PlayerLists).

activate_select_skill_target(PlayerLists, SkillUser, TargetType, DialogCode, AmountType, Amount, TargetReply) ->	
	foreach (	fun ({PlayerPid, _}) ->
							case PlayerPid of
								SkillUser ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#6a, TargetType, DialogCode, AmountType, Amount] ++ TargetReply});
								_ ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#6a, 204]})
							end
						end, PlayerLists).

player_select_target_skill(PlayerPid, SkillUser, Data) ->
	case PlayerPid of
		SkillUser ->
			TargetList = selected_list_operate(PlayerPid, Data),
			case stack_pool:get_last_stack(self(), pre_set_seal_use_skill_loop) of
				{ok, reveal_power} -> skill_utility:add_select_seal_power_to_fx(TargetList);
				{ok, select_player} -> skill_utility:target_player(TargetList);
				{ok, swap_skill_target} -> skill_utility:select_seal_swap_skill_target(TargetList);
				%_ -> seal_skill:player_select_target_skill (PlayerPid, TargetList)
				_ -> new_seal_skill:player_select_target_skill(PlayerPid, TargetList)
			end;
		_ -> play_utility:out_of_turn (PlayerPid, player_select_target_skill)
	end.
	
activate_seal_skill_animation(PlayerList, CardOwner, CardOrder, CardID) ->
	lists:foreach(fun({Pid, _}) ->
		case Pid of
			CardOwner -> gen_server:cast(Pid, {send, [16#88, 16#6e, 1, CardOrder, <<CardID:16>>]});
			_ -> gen_server:cast(Pid, {send, [16#88, 16#6e, 0, CardOrder, <<CardID:16>>]})
		end 
	end, PlayerList).
	
server_select_target(PlayerPid, PlayerSelect, Data) ->
	case PlayerPid of
		PlayerSelect ->
			TargetList = selected_list_operate(PlayerPid, Data),
				special_server_function:select_seal_to_hand(PlayerSelect, TargetList);
		_ -> play_utility:out_of_turn(PlayerPid, server_select_target)
	end.

%player_select_target_skill (PlayerPid, SkillUser, Data) ->
	%case PlayerPid of
		%SkillUser ->
			%TargetList = selected_list_operate(PlayerPid, Data),
			%seal_skill:player_select_target_skill (PlayerPid, TargetList);
		%_ -> play_utility:out_of_turn (PlayerPid, player_select_target_skill)
	%end.
	
selected_list_operate(PlayerPid, [A, B, C, D|Tail]) ->
	%io:format('++++++++++ ~p ~p ~p ~p ~p~n', [A, B, C, D, Tail]),
	case [A, B, C, D] of
		[1, 0, 0, 0] -> [PlayerPid];
		[0, 0, 0, 0] -> [mnesia_play:get_opponent_pid(PlayerPid)];
		_ ->
			case list_to_binary ([A, B, C, D]) of
				<<WhoCard:8, CardOrder:8, CardID:16>> ->
					TargetOwnerPid = play_utility:get_owner_pid (PlayerPid, WhoCard),
					[{TargetOwnerPid, CardOrder, CardID}]++selected_list_operate(PlayerPid, Tail)
			end
	end;
selected_list_operate(_, []) -> [].	

activate_player_select_skill_decision(PlayerLists, SkillUser) ->
	foreach(fun({PlayerPid, _}) ->
						case PlayerPid of
								SkillUser -> gen_server:cast(PlayerPid, {send, [16#88, 16#41, 1]}); %16#99  is Dummy
								_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#41, 0]}) %16#99  is Dummy
							end
					end, PlayerLists).
					
activate_player_select_deck(PlayerList, PlayerPid) ->
	lists:foreach(fun({Pid, _}) ->
		case Pid of
			PlayerPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#6d, 1]});
			_ -> gen_server:cast(Pid, {send, [16#88, 16#6d, 0]})
		end
	end, PlayerList).
	
response_player_select_deck(PlayerReturn, PlayerPid, Data) ->
	case PlayerReturn of
		PlayerPid -> skill_utility:player_select_deck(PlayerPid, Data);
		_ -> play_utility:out_of_turn(PlayerPid, player_select_deck)
	end.

response_player_select_skill_decision(PlayerReturn, PlayerPid, Data) ->
	case PlayerReturn of
		PlayerPid -> skill_utility:operate_player_selected_decision(Data);
		_ -> play_utility:out_of_turn (PlayerPid, player_select_decision)
	end.

% ส่งไปบอก Client ว่าผู้ใช้ สกิล ได้เสร็จสิ้นการบวนการเลือกเป้าหมายแล้ว เพื่อลบ กรอบสีส้มออก
finish_set_seal_use_skill(PlayerLists, SkillUser) ->
	foreach (	fun ({PlayerPid, _}) ->
							case SkillUser of
								PlayerPid ->	gen_server:cast(PlayerPid, {send, [16#88, 16#90]});
								_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#90]})
							end
						end, PlayerLists).

update_player_select_target_skill(PlayerLists, SkillUser, CardOwner, TargetList, Data, TargetType) ->
	foreach (	fun ({PlayerPid, _}) ->
							case PlayerPid of
								SkillUser ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#6b, 204]});
								_ ->
									case TargetType of
										all_is_card -> gen_server:cast(PlayerPid, {send, [16#88, 16#6b, Data, length(TargetList)]++list_card_belong(CardOwner, TargetList)});
										_ -> 
											case [SkillUser] of
												TargetList -> gen_server:cast(PlayerPid, {send, [16#88, 16#6b, Data, 0]});
												_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#6b, Data, 1]})
											end
									end
							end
						end, PlayerLists).
			
list_card_belong(CardOwner, [{TPid, CardOrder, CardID, CardZone, CardLine}|Tail]) ->
	case TPid of
		CardOwner -> 
			[1, CardOrder, <<CardID:16>>, CardZone, CardLine]++list_card_belong(CardOwner, Tail);
		_ -> [0, CardOrder, <<CardID:16>>, CardZone, CardLine]++list_card_belong(CardOwner, Tail)
	end;
list_card_belong(_, []) ->  [].
	
activate_select_curse(PlayerLists, CardOwner, AmountType, CurseNum, CurseSize, TargetCurse) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#6c, AmountType, CurseNum, CurseSize] ++ TargetCurse});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#6c, 204]})
				end
			end, PlayerLists).
			
response_activate_select_curse(PlayerPid, SkillUser, Data) ->
	case PlayerPid of
		SkillUser ->
			skill_utility:add_selected_curse_to_fx(Data);% Delete
			%skill_effect:update_player_selected_curse(Data);
		_ -> play_utility:out_of_turn (PlayerPid, player_select_skill_condition)
	end.

activate_select_element(PlayerLists, PlayerPid, AmountType, ElemAmount, ElemSize, ElemToSelect) ->
	foreach(fun({Pid, _}) ->
				case Pid of
					PlayerPid ->
						io:format("send select element ~n"),
						gen_server:cast(Pid, {send, [16#88, 16#62, AmountType, ElemAmount, ElemSize] ++ ElemToSelect});
					_ -> 
						gen_server:cast(Pid, {send, [16#88, 16#62, 204]})
				end
			end, PlayerLists).
	
%update_player_select_curse(PlayerLists, SkillUser, Data) ->
	%foreach (	fun ({PlayerPid, _}) ->
							%case PlayerPid of
								%SkillUser ->
									%gen_server:cast(PlayerPid, {send, [16#88, 16#xx, 204]});
								%_ ->
									%gen_server:cast(PlayerPid, {send, [16#88, 16#xx, Data]})
							%end
						%end, PlayerLists).

% activate_select_skill_condition_target(PlayerLists, SkillUser, TargetType, DialogCode, AmountType, Amount, TargetReply) ->	
	% foreach (	fun ({PlayerPid, _}) ->
							% case PlayerPid of
								% SkillUser ->
									% gen_server:cast(PlayerPid, {send, [16#88, 16#6d, TargetType, DialogCode, AmountType, Amount] ++ TargetReply});
								% _ ->
									% gen_server:cast(PlayerPid, {send, [16#88, 16#6d, 204]})
							% end
						% end, PlayerLists).
% 
% player_select_skill_conditon_target(PlayerPid, SkillUser, Data) ->
	% case PlayerPid of
		% SkillUser ->
			% TargetList = selected_list_operate(PlayerPid, Data),
			% skill_effect:add_select_seal_power_to_fx(TargetList);
		% _ -> play_utility:out_of_turn (PlayerPid, player_select_skill_conditon_target)
	% end.
	

activate_select_elem(PlayerLists, CardOwner, AmountType, ElemNum, ElemSize, TargetElem) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#88, AmountType, ElemNum, ElemSize] ++ TargetElem});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#88, 204]})
				end
			end, PlayerLists).
		
response_activate_select_elem(PlayerPid, SkillUser, Data) ->
	case PlayerPid of
		SkillUser ->
			skill_utility:add_selected_elem_to_fx(Data);
		_ -> play_utility:out_of_turn (PlayerPid, player_select_skill_condition)
	end.

reject_cards_using_skill(PlayerPid, PlayerList, Reason, LockMsg) ->
	case stack_pool:get_last_stack (self(), play) of
		{ok, PlayStep} ->
			lists:foreach(
										fun({Pid, _}) ->
											case Pid of
												PlayerPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#6f, 0, Reason]});
												OpponentPid -> gen_server:cast(OpponentPid, {send, [16#88, 16#6f, 0, 204]})
											end
										end, PlayerList),
			interfere_step:return_play(PlayStep),
			LockMsg;
		_ ->	
			lists:foreach(
										fun({Pid, _}) ->
											case Pid of
												PlayerPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#6f, 1, Reason]});
												OpponentPid -> gen_server:cast(OpponentPid, {send, [16#88, 16#6f, 1, 204]})
											end
										end, PlayerList),
			[fe]
	end.
