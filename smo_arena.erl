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
-module(smo_arena).
-behaviour(gen_server).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule interface
-export([start/1]).       % 
-export([start_link/1]).  % 
-export([is_your_turn/1]).

%% internal exports
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-import (lists, [foreach/2, split/2, append/2, reverse/2]).

-record(arena_data, {room_id, player_lists, login_name, rank, rname_size, room_name, 
password, have_pwd, play_time, number_of_player, allow_observer, sub_time, 
option_data, step_timeout, lock_msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intermodule exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Args) ->
	%%smo_logger:msg("Por: smo_arena:start"),
	%%[SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver] = Args,
	%%smo_logger:msg("McDuck Said : We're here."),
	supervisor:start_child(smo_arena_sup, [Args]). % simple_one_for_one
	
%% called by supervisor to start the socket gen_server
start_link(Args) ->
	%%smo_logger:msg("McDuck Said : In smo_arena:start_link"),
	Options = [],
	%gen_server:start_link({local, pump@PUMP}, smo_arena, Args, Options).
	AllNodes = nodes(),
	[Node] = function_utility:random_select(1, AllNodes),
	smo_logger:fmsg("<<<<<<<<<<<<<<<<<<< Selected Node is ~p>>>>>>>>>>>>>>>>>>>>>>>> ~n", [Node]),
	spawn_link(Node, gen_server, start_link, [smo_arena, Args, Options]).
	%gen_server:start_link(smo_arena, Args, Options).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.	
	
handle_call({get_current_lockmsg}, _, State) ->
	{reply, {ok, State#arena_data.lock_msg}, State};
%%handle_call({test_call, PlayerPid, LoginName, RoomID, HeaderReply, JoinFunction}, From, State) ->
%%	smo_logger:msg(">>>>> test call comming <<<<<<"),
%%	{reply, ok, State};
% handle_call({join_arena, PlayerPid, LoginName, RoomID, HeaderReply, JoinFunction}, _, State) ->
	% smo_logger:msg("<<<<<<<<<< Arena has been joined >>>>>>>>>>"),
	% AguardPid = get(smo_arena_guardian),
	% gen_server:cast(AguardPid, {set_room_infomation, RoomID, player_in_room, 2}),
	% %lib_lobby_protocol:set_room_infomation(RoomID, player_in_room, 2),
	% DispPid = get(smo_dispatcher),
	% case gen_server:call(AguardPid, {get_room_rank, self()}) of
	% %case lib_lobby_protocol:get_room_rank(self()) of
		% [2] -> "";
		% _ -> gen_server:cast(DispPid, {broadcast_new_arena, [16#81, 16#00, <<RoomID:16>>, 2, 1]})%smo_dispatcher:update_new_arena([16#81, 16#00, <<RoomID:16>>, 2, 1])
	% end,
	% smo_logger:msg("State1"),
% 
	% %smo_arena_guardian:update_arena(RoomID, PlayerPid),
	% gen_server:cast(AguardPid, {update_arena, RoomID, PlayerPid}),
	% 
	% %{ok, {RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, PWD}} = lib_lobby_protocol:get_room_join_infomation(RoomID),
	% %RoomNSize = length(RoomName),
	% %HavePWD = length(PWD),
	% %smo_player_guardian:update_arena(RoomID, [RoomNSize, RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, HavePWD]),
	% smo_logger:msg("State2"),
	% join_arena(PlayerPid, LoginName, State#arena_data.room_name, HeaderReply, JoinFunction, State#arena_data.player_lists, State#arena_data.sub_time),
	% smo_logger:msg("State3"),
	% request_player_deck(PlayerPid, LoginName),
	% smo_logger:msg("State4"),
	% gen_server:cast(PlayerPid, {set_arena, self(), RoomID}),
	% calculate_time_left(State, [{PlayerPid, LoginName} | State#arena_data.player_lists],
						% State#arena_data.option_data, 
						% State#arena_data.step_timeout, 
						% State#arena_data.lock_msg);	
%%%===============================================
%%%============== WAITING STEP ===================
%%%===============================================
% handle_call({joiner_ready}, _, State) ->
	% smo_logger:msg("Joiner_Ready"),
	% foreach( fun({PlayerPid, LoginName}) -> 
			% io:format("send [83, 00] to {~p~p}", [PlayerPid, LoginName]),
			% gen_server:cast(PlayerPid, {send, [16#83, 16#00]}) 
	% end, State#arena_data.player_lists),
	% calculate_time_left (State, State#arena_data.player_lists, 
						% State#arena_data.option_data, 
						% State#arena_data.step_timeout, [m8300]);	
						
handle_call({owner_start}, _, State) ->
	
	create_log_play_game(State#arena_data.player_lists),
	
	gen_server:cast(self(), {room_play_status}),
	PguardPid = get(smo_player_guardian),
	
	% AguardPid = get(smo_arena_guardian),
	% RoomID = gen_server:call(AguardPid, {get_room_info, self(), room_id}),
	% gen_server:cast(AguardPid, {set_room_infomation, RoomID, room_status, 2}),
	% 
	% case gen_server:call(AguardPid, {get_room_rank, self()}) of
	% %case lib_lobby_protocol:get_room_rank(self()) of
		% [2] -> "";
		% _ -> gen_server:cast(PguardPid, {update_arena, RoomID, 2})
	% end,
		
	mnesia_play:set_game_data (self(), player_list, State#arena_data.player_lists),
	foreach(	fun({PlayerPid, {LoginName, _}}) ->
		gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 2}),
		gen_server:cast(PguardPid, {update_player_status, LoginName}),
		gen_server:cast(PlayerPid, {send, [16#83, 16#01]})
	end, State#arena_data.player_lists),
	
	calculate_time_left(State, State#arena_data.player_lists, 
							State#arena_data.option_data,
							State#arena_data.step_timeout, [m8300]);	
handle_call({pao_ying_chub, Data}, From, State) ->
	{Pid,_Tag} = From,
	calculate_time_left (State, State#arena_data.player_lists, lib_arena_play:check_ying_chub(State#arena_data.option_data, Pid, Data), State#arena_data.step_timeout);			
handle_call({play_order_select, Data}, From, State) ->
	{Pid,_Tag} = From,
	lib_arena_play:reply_play_order(State#arena_data.option_data, Pid, Data, State#arena_data.player_lists),
	calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_call({response_update_initial_data}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, into_next_step),
	calculate_time_left (State, State#arena_data.player_lists, 
						OptionUpdate, 
						State#arena_data.step_timeout, 
						State#arena_data.lock_msg);
						
handle_call({request_next_step}, From, State) ->
	{Pid,_Tag} = From,
	case is_your_turn(Pid) of
		1 ->	
			play_utility:into_next_step();
		0 ->	
			play_utility:out_of_turn (Pid, request_next_step)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);

handle_call({leave_room, LeaveType}, From, State) ->
	{LeaverPid,_Tag} = From,
	put(leaverShrine,shrine_zone:get_shrine_level(LeaverPid)),
	mnesia_play:remove_player_data (LeaverPid),
	PguardPid = get(smo_player_guardian),
	AguardPid = get(smo_arena_guardian),
	RankStatus = gen_server:call(AguardPid, {get_room_rank, self()}),
	%RankStatus = lib_lobby_protocol:get_room_rank(self()),
	case gen_server:call(AguardPid, {get_room_id, self()}) of
	%case lib_lobby_protocol:get_room_id(self()) of
		% กรณีห้องถูกลบไปแล้ว อาจเกิดจากผู้ที่ออกจากห้องในคราวนี้เป็นที่ออกจากห้องทีหลัง
		[] -> "";
		% กรณีห้องยังไม่ถูกลบไป เกิดจากผู้ที่ออกจากห้องในคราวนี้เป็นผู้ออกจากห้องก่อน
		[RoomID] -> 	
			gen_server:cast(AguardPid, {delete_arena, RoomID})
			%lib_lobby_protocol:remove_room(RoomID), smo_arena_guardian:delete_arena(RoomID)
	end,
	case State#arena_data.player_lists of
		% กรณีมีผู้เล่นคนเดียวหรือเหลือผู้เล่นคนเดียว
		[{PlayerPid, {LoginName, _}}] -> smo_logger:fmsg("Player {~p, ~p} is Leave Arena ~n", [PlayerPid, LoginName]),
			erase(subturn),
			send_player_leave_room(PlayerPid, LeaverPid);%lib_lobby_protocol:set_room_infomation(RoomID, player_in_room, 0);
		% กรณีมีผู้เล่นสองคนแล้วเกิดคนใดคนหนึ่งออกไปก่อน
		_ ->
			
			case get(game_end) of
				undefined ->
					case RankStatus of
						[2] -> 
							LeaveResult = get_leave_game_result(State#arena_data.player_lists, LeaverPid) ++ ["0"],
							foreach(fun({PlayerPid, {LoginName, _}}) ->
							% กรณที่เป็นการเล่นแบบ Rank ไป card_info:remove_card_info(PlayerPid), ที่ calculate_bonus_point
													case PlayerPid of
														LeaverPid -> send_player_leave_room(PlayerPid, LeaverPid);
														_ ->
															send_player_leave_room(PlayerPid, LeaverPid),
															gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 1})
													end
												end, State#arena_data.player_lists),
							calculate_bonus_point(LeaveResult, State#arena_data.player_lists, LeaveType);
						[3] -> 
							LeaveResult = get_leave_game_result(State#arena_data.player_lists, LeaverPid) ++ ["0"],
							foreach( fun( {PlayerPid, {LoginName, _}} ) ->
													card_info:remove_card_info(PlayerPid),
													case PlayerPid of
														LeaverPid -> send_player_leave_room(PlayerPid, LeaverPid);
														_ ->
															send_player_leave_room(PlayerPid, LeaverPid),
															gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 1}),
															send_msg_end_game(PlayerPid, your_result, player_win)
													end
												end, State#arena_data.player_lists),
							calculate_tournament_statistic(LeaveResult, State#arena_data.player_lists, LeaveType);
						_NotRank ->
							DataString = get_command_string(State#arena_data.player_lists, 0, player_win, "0"),
							create_log_end_game(DataString),
							foreach( fun( {PlayerPid, {LoginName, _}} ) ->
													card_info:remove_card_info(PlayerPid),
													case PlayerPid of
														LeaverPid ->
															send_player_leave_room(PlayerPid, LeaverPid);
														_ ->
															send_player_leave_room(PlayerPid, LeaverPid),
															
															gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 1}),																
															send_msg_end_game(PlayerPid, your_result, player_win)
													end
												end, State#arena_data.player_lists)
					end;
				_ -> 
					foreach(fun({PlayerPid, {LoginName, _}}) ->
											card_info:remove_card_info(PlayerPid),
											case PlayerPid of
												LeaverPid ->
													send_player_leave_room(PlayerPid, LeaverPid);
												_ ->
													send_player_leave_room(PlayerPid, LeaverPid),
													
													gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 1})
											end
										end, State#arena_data.player_lists)
			end
	end,
	{L1} = delete(LeaverPid, State#arena_data.player_lists),
	calculate_time_left(State, L1, [], State#arena_data.step_timeout);
	
handle_call({leave_rank_room}, From, State) ->
	smo_logger:fmsg("leave rank room ~p~n", [self()]),
	PguardPid = get(smo_player_guardian),
	{LeaverPid,_Tag} = From,
	mnesia_play:remove_player_data (LeaverPid),
	foreach(fun({PlayerPid, {LoginName, _}}) ->
		case PlayerPid of
			LeaverPid -> send_player_leave_room(PlayerPid, LeaverPid);
			_ ->
				send_player_leave_room(PlayerPid, LeaverPid),
				gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 1})
				%lib_lobby_protocol:change_player_data(PlayerPid, LoginName, playing_status, 1)
		end
	end, State#arena_data.player_lists),
	%mnesia_play:set_game_data(self()),
	{L1} = delete(LeaverPid, State#arena_data.player_lists),
	calculate_time_left (State, L1, [], State#arena_data.step_timeout);
%%%===============================================
%%%============== CHECKUP STEP ===================
%%%===============================================
handle_call({req_into_checkup}, From, State) ->
	MainTimer = 20000,
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, into_checkup),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, [MainTimer , now()], State#arena_data.lock_msg);	

%%%===============================================
%%%================= DRAW STEP ===================
%%%===============================================
handle_call({req_into_draw}, From, State) ->
	MainTimer = 20000,
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, into_draw),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, [MainTimer , now()], State#arena_data.lock_msg);
handle_call({request_draw, Data}, From, State) ->
	{Pid,_Tag} = From,
	case is_your_turn(Pid) of
		1 ->	
			draw_step:request_draw(Pid, Data);
		0 ->	
			play_utility:out_of_turn (Pid, request_draw)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);
						
handle_call({response_player_select_deck, Data}, From, State) ->
	{Pid,_Tag} = From,
	draw_card:response_select_deck(Pid, Data),
	calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);

handle_call({response_player_select_skill_decision, Data}, From, State) ->
	{Pid,_Tag} = From,
	using_skill_controller:response_player_select_skill_decision(Pid, State#arena_data.option_data, Data),
	calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);
						
handle_call({response_update_player_collection}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, Pid, return_stack),
	calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);
						
handle_call({response_move_mystic_to_other_target}, From, State) ->
	{Pid,_Tag} = From,
	smo_logger:fmsg("response_move_mystic_to_other_target ~p ~n", [Pid]),
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, Pid, return_stack),
	calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);
						
handle_call({response_decide_change_zone, Data}, From, State) ->
	{Pid,_Tag} = From,
	smo_logger:fmsg("response_decide_change_zone ~p ~n", [Pid]),
	function_utility:response_decide_change_zone(Pid, Data, State#arena_data.option_data),
	calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, State#arena_data.lock_msg);
						
handle_call({rec_res_draw}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({skip_draw}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 ->	play_utility:into_next_step ();
		0 ->	play_utility:out_of_turn (PlayerPid, skip_draw)
	end,			
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
	
%%%===============================================
%%%=============== DISCARD STEP ==================
%%%===============================================
handle_call({req_into_discard}, From, State) ->
	MainTimer = 20000,
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, into_discard),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, [MainTimer , now()], State#arena_data.lock_msg);
handle_call({request_discard, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 ->	discard_step:check_player_discard (PlayerPid, Data);
		0 ->	play_utility:out_of_turn (PlayerPid, request_discard)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_call({rec_res_discard}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
	
%%%===============================================
%%%============== INTERFERE STEP =================
%%%===============================================
handle_call({req_use_card, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = interfere_step:check_play_interfere (PlayerPid, State#arena_data.option_data, card_play, Data),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({req_use_skill, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = interfere_step:check_play_interfere (PlayerPid, State#arena_data.option_data, skill_play, Data),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({skip_interfere}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate =
		case interfere_step:check_skip_interfere(Pid, State#arena_data.player_lists, State#arena_data.option_data) of
			[] -> State#arena_data.option_data;
			Option -> Option
		end, 
	% แก้ไขส่วนนี้ทำให้ผู้เล่นฝั่งตรงข้ามไม่ได้จั่วการ์ด
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
	
%%%===============================================
%%%================= MAIN STEP ===================
%%%===============================================
%%%16#00
handle_call({req_into_main}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, into_main),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
%%% 16#01
handle_call({request_cast_card, Data}, From, State) ->
	{Pid,_Tag} = From,
	casting_controller:request_cast_card(Pid, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
						 State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({req_skill_use, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:request_use_skill(PlayerPid, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
						 State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({req_change_line, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 -> case list_to_binary(Data) of
				<<WhoCard:8, CardOrder:8, CardID:16, ChangeType:8>> ->
					CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
					line_change:assign_change_line (PlayerPid, CardOwner, CardOrder, CardID, ChangeType);
				_ ->	io:format("Change line data ~p error !!! ~n", [Data])
			end;
		0 ->	play_utility:out_of_turn (PlayerPid, req_change_line)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_call({request_assign_attack, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 ->	assign_atk_controller:assign_atk_check(PlayerPid, Data);
		0 ->	play_utility:out_of_turn (PlayerPid, req_assign_atk)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
						 State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({req_assign_combination, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 -> case list_to_binary(Data) of
				<<WhoCard:8, CardOrder:8, CardID:16>> ->
					CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
					combination:declare_combination (PlayerPid, CardOwner, CardOrder, CardID);
				_ ->	io:format("Combination data error!!!~n")
			end;
		0 ->	play_utility:out_of_turn (PlayerPid, req_assign_combination)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_call({req_break_combination, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case is_your_turn(PlayerPid) of
		1 ->
			case list_to_binary(Data) of
				<<WhoCard:8, CardOrder:8, CardID:16>> ->
					CardOwner = play_utility:get_owner_pid (PlayerPid, WhoCard),
					break_combine:assign_break_combine (PlayerPid, CardOwner, CardOrder, CardID);
				_ ->	io:format("Request break combine data error !!! ~n")
			end;
		0 -> play_utility:out_of_turn (PlayerPid, select_combine_set)
	end,			
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_call({player_select_activation_ability, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	%case stack_pool:get_last_stack(self(), player_select_activation_ability) of
		%{ok, Module} -> io:format("++++++------------------module ~p~n", [Module]),
			%Module:player_select_activation_ability (PlayerPid, OptionData, Data)
	%end,
	mod_ability_activate:post_select_activation(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, 
							State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_activation}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Casting Zone (CALL)
handle_call({response_card_caster}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, Pid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({player_select_growth, Data}, From, State) ->
	{Pid,_Tag} = From,
	casting_controller:player_select_growth (Pid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_growth, Data}, From, State) ->
	{Pid,_Tag} = From,
	GrowthOption = State#arena_data.option_data,
	set_growth:select_growth_option (Pid, Data, GrowthOption),
	%OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, growth_selected),
	calculate_time_left (State, State#arena_data.player_lists,  GrowthOption, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_line, Data}, From, State) ->
	{Pid,_Tag} = From,
	casting_controller:select_line_cast(Pid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
							  State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_line}, From, State) ->
	{Pid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, Pid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({player_select_ability_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	%ability_utility:player_select_ability_target (PlayerPid, OptionData, Data),
	case stack_pool:get_last_stack(self(), reselect_target) of
		{ok, yes} -> mod_ability_activate:add_all_fx_to_reselect_target(PlayerPid, State#arena_data.option_data, Data); 
		{ok, case_then_select} -> special_effect:set_second_effect_target(PlayerPid, State#arena_data.option_data, Data);
		{ok, first_select_mystic_target} -> new_mystic_check:set_effect_to_selected_target(PlayerPid, State#arena_data.option_data, Data);
		_ ->
			mod_ability_activate:set_effect_to_selected_target(PlayerPid, State#arena_data.option_data, Data)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_ability_target}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, res_update_select_ability),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, 
						State#arena_data.step_timeout, State#arena_data.lock_msg);						
						
%%------------- Other Zone (CALL)
handle_call({play_interfere_step}, From, State) ->
	{PlayerPid,_Tag} = From,
	case State#arena_data.option_data of
		{Any, PlayerPid, Skip} ->
			OptionUpdate = {Any, PlayerPid, Skip},
			play_utility:out_of_turn (PlayerPid, player_select_ability_target);
		{Any, _, Skip} ->
			OptionUpdate = {Any, PlayerPid, Skip},
			interfere_step:play_interfere_step (State#arena_data.player_lists, PlayerPid)
	end,
	io:format("-----------------play_interfere_step-----------------~n"),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, 
						State#arena_data.step_timeout, [fe]);
handle_call({skip_sub_interfere}, From, State) ->
	{Pid,_Tag} = From,
	LockMsg = State#arena_data.lock_msg,
	StepTimeOut = State#arena_data.step_timeout,
	OptionData = State#arena_data.option_data,
	PlayerLists = State#arena_data.player_lists,
	case LockMsg of
		[m880e] ->
			case interfere_step:check_skip_sub_interfere (Pid, PlayerLists, OptionData) of
				{ok, OU} ->
					OptionUpdate = OU,
					LockMsgUpdate = [m880e];
				{error, OU} ->
					OptionUpdate = OU,
					LockMsgUpdate = [m880e]
			end;
		[fe] ->
			case interfere_step:check_cancel_sub_interfere (Pid, PlayerLists, OptionData) of
				{ok, OU} ->		
					OptionUpdate = OU,
					LockMsgUpdate = [m880e];
				{error, OU} ->
					OptionUpdate = OU,
					LockMsgUpdate = [fe]
			end;
		_ ->	
			OptionUpdate = OptionData,
			LockMsgUpdate = LockMsg
	end,
	io:format("Option update ~p and Lock msg ~p [calculate_time_left .erl]~n", [OptionUpdate, LockMsg]),
	%%% the variable used in calculate_time_left below assigned above, please check.
	calculate_time_left (State, PlayerLists, OptionUpdate, StepTimeOut, LockMsgUpdate);

%%------------- Attack Zone (CALL)
handle_call({response_attacker}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({res_player_select_attack_zone, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	new_assign_atk:zone_selected(PlayerPid, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({select_attacker_ability, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case State#arena_data.option_data of
		PlayerPid -> 
			case Data of
				[AbilitySelect] ->
					%assign_atk:select_attacker_ability (PlayerPid, AbilitySelect);
					mod_ability_activate:update_opponent_what_selected(PlayerPid, AbilitySelect);
				_ -> io:format("select_attacker_ability Data error !!! ~n")
			end;
		_Other -> io:format("Out of Step case ~p~n", [_Other]),
			play_utility:out_of_turn (PlayerPid, select_attacker_ability)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_ability}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, res_update_select_ability),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, [8812]);
handle_call({player_select_attack_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	assign_atk_controller:player_select_attack_target (PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_target}, From, State) ->
	{PlayerPid,_Tag}  = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, attack_target_selected),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({select_attacked_ability, Data}, From, State) ->
	{PlayerPid,_Tag}  = From,
	case State#arena_data.option_data of
		PlayerPid -> case Data of
				[AbilitySelect] -> 
					assign_atk:select_attacked_ability (PlayerPid, AbilitySelect);
				_ -> io:format("select_attacked_ability Data error !!! ~n")
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, select_attacked_ability)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_select_attacked_ability}, From, State) ->
	{PlayerPid,_Tag}  = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, response_select_attacked_ability),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({res_update_destroy}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Combination Zone (CALL)
handle_call({select_combine_option, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	{CombineOption, SelectPid} = State#arena_data.option_data,
	case PlayerPid of
		SelectPid -> combination:select_combination_option(PlayerPid, Data, CombineOption);
		_ -> play_utility:out_of_turn(PlayerPid, select_combine_option)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({select_combine_set, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	{CombineCheck, SelectPid} = State#arena_data.option_data,
	case PlayerPid of
		SelectPid -> combination:select_combination_set(PlayerPid, Data, CombineCheck);
		_ -> play_utility:out_of_turn (PlayerPid, select_combine_set)
	end,
	calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({rep_update_support_seal}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_completed_combine}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Move to Shrine (CALL)
handle_call({response_update_shrine_level}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, count_shrine_level),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
% handle_call({select_card_ability_affect, Data}, From, State) ->
	% {PlayerPid,_Tag} = From,
	% ability_utility:select_card_ability_affect(PlayerPid, State#arena_data.option_data, Data),
	% calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Line Change (CALL)
handle_call({rec_res_changing_line}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({rec_res_changed_line}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	case lists:flatlength(OptionUpdate) of
		0 -> LockMsgUpdate = 0;
		_ -> LockMsgUpdate = State#arena_data.lock_msg
	end,
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, LockMsgUpdate);
handle_call({change_line_complete}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	interfere_step:return_play(check_play_step),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
%%------------- Attack to hand (CALL)
handle_call({response_update_hand_attacker}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, hand_attacker_updated),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_select_hand_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionData = State#arena_data.option_data,
	case PlayerPid of
		OptionData -> case Data of
				[CardOrder] ->
					hand_atk:update_hand_card_target (PlayerPid, State#arena_data.player_lists, CardOrder);
				_ ->	io:format("Hand target  data error!!!~n")
			end;
		_ ->	play_utility:out_of_turn (PlayerPid, select_hand_target)
	end,
	calculate_time_left (State, State#arena_data.player_lists, OptionData, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_hand_target}, From, State) ->
	{PlayerPid,_Tag} = From,
	case play_utility:check_ready_step (State#arena_data.option_data, PlayerPid) of
		{ready} ->
			hand_atk:hand_card_attacked (),
			UpdateOption = 0;
		{not_ready, Return} ->
			UpdateOption = Return
	end,
	calculate_time_left (State, State#arena_data.player_lists, UpdateOption, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Player Select Attack Action (CALL)
handle_call({player_select_attack_action, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	assign_atk_controller:player_select_attack_action(State#arena_data.option_data, PlayerPid, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_additional_attack, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionData = State#arena_data.option_data,
	case OptionData of
		PlayerPid ->
			new_assign_atk:player_select_additional_attack(Data);
		_ ->	play_utility:out_of_turn (PlayerPid, player_select_additional_attack)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Break combine (CALL)
handle_call({res_update_break_combine}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_call({response_update_card_to_arena}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Casting Mystic (CALL)
handle_call({select_mystic_ability, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	mystic_card_controller:select_mystic_ability(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_casting_mystic}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_mystic_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	new_mystic_check:set_effect_to_selected_target(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_mystic_target}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, checK_other_mystic_effect),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_cast_mystic_success}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_mystic_effect, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	mystic_card_controller:player_select_mystic_effect (PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_mystic_fx}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);


%%------------- Seal Skill (CALL)
handle_call({response_update_card_use_skill}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, response_update_card_use_skill),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_skill, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:player_select_skill(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_select_skill}, From, State) ->
%			io:format('calculate_time_left :response_update_select_skill PlayerPid = ~p~n', [PlayerPid]),
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, response_update_select_skill),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_player_mp}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

handle_call({server_select_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:server_select_target(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
	
handle_call({player_select_target_skill, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:player_select_target_skill(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_player_select_target_skill}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_finish_set_seal_use_skill}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_activate_select_curse, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:response_activate_select_curse(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_activate_select_elem, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:response_activate_select_elem(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
	
handle_call({response_activate_skill_animation}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
	
handle_call({response_select_deck, Data}, From, State) ->
	{PlayerPid, _} = From,
	using_skill_controller:response_player_select_deck(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [0]);
	
handle_call({response_player_select_skill_condtion_target, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	using_skill_controller:response_player_select_skill_conditon_target(PlayerPid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Move to library zone (CALl)
handle_call({response_update_card_to_library}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({response_update_deck_size}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Growth Zone (CALL)
handle_call({player_select_growth_option, Data}, From, State) ->
	{Pid,_Tag} = From,
	casting_controller:player_select_growth_option (Pid, State#arena_data.option_data, Data),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({player_select_growth_set, Data}, From, State) ->
	{Pid,_Tag} = From,
	case is_your_turn(Pid) of
		1 -> set_growth:player_select_growth_set (Pid, Data, State#arena_data.option_data);
		0 -> play_utility:out_of_turn (Pid, player_select_growth_set)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%handle_call({player_select_growth_set, Data}, From, State) ->
%	{Pid,_Tag} = From,
%	casting_controller:player_select_growth_set (Pid, State#arena_data.option_data, Data),
%	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handle_call({response_update_growth_material}, From, State) ->
	% {Pid,_Tag} = From,
	% OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, Pid, return_stack),
	% calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({reject_casting, Reason, no_change_lock_msg}, From, State) ->
	{Pid,_Tag} = From,
	handle_cast(Pid, {send, [16#88, 16#ff, Reason]}),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
							  State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Remove Zone (CALL)
handle_call({response_update_remove_cards}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%------------- Reveal Cards (CALL)
handle_call({response_reveal_cards}, From, State) ->
	{PlayerPid,_Tag} = From,
	OptionUpdate = play_utility:check_ready_step(State#arena_data.option_data, PlayerPid, return_stack),
	calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);

%%%===============================================
%%%================= CLEAN STEP ==================
%%%===============================================							
handle_call({req_into_mp_clean}, From, State) ->
	{Pid,_Tag} = From,
	case play_utility:check_ready_step (State#arena_data.option_data, Pid) of
		{ready} ->
			%%%smo_logger:msg("check_ready_stp return {ready}"),
			mp_clean_step:into_step(),
			UpdateOption = [];
		{not_ready, Return} ->
			%%%smo_logger:msg("check_ready_stp return {not_ready}"),
			UpdateOption = Return
	end,
	calculate_time_left (State, State#arena_data.player_lists, UpdateOption, State#arena_data.step_timeout);
handle_call({rec_update_mp_clean}, From, State) ->
	{Pid,_Tag} = From,
	case play_utility:check_ready_step (State#arena_data.option_data, Pid) of
		{ready} ->
			play_utility:into_next_step(),
			UpdateOption = [];
		{not_ready, Return} ->
			UpdateOption = Return
	end,
	calculate_time_left (State, State#arena_data.player_lists, UpdateOption, State#arena_data.step_timeout);
	
%%%===============================================
%%%================= END STEP ====================
%%%===============================================
handle_call({req_into_eos}, From, State) ->
	{Pid,_Tag} = From,
	case play_utility:check_ready_step (State#arena_data.option_data, Pid) of
		{ready} ->	 end_of_subturn:end_of_subturn_step (),
				UpdateOption = [];
		{not_ready, Return} -> UpdateOption = Return
	end,
	calculate_time_left (State, State#arena_data.player_lists, UpdateOption, State#arena_data.step_timeout);

%%%===============================================
%%%================= MISC  ====================
%%%===============================================
handle_call({req_shrine_info, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case Data of
		[1] -> shrine_zone:req_shrine_info(PlayerPid, PlayerPid);
		[0] -> OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
			  shrine_zone:req_shrine_info(PlayerPid, OpponentPid)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({req_hand_info}, From, State) ->
	{PlayerPid,_Tag} = From,
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	hand_zone:req_hand_info(PlayerPid, OpponentPid),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({req_remove_info, Data}, From, State) ->
	{PlayerPid,_Tag} = From,
	case Data of
		[1] -> remove_zone:req_remove_info(PlayerPid, PlayerPid);
		[0] -> OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
			  remove_zone:req_remove_info(PlayerPid, OpponentPid)
	end,
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_call({in_game_chat, {SPlayer, Data}}, _, State) ->
	{AvatarSet, AvatarID} = get_chat_owner_avatar(State#arena_data.player_lists, SPlayer),
	%FlatString = lists:flatten([AvatarSet, AvatarID]),
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#0b, 16#03, AvatarSet, AvatarID] ++ Data})
				%gen_server:cast(PlayerPid, {send, [16#0b, 16#03, 0, 0] ++ Data})
			end, State#arena_data.player_lists),
	calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
		
handle_call({get_game_data, FieldData}, _, State) ->
	Result = mnesia_play:get_game_data (self(), FieldData),
	{reply, Result, State};

handle_call({room_can_join}, _, State) ->
	Result = 
	case get(joined) of
		undefined -> ok;
		_ -> error
	end,
	{reply, Result, State};
			
handle_call(_, _, State) ->
	{noreply, State}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle of casting session
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({join_arena, PlayerPid, LoginName, Avatar, RoomID, HeaderReply, JoinFunction, TID, Mode}, State) ->
	
	if 
		TID =/= undefined ->
			case lib_tournament:compatitor_name_status(LoginName, LoginName, TID)  of
				[{MID, _, _, _, _, _}] -> % return เป็น ชุดข้อมูล ได้แก่ .... UserName ของคู่แข่ง, วันเวลา, ระยะเวลาของ match
					put(match_ID, MID);
					_ -> gen_server:cast(LoginName, {send, [16#81, 16#a1, 0]})
			end,
			Match_ID = get(match_ID);
		true -> 
			Match_ID = 0
	end,

	case get(joined) of
		undefined ->
			put(joined, joined),
			io:format("<<<<<<<<<< Arena has been joined >>>>>>>>>>~n"),
			
			AguardPid = get(smo_arena_guardian),

			gen_server:cast(AguardPid, {set_room_infomation, RoomID, player_in_room, 2}),
			DispPid = get(smo_dispatcher),
			case gen_server:call(AguardPid, {get_room_rank, self()}) of
				[2] -> "";
				[3] -> 
					[{_PlayerPid, {_LoginName2, _}}] = State#arena_data.player_lists,
					%%SQLCommand = "CALL smno.sp_league_insert_logGame (" ++ integer_to_list(TID) ++ ",'" ++ LoginName ++"','" ++ LoginName2 ++ "');",
					SQLCommand = "CALL smno.sp_league_insert_logGame (" ++ integer_to_list(Match_ID) ++ ");",
					
					smo_logger:fmsg("~p~n", [SQLCommand]),
					lib_database:get_query(SQLCommand);				
				
				_ -> gen_server:cast(DispPid, {broadcast_new_arena, [16#81, 16#00, <<RoomID:16>>, 2, 1]})%smo_dispatcher:update_new_arena([16#81, 16#00, <<RoomID:16>>, 2, 1])
			end,
			gen_server:cast(AguardPid, {update_arena, RoomID, PlayerPid}),
			
			%{ok, {RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, PWD}} = lib_lobby_protocol:get_room_join_infomation(RoomID),
			%RoomNSize = length(RoomName),
			%HavePWD = length(PWD),
			%smo_player_guardian:update_arena(RoomID, [RoomNSize, RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, HavePWD]),
			%smo_logger:msg("State2"),
			
			gen_server:cast(PlayerPid, {set_arena, self(), RoomID}),
			
			lists:foreach(	fun ( {OwnerPid, {OwnerName, _}} ) -> 
				mnesia_play:reply_join_room(OwnerPid, OwnerName, PlayerPid, LoginName, State#arena_data.room_name, HeaderReply, JoinFunction, State#arena_data.sub_time)
			end, State#arena_data.player_lists),
			mnesia_play:create_new_player_data(PlayerPid),
			case Mode of
				0 -> request_player_deck(PlayerPid, LoginName);
				_ -> set_precon_deck(PlayerPid, Mode)
			end,
			cast_calculate_time_left(State, [{PlayerPid, {LoginName, Avatar}} | State#arena_data.player_lists],
								State#arena_data.option_data, 
								State#arena_data.step_timeout, 
								State#arena_data.lock_msg);
		_ -> 
			gen_server:cast(PlayerPid, {send, [16#01, 16#02, 16#02]}),
			cast_calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg)
	end;
	
%-----------------------------ร้องขอออกจากห้อง -------------------------------
%---------------------------------------------------------------------------------------
handle_cast({player_request_surrender, _SPlayer}, State) ->
	foreach( fun({PlayerPid, _}) -> 
			case PlayerPid of
				_SPlayer -> "";
				_ -> gen_server:cast(PlayerPid, {send, [16#83, 16#10]})
			end
	end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, 
						State#arena_data.option_data, 
						State#arena_data.step_timeout,
						State#arena_data.lock_msg);
						
handle_cast({player_response_surrender, _SPlayer, Data}, State) ->	
	foreach( fun({PlayerPid, _}) -> 
			case PlayerPid of
				_SPlayer -> "";
				_ -> gen_server:cast(PlayerPid, {send, [16#83, 16#fa]++Data})
			end
	end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, 
						State#arena_data.option_data, 
						State#arena_data.step_timeout,
						State#arena_data.lock_msg);
%-----------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------
handle_cast({request_card_deck_to_hand, PlayerPid, Data}, State) ->
	special_server_function:require_to_hand(PlayerPid, Data),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout);


handle_cast({joiner_ready}, State) ->
	foreach( fun({PlayerPid, _}) -> 
			gen_server:cast(PlayerPid, {send, [16#83, 16#00]}) 
	end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, 
						State#arena_data.option_data, 
						State#arena_data.step_timeout, [m8300]);	

handle_cast({update_into_checkup}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#84, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [16#00]);
handle_cast({update_into_draw}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
%						io:format ("Update into draw ~p~n", [PlayerPid]),
				gen_server:cast(PlayerPid, {send, [16#85, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8500]);
handle_cast({update_into_discard}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#86, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [16#00]);
handle_cast({update_into_interfere}, State) ->
	foreach(	fun ({PlayerPid, _}) ->
				interfere_step:interfere_player_def(PlayerPid, is_your_turn(PlayerPid))
			end, State#arena_data.player_lists),
	case stack_pool:get_last_stack(self(), player_pid) of
		{ok, LastPlayPid} -> OptionUpdate = {LastPlayPid, 0};
		{error, no_stack} -> OptionUpdate = {0, 0}
	end,
	cast_calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout);
handle_cast({update_into_main}, State) ->
	MainTimer = 20000,
	stack_pool:remove_stack (self()),
	foreach (	fun ({PlayerPid, _}) -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], [MainTimer , now()]);
handle_cast({update_into_mp_clean}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#89, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({update_into_eos}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#8a, 16#00, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({acknowledge_play}, State) ->
	foreach(fun ({PlayerPid, _}) -> gen_server:cast(PlayerPid, {send, [16#88, 16#02, is_your_turn(PlayerPid)]}) end, State#arena_data.player_lists),
	io:format("-----------------acknowledge_play-----------------~n"),
	cast_calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [fe]);
handle_cast({response_draw, ResponseData, PlayerDrawData}, State) ->
	foreach (	fun({PlayerPid, _}) ->
				case is_your_turn (PlayerPid) of
					0 -> ReplyData = ResponseData;
					1 -> ReplyData = ResponseData ++ PlayerDrawData
				end,
%						io:format("Reply data is ~p~n", [ReplyData]),
				gen_server:cast(PlayerPid, {send, ReplyData})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8503]);
handle_cast({reject_casting, PlayerPid, Reason}, State) ->
	LockMsgUpdate = casting_controller:reject_casting_card (PlayerPid, Reason, State#arena_data.lock_msg),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
							  State#arena_data.step_timeout, LockMsgUpdate);
handle_cast({rejected_move_to_arena, PlayerPid, Reason}, State) ->
	LockMsgUpdate = casting_controller:reject_casting_card(PlayerPid, Reason, State#arena_data.lock_msg),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
							  State#arena_data.step_timeout, LockMsgUpdate);
handle_cast({update_card_casting, CardOwner, CardOrder, CardID, MpRest}, State) ->
	casting_controller:update_card_casting(State#arena_data.player_lists, CardOwner, CardOrder, CardID, MpRest),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8805]);
handle_cast({update_clean_mp, PlayerTurn, MaxMp}, State) ->
	foreach (	fun({PlayerPid, _}) ->
				case PlayerPid of
					PlayerTurn -> 
						gen_server:cast(PlayerPid, {send, [16#89, 16#01, 1, MaxMp]});
					_ ->	
						gen_server:cast(PlayerPid, {send, [16#89, 16#01, 0, MaxMp]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);

handle_cast({room_play_status}, State) ->

	AguardPid = get(smo_arena_guardian),
	[RoomID] = gen_server:call(AguardPid, {get_room_id, self()}),
	gen_server:cast(AguardPid, {set_room_infomation, RoomID, room_status, 2}),
	case gen_server:call(AguardPid, {get_room_rank, self()}) of
		[2] -> "";
		_ -> 
			DispPid= get(smo_dispatcher),
			gen_server:cast(DispPid, {broadcast_new_arena, [16#81, 16#00, <<RoomID:16>>, 2, 2]})
	end,
	
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
	
handle_cast({redraw, DrawNumber, HaveToDraw}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
					gen_server:cast(PlayerPid, {send, [16#85, 16#04, is_your_turn(PlayerPid), HaveToDraw]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State#arena_data.player_lists, {[], DrawNumber}, State#arena_data.step_timeout, [m8502]);
handle_cast({act_player_discard}, State) ->
	foreach(fun ({PlayerPid, _}) ->
				gen_server:cast(PlayerPid, {send, [16#86, 16#02, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, 
								State#arena_data.step_timeout, [16#02]);

% ------------------------- Growth Zone (CAST) ----------------------------------
handle_cast({select_card_growth, PlayerPid}, State) ->
	casting_controller:select_card_growth (State#arena_data.player_lists, PlayerPid),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8806]);
handle_cast({update_select_growth, PlayerPid, Data, GrowthOption}, State) ->
	%casting_controller:update_select_growth(State#arena_data.player_lists, PlayerPid, GrowthSelected),
	casting_controller:update_select_growth(State#arena_data.player_lists, PlayerPid, Data),
	cast_calculate_time_left (State, State#arena_data.player_lists, GrowthOption, State#arena_data.step_timeout, [m8838]);
%handle_cast({activate_select_material_growth, PlayerPid, OptionCanSelected}, State) ->
%	casting_controller:activate_select_material_growth (State#arena_data.player_lists, PlayerPid, OptionCanSelected),
%	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8838]);
handle_cast({activate_select_growth_set, PlayerPid, GrowthMaterialSet}, State) ->
	casting_controller:activate_select_growth_set (State#arena_data.player_lists, PlayerPid, GrowthMaterialSet),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8839]);
handle_cast({send_update_growth, PlayerPid,  CardOrder, CardID}, State) ->
	casting_controller:send_update_growth(State#arena_data.player_lists, PlayerPid, CardOrder, CardID),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({act_sub_interfere, LastPlayStep}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				interfere_step:sub_interfere_player_def (PlayerPid, is_your_turn(PlayerPid))
			end, State#arena_data.player_lists),
	case stack_pool:get_last_stack (self(), player_pid) of
		{ok, LastPlayPid} ->
			OptionUpdate = {LastPlayStep, LastPlayPid, 0};
		{error, no_stack} ->
			OptionUpdate = {LastPlayStep, 0, 0}
	end,
	cast_calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, [m880e]);
handle_cast({act_select_line, CardOwner, Module}, State) ->
	casting_controller:activate_select_line (State#arena_data.player_lists, CardOwner),
	cast_calculate_time_left(State, State#arena_data.player_lists, {CardOwner, Module}, State#arena_data.step_timeout, [m8808]);
handle_cast({update_select_line, CardOwner, Line}, State) ->
	casting_controller:update_select_line(State#arena_data.player_lists, CardOwner, Line),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({act_select_mystic_ability, {SelectCase, PlayerPid}, Header, Message}, State) ->
	mystic_card_controller:act_select_mystic_ability (State#arena_data.player_lists, PlayerPid, Header, Message),
	cast_calculate_time_left (State, State#arena_data.player_lists, {SelectCase, PlayerPid}, State#arena_data.step_timeout, [m8850]);
handle_cast({update_casting_mystic, OwnerPid, Header, UpdateData}, State) ->
	mystic_card_controller:update_casting_mystic(State#arena_data.player_lists, OwnerPid, Header, UpdateData),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8851]);
handle_cast({move_msytic_to_other_target, PlayerPid, {CardOwner, CardOrder, CardID}, {PreOwner, PreOrder, PreID}, {TOwner, TOrder, TID}}, State) ->
	mystic_card_controller:move_mystic_to_other_target(State#arena_data.player_lists, PlayerPid, {CardOwner, CardOrder, CardID}, {PreOwner, PreOrder, PreID}, {TOwner, TOrder, TID}),
	cast_calculate_time_left(State, State#arena_data.player_lists, [PlayerPid], State#arena_data.step_timeout, [m88b0]);
%%%------------------------------------------------------------------------------------------
handle_cast({update_game_end, EndPid, GameResult}, State) ->

	put(game_end, ok),
	AguardPid = get(smo_arena_guardian),
	PlayerLists = State#arena_data.player_lists,
	RankStatus = gen_server:call(AguardPid, {get_room_rank, self()}),
	case RankStatus of
		[2] -> 
			Result = get_game_result(PlayerLists, EndPid, GameResult, "0"),
			% กรณีการเล่นแบบ Rank ไป card_info:remove_card_info(PlayerPid), ที่ calculate_bonus_point
			calculate_bonus_point(arrange_player(Result), PlayerLists, game_end);
		[3] -> 
			Result = get_game_result(PlayerLists, EndPid, GameResult, "0"),
			calculate_tournament_statistic(arrange_player(Result), PlayerLists, game_end),
			lists:foreach(
								fun({PlayerPid, _}) ->
									card_info:remove_card_info(PlayerPid),
									case PlayerPid of
										EndPid -> send_msg_end_game(PlayerPid, your_result, GameResult);
										_ -> send_msg_end_game(PlayerPid, other_result, GameResult)
									end
								end, PlayerLists);
		_NotRank ->
			DataString = get_command_string (State#arena_data.player_lists, 0, GameResult, "0"),
			create_log_end_game(DataString),
			foreach(	fun({PlayerPid, _}) ->
											card_info:remove_card_info(PlayerPid),
											case PlayerPid of
												EndPid -> 
													send_msg_end_game(PlayerPid, your_result, GameResult);
												_ ->
													send_msg_end_game(PlayerPid, other_result, GameResult)
											end
								end, PlayerLists)
	end,
	cast_calculate_time_left(State, State#arena_data.player_lists, 
							State#arena_data.option_data, State#arena_data.step_timeout, 
							State#arena_data.lock_msg);
							
handle_cast({player_select_deck_to_draw, PlayerDrawPid, DeckAssign}, State) ->
	foreach(	fun ({PlayerPid, _}) ->
							case PlayerPid of
								PlayerDrawPid ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#a0, 1]++DeckAssign});
								_ ->
									gen_server:cast(PlayerPid, {send, [16#88, 16#a0, 0]})
							end
						end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m88a0]);
	
handle_cast({update_player_collection, PlayerDrawPid, ResponseData}, State) ->
	foreach(fun({PlayerPid, _}) ->
						case PlayerPid of
							PlayerDrawPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#a1, 1] ++ ResponseData});
							_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#a1, 0] ++ ResponseData})
						end
					end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m88a1]);
	
handle_cast({act_player_draw, HaveToDraw}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
			gen_server:cast(PlayerPid, {send, [16#85, 16#02, is_your_turn(PlayerPid), HaveToDraw]})
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8502]);
	
handle_cast({force_skip_draw, PlayerPid}, State) ->
	gen_server:cast(PlayerPid, {send, [16#85, 16#05]}),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
	
handle_cast({act_next_command}, State) ->
	smo_logger:msg("Send next command to client"),
	foreach (	fun ({PlayerPid, _}) -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#fe, is_your_turn(PlayerPid)]})
			end, State#arena_data.player_lists),
			io:format("-----------------act_next_command-----------------~n"),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [fe]);
handle_cast({response_discard, OwnerCard, HandSize, CardSize, CardReply}, State) ->
	foreach (	fun({PlayerPid, _}) ->
				case PlayerPid of
					OwnerCard -> 
						gen_server:cast(PlayerPid, {send, [16#86, 16#03, 1, HandSize, CardSize] ++ CardReply});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#86, 16#03, 0, HandSize, CardSize] ++ CardReply})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8603]);
handle_cast({play_out_of_turn, PlayerPid}, State) ->
	gen_server:cast(PlayerPid, {send, [16#83, 16#fc]}),
	cast_calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_cast({cancel_attack, Reason}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case is_your_turn(PlayerPid) of
					1 -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#1f, 1, Reason]});
					0 -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#1f, 0]})
				end
			end, State#arena_data.player_lists),
			io:format("-----------------cancel_attack-----------------~n"),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [fe]);
handle_cast({activate_select_attack_action, PlayerPid, CardOwner, CardOrder, CardID}, State) ->
	assign_atk_controller:activate_select_attack_action(State#arena_data.player_lists, PlayerPid, CardOwner, CardOrder, CardID),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8817]);
handle_cast({activate_attacker, PlayerPid, PlayerMp, CardOwner, CardData}, State) ->
	assign_atk_controller:activate_attacker (State#arena_data.player_lists, PlayerPid, PlayerMp, CardOwner, CardData),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8810]);
handle_cast({player_select_zone_attack, PlayerPid}, State) ->
	assign_atk_controller:player_select_zone_attack(State#arena_data.player_lists, PlayerPid),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8811]);
handle_cast({activate_select_atk_target, PlayerPid, TargetMsg}, State) ->
	assign_atk_controller:activate_select_atk_target (State#arena_data.player_lists, PlayerPid, TargetMsg),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8813]);			
handle_cast({update_attack_target, CardOwner, CardOrder, CardID}, State) ->
	assign_atk_controller:update_attack_target (State#arena_data.player_lists, CardOwner, CardOrder, CardID),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8814]);
handle_cast({update_change_zone, Zone, PlayerPid, Header, ReplyMsg}, State) ->
	gen_server:cast(PlayerPid, {send, Header ++ ReplyMsg}),
	case Zone of
		same_zone ->
			cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m881e]);
		shrine_cards ->
			cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m881e]);
		remove_zone ->
			cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m883c])
	end;
handle_cast({update_hand_attacker, MpRest, CardOwner, CardOrder, CardID}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
		case PlayerPid of
			CardOwner -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#20, 1, MpRest, 1, CardOrder, <<CardID:16>>]});
			_ -> 
				gen_server:cast(PlayerPid, {send, [16#88, 16#20, 0, MpRest, 0, CardOrder, <<CardID:16>>]})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8820]);
handle_cast({activate_select_additional_attack, PlayPid}, State) ->
	foreach ( fun ({PlayerPid, _}) ->
		case PlayerPid of
			PlayPid ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#18, 1]});
			_ ->	
				gen_server:cast(PlayerPid, {send, [16#88, 16#18, 0]})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayPid, State#arena_data.step_timeout, [m8818]);
handle_cast({reject_change_line, CardOwner, CardOrder, CardID, Reason}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#2f, Reason, CardOrder, <<CardID:16>>]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#2f]})
				end
			end, State#arena_data.player_lists),
	io:format("-----------------reject_change_line-----------------~n"),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [fe]);
handle_cast({update_seal_changing_line, CardOwner, CardOrder, CardID, ChangeType}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#2a, 1, CardOrder, <<CardID:16>>, ChangeType]});
					_ ->	
						gen_server:cast(PlayerPid, {send, [16#88, 16#2a, 0, CardOrder, <<CardID:16>>, ChangeType]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({update_changed_line, CardOwner, CardOrder, CardID, LineUpdate}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner ->
						gen_server:cast(PlayerPid, {send, [16#88, 16#2b, 1, CardOrder, <<CardID:16>>, LineUpdate]});
					_ ->	
						gen_server:cast(PlayerPid, {send, [16#88, 16#2b, 0, CardOrder, <<CardID:16>>, LineUpdate]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({move_card_to_line, CardOwner, CardOrder, CardID, LineMove}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					CardOwner -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#2e, 1, 1, CardOrder, <<CardID:16>>, LineMove]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#2e, 1, 0, CardOrder, <<CardID:16>>, LineMove]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_cast({res_remove_info, ReplyPid, ReplyData}, State) ->
	gen_server:cast(ReplyPid, {send, ReplyData}),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_cast({response_reveal_cards, PlayerPid}, State) ->
	OptionUpdate = play_utility:check_ready_step (State#arena_data.option_data, PlayerPid, return_stack),
	cast_calculate_time_left (State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_cast({update_cards_reveal, PlayerPid, RevealData}, State) ->
	case RevealData of
		0 ->	
			gen_server:cast(self(), {response_reveal_cards, PlayerPid}),
			OptionUpdate = State#arena_data.option_data,
			gen_server:cast(PlayerPid, {send, [16#88, 16#7b, 0]});
		_ ->	
			OptionUpdate = [mnesia_play:get_opponent_pid(PlayerPid)],
			gen_server:cast(PlayerPid, {send, [16#88, 16#7b] ++ RevealData})
	end,
	cast_calculate_time_left(State, State#arena_data.player_lists, OptionUpdate, State#arena_data.step_timeout, [m887b]);
handle_cast({reject_use_skill, PlayerPid, Reason}, State) ->
	LockMsgUpdate = using_skill_controller:reject_cards_using_skill(PlayerPid, State#arena_data.player_lists, Reason, State#arena_data.lock_msg),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, LockMsgUpdate);
handle_cast({update_card_use_skill, PlayerPid, CardOwner, CardOrder, CardID, PlayerMp, SkillNo}, State) ->
	using_skill_controller:update_card_use_skill(State#arena_data.player_lists, PlayerPid, CardOwner, CardOrder, CardID, PlayerMp, SkillNo),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8868]);
handle_cast({activate_select_skill, PlayerPid, CardOwner, SkillList}, State) ->
	using_skill_controller:activate_select_skill(State#arena_data.player_lists, PlayerPid, CardOwner, SkillList),
	cast_calculate_time_left (State, State#arena_data.player_lists, {PlayerPid, CardOwner}, State#arena_data.step_timeout, [m8867]);
handle_cast({activate_select_skill_target, PlayerPid, TargetType, DialogCode, AmountType, Amount, TargetReply}, State) ->
	using_skill_controller:activate_select_skill_target(State#arena_data.player_lists, PlayerPid, TargetType, DialogCode, AmountType, Amount, TargetReply),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m886a]);
	
handle_cast({activate_seal_skill_animation, CardOwner, CardOrder, CardID}, State) ->
	using_skill_controller:activate_seal_skill_animation(State#arena_data.player_lists, CardOwner, CardOrder, CardID),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m886e]);
	
handle_cast({server_select_skill_target, PlayerPid, TargetType, DialogCode, AmountType, Amount, TargetReply}, State) ->
	special_server_function:server_select_target(State#arena_data.player_lists, PlayerPid, TargetType, DialogCode, AmountType, Amount, TargetReply),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [mxxxxx]);
	
	
% handle_cast({update_player_select_skill, PlayerPid, SkillNumber}, State) ->
	% using_skill_controller:update_player_select_skill(State#arena_data.player_lists, PlayerPid, SkillNumber),
	% cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8868]);
handle_cast({update_player_mp, PlayerPid, MpRest}, State) ->
	using_skill_controller:update_player_mp (State#arena_data.player_lists, PlayerPid, MpRest),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8869]);
handle_cast({finish_set_seal_use_skill, PlayerPid}, State) ->
	using_skill_controller:finish_set_seal_use_skill(State#arena_data.player_lists, PlayerPid),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8890]);
handle_cast({update_player_select_target_skill, PlayerPid, CardOwner, TargetList, TextCode, TargetType}, State) ->
	using_skill_controller:update_player_select_target_skill(State#arena_data.player_lists, PlayerPid, CardOwner, TargetList, TextCode, TargetType),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m886b]);
handle_cast({activate_player_select_skill_decision, PlayerPid}, State) ->
	using_skill_controller:activate_player_select_skill_decision(State#arena_data.player_lists, PlayerPid),
	cast_calculate_time_left(State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8841]);
handle_cast({activate_select_deck, PlayerPid}, State) ->
	using_skill_controller:activate_player_select_deck(State#arena_data.player_lists, PlayerPid),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m886d]);
	
handle_cast({player_decide_change_zone, PlayerPid, ToZone, ToZoneList}, State) ->
	function_utility:activate_decide_change_zone(State#arena_data.player_lists, PlayerPid, ToZone),
	cast_calculate_time_left(State, State#arena_data.player_lists, {PlayerPid, ToZone, ToZoneList}, State#arena_data.step_timeout, [m8840]);
	
	
handle_cast({res_shrine_info, ReplyPid, ReplyData}, State) ->
	gen_server:cast(ReplyPid, {send, ReplyData}),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);
handle_cast({update_shrine_level}, State) ->
	foreach(	fun ({PlayerPid, _}) ->
				OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
				OwnerShLv = shrine_zone:get_shrine_level(PlayerPid),
				OpponentShLv = shrine_zone:get_shrine_level(OpponentPid),
				gen_server:cast(PlayerPid, {send, [16#88, 16#30, OwnerShLv, OpponentShLv]})
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8830]);
handle_cast({update_shrine_level, PlayerPid, Header, ShrineMessage}, State) ->
	gen_server:cast(PlayerPid, {send, append(Header, ShrineMessage)}),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);		
handle_cast({activate_player_select_activation_ability, Controller, CardOwner, CardID, ActivationID}, State) ->

	lists:foreach(fun({PlayerPid, _}) -> 
		case Controller of
			PlayerPid ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#03, <<CardID:16>>, ActivationID]});
			_ ->	
				gen_server:cast(PlayerPid, {send, [16#88, 16#03, 16#cc]})
		end
	end, State#arena_data.player_lists),
	
	%casting_controller:activate_player_select_activation_ability(State#arena_data.player_lists, Controller, CardOwner, CardID, ActivationID),
	cast_calculate_time_left(State, State#arena_data.player_lists, {Controller, CardOwner}, State#arena_data.step_timeout, [m8803]);	
handle_cast({select_ability_target, PlayerPid, CardOwner, CardOrder, CardID, TargetAmountType, TargetAmount, DisplayCode, TargetAbility}, State) ->
	ability_utility:select_ability_target(State#arena_data.player_lists, PlayerPid, CardOwner, CardOrder, CardID, TargetAmountType, TargetAmount, DisplayCode, TargetAbility),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m880c]);
handle_cast({player_select_ability, Selecter, _PlayPid, AbilityID}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					Selecter -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#11, 1, AbilityID]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#11, 0, AbilityID]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, Selecter, State#arena_data.step_timeout, [8811]);
handle_cast({update_select_ability, PlayPid, AbilityID, AbilitySelect}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayPid of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#12, 1, AbilityID, AbilitySelect]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#12, 0, AbilityID, AbilitySelect]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8812]);
handle_cast({rejected_combination, PlayPid, Reason}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayPid of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#5f, Reason]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#5f, 204]})
				end
			end, State#arena_data.player_lists),
	%gen_server:cast(PlayerPid, {send, [16#88, 16#5f, Reason]}),
	io:format("-----------------reject_combination-----------------~n"),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [fe]);
handle_cast({act_combine_option, SelectPid, Header, Data, CombineOption}, State) ->
	foreach(	fun ({PlayerPid, _}) ->
				case PlayerPid of
					SelectPid -> gen_server:cast(PlayerPid, {send, Header ++ [1] ++Data});
					_ -> gen_server:cast(PlayerPid, {send, Header ++ [0]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, {CombineOption, SelectPid}, State#arena_data.step_timeout, [m885a]);
handle_cast({act_combine_set_option, SelectPid, Header, CombineSetMsg, CombineCheck}, State) ->
	foreach(fun ({PlayerPid, _}) ->
%				io:format ("Send activation combine set to ~p~n", [PlayerPid]),
		case PlayerPid of
			SelectPid -> gen_server:cast(PlayerPid, {send, Header ++ CombineSetMsg});
			_ -> gen_server:cast(PlayerPid, {send, Header ++ [16#cc]})
		end
	end, State#arena_data.player_lists),
%			io:format ("All send data ~n"),
	cast_calculate_time_left(State, State#arena_data.player_lists, {CombineCheck, SelectPid}, State#arena_data.step_timeout, [m885b]);

handle_cast({rejected_growth, PlayPid}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayPid of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#40, 1]});
					_ -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#40, 0]})
				end
			end, State#arena_data.player_lists),
cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [m8840]);

handle_cast({act_growth_set_option, Header, GrowthSetMsg, GrowthCheck}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
		case is_your_turn(PlayerPid) of
			1 -> gen_server:cast(PlayerPid, {send, Header ++ GrowthSetMsg});
			0 -> gen_server:cast(PlayerPid, {send, Header ++ [0]})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, GrowthCheck, State#arena_data.step_timeout, [m8839]);

handle_cast({update_support_seal, Header, PlayPid, SupportMsg}, State) ->

	foreach (	fun ({PlayerPid, _}) -> 
		case PlayerPid of
			PlayPid ->	gen_server:cast(PlayerPid, {send, append([Header, 1], SupportMsg)});
			_ ->	gen_server:cast(PlayerPid, {send, append([Header, 0], SupportMsg)})
		end
	%%%%%%%
	%% gen_server:cast(PlayerPid, {send, append(Header, SupportMsg)})
	%%%%%%%
		end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m885c]);
handle_cast({update_card_combination, Header, PlayPid, Message}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
		case PlayerPid of
			PlayPid ->	gen_server:cast(PlayerPid, {send, Header ++ [1] ++ Message});
			_ ->	gen_server:cast(PlayerPid, {send, Header ++ [0] ++ Message})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m885d]);
handle_cast({reject_break_combine, PlayerPid, Reason}, State) ->
	gen_server:cast(PlayerPid, {send, [16#88, 16#65, Reason]}),
	io:format("-----------------reject_breakcombine-----------------~n"),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, [fe]);
handle_cast({update_break_combine, BreakMessage}, State) ->
	foreach (	fun ({PlayerPid, _}) -> 
				gen_server:cast(PlayerPid, {send, BreakMessage}) 
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout);
handle_cast({update_arena_cards, OwnerPid, CardList}, State) ->
	foreach (	fun ({PlayerPid, _}) -> 
				case OwnerPid of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, append([16#88, 16#61, 1], CardList)} );
					_ ->  gen_server:cast(PlayerPid, {send, append([16#88, 16#61, 0], CardList)} )
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8861]);
handle_cast({update_ability_target, PlayerPid, DisplayCode, TargetAbility}, State) ->
	ability_utility:update_ability_target(State#arena_data.player_lists, PlayerPid, DisplayCode, TargetAbility),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m880d]);
% handle_cast({activate_card_ability_affect, PlayerPid, CardOrder, CardID}, State) ->
	% ability_utility:activate_card_ability_affect(PlayerPid, CardOrder, CardID),
	% cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8832]);
% handle_cast({activate_select_card_affect, PlayerPid, ActivateMsg}, State) ->
	% ability_utility:send_activate_select_affect (PlayerPid, ActivateMsg),
	% cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8831]);
handle_cast({activate_select_hand_card_target, Header, PlayPid, Msg}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					PlayPid -> gen_server:cast(PlayerPid, {send, Header ++ [1] ++ Msg});
					_ -> gen_server:cast(PlayerPid, {send, Header ++ [0]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayPid, State#arena_data.step_timeout, [m8821]);
handle_cast({update_hand_card_target, OwnerPid, CardOrder, CardID}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					OwnerPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#22, 1, CardOrder, <<CardID:16>>]});
					_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#22, 0, CardOrder, <<CardID:16>>]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8822]);
handle_cast({update_select_attacked_ability, PlayPid, AbilityID, AbilitySelect}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayPid of
					PlayerPid -> 
						gen_server:cast(PlayerPid, {send, [16#88, 16#16, 1, AbilityID, AbilitySelect]});
					_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#16, 0, AbilityID, AbilitySelect]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8816]);
handle_cast({destroy_arena}, State) ->
	stack_pool:remove_stack (self()),
	mnesia_play:remove_game_data(self()),
	%lib_lobby_protocol:remove_room(State#arena_data.room_id), 
	%% arena guardian will automatically alert all player that arena were removed.
	%smo_arena_guardian:delete_arena(State#arena_data.room_id),
	{stop, normal, State};
handle_cast({ask_select_attacker_ability, PlayPid, AbilityID}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayPid of
					PlayerPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#11, 1, AbilityID]});
					_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#11, 0, AbilityID]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayPid, State#arena_data.step_timeout, [8811]);
handle_cast({update_select_activation, ActivationSelected}, State) ->
	casting_controller:update_select_activation (State#arena_data.player_lists, ActivationSelected),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8804]);
handle_cast({card_break_combine, CardOwner, CardOrder, CardID}, State) ->
	foreach (fun({PlayerPid, _}) ->
		case PlayerPid of
			CardOwner ->
				gen_server:cast(PlayerPid, {send, [16#88, 16#78] ++ [1] ++ [CardOrder, <<CardID:16>>] ++ [1] ++ [10, 9]});
			_ ->	gen_server:cast(PlayerPid, {send, [16#88, 16#78] ++ [0] ++ [CardOrder, <<CardID:16>>] ++ [1] ++ [10, 9]})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
handle_cast({update_select_mystic_target, UpdateType, TargetType, TargetPid, CardOrder, CardID}, State) ->
	mystic_card_controller:update_select_mystic_target (State#arena_data.player_lists, UpdateType, TargetType, TargetPid, CardOrder, CardID),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8853]);
handle_cast({update_cast_mystic_success, Header, PlayerPid, CardOrder, CardID, TPid, TOrder, Tid}, State) ->
	mystic_card_controller:update_cast_mystic_success(State#arena_data.player_lists, Header, PlayerPid, CardOrder, CardID, TPid, TOrder, Tid),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8854]);
handle_cast({res_hand_info, ReplyPid, ReplyData}, State) ->
	gen_server:cast(ReplyPid, {send, ReplyData}),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
	
handle_cast({hand_card_add, Cards}, State) ->
	card_effect_controller:hand_cards_add(State#arena_data.player_lists, Cards),
	cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
	
% handle_cast({hand_card_add, HandSize, Cards}, State) ->
	% card_effect_controller:hand_cards_add(State#arena_data.player_lists, HandSize, Cards),
	% cast_calculate_time_left (State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout);
	
handle_cast({update_cards_to_library, PlayerPid, Header, ReplyMsg}, State) ->
	gen_server:cast(PlayerPid, {send, Header ++ ReplyMsg}),
	% foreach (	fun ({PlayPid, _}) ->
				% case PlayPid of
				% PlayerPid -> gen_server:cast(PlayPid, {send, Header ++ ReplyMsg});
				% _ -> gen_server:cast(PlayPid, {send, Header ++ ReplyOpp})
				% end
			% end,  State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8874]);
handle_cast({update_deck_size, PlayerTurnPid, PSS, PMS, OSS, OMS}, State) ->
	foreach (	fun ({PlayerPid, _}) ->
				case PlayerPid of
					PlayerTurnPid -> gen_server:cast(PlayerPid, {send, [16#88, 16#75, PSS, PMS, OSS, OMS]});
					_ -> gen_server:cast(PlayerPid, {send, [16#88, 16#75, OSS, OMS, PSS, PMS]})
				end
			end, State#arena_data.player_lists),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [8875]);
handle_cast({activate_select_mystic_target, PlayerPid, TargetType, TargetNumber, TargetSize, TargetReply, DisplayCode}, State) ->
	mystic_card_controller:activate_select_mystic_target(State#arena_data.player_lists, PlayerPid, TargetType, TargetNumber, TargetSize, TargetReply, DisplayCode),
	cast_calculate_time_left(State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [8852]);
handle_cast({activate_select_mystic_option, PlayerPid, MysticFxId, Data}, State) ->
	mystic_card_controller:activate_select_mystic_option (State#arena_data.player_lists, PlayerPid, MysticFxId, Data),
	cast_calculate_time_left (State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8855]);
handle_cast({update_select_mystic_fx, PlayerPid, MysticFxId, FxData}, State) ->
	mystic_card_controller:update_select_mystic_fx (State#arena_data.player_lists, PlayerPid, MysticFxId, FxData),
	cast_calculate_time_left (State, State#arena_data.player_lists, [], State#arena_data.step_timeout, [m8856]);
handle_cast({activate_select_curse, PlayerPid, AmountType, CurseNum, CurseSize, CurseList}, State) ->
	using_skill_controller:activate_select_curse(State#arena_data.player_lists, PlayerPid, AmountType, CurseNum, CurseSize, CurseList),
	cast_calculate_time_left(State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m886c]);
handle_cast({activate_select_element, PlayerPid, AmountType, ElemAmount, ElemSize, ElemToSelect}, State) ->
	using_skill_controller:activate_select_element(State#arena_data.player_lists, PlayerPid, AmountType, ElemAmount, ElemSize, ElemToSelect),
	cast_calculate_time_left(State, State#arena_data.player_lists, PlayerPid, State#arena_data.step_timeout, [m8862]);
	
%16#85 Update Ability----------
handle_cast({update_ability, PlayerPid, Data}, State) ->
	mod_ability_activate:set_all_ability_activate (PlayerPid, Data),
	cast_calculate_time_left (State, State#arena_data.player_lists, 
						State#arena_data.option_data, 
						State#arena_data.step_timeout,
						State#arena_data.lock_msg);
handle_cast({act_seal_power, PlayerPid, Header, Data}, State) ->
	gen_server:cast(PlayerPid, {send, [16#88, 16#86, Data]}),
	cast_calculate_time_left (State, State#arena_data.player_lists, 
						Data, 
						State#arena_data.step_timeout, 
						State#arena_data.lock_msg);
handle_cast({update_seal_power, Data}, State) ->
	Type = State#arena_data.option_data,
	mod_ability_effect:set_new_power(Data, Type),
	cast_calculate_time_left (State, State#arena_data.player_lists, 
						State#arena_data.option_data, 
						State#arena_data.step_timeout,
						State#arena_data.lock_msg);	

handle_cast({check_opponent_status, SPlayer}, State) ->
	lists:foreach(fun({PlayerPid, _}) ->
		case PlayerPid of
			SPlayer -> do_nothing;
			_ -> gen_server:cast(PlayerPid, {countdown_to_be_losser})
		end
	end, State#arena_data.player_lists),
	cast_calculate_time_left(State, State#arena_data.player_lists, State#arena_data.option_data, State#arena_data.step_timeout, State#arena_data.lock_msg);

handle_cast(_, State) ->
	{noreply,State}.

handle_info(_, State) ->
	{noreply, State}.

init(Args) ->
	[SPlayer, LoginName, PlayerName, Avatar, JoinerPid, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver, SubTime, AguardPid, PguardPid, DispPid, TID, Mode] = Args,
	put(smo_arena_guardian, AguardPid),
	put(smo_player_guardian, PguardPid),
	put(smo_dispatcher, DispPid),
	put(tournament_id, TID),
	%smo_logger:fmsg("R - ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w",[SPlayer, LoginName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver, SubTime]),
	%% call create_room
	case gen_server:call(AguardPid, {create_room, SPlayer, [self(), Rank, RoomName, Password, PlayTime, NumberOfPlayer, AllowObserver, SubTime, 0, Mode]}) of
		{ok, RoomID} ->
			put(room_id, RoomID),
			lib_database:connect(),
			%% Update requester that room was created.
			gen_server:cast(SPlayer, {send, [16#01, 16#01, [<<RoomID:16>>, RNameSize, RoomName, length(PlayerName), PlayerName, SubTime]]}),

			%% need update all client here
			%% the current process (gen_server is an exactly arena process we're working on.)
			%%[Node] = check_node(),
			%%RoomPid = spawn_link(Node, room_controller, create_new_room, [RoomID, PlayerPid, ChannelPid, LoginName]),
			create_new_room(RoomID, SPlayer, LoginName, Mode),
			
			gen_server:cast(PguardPid, {change_player_data, SPlayer, LoginName, playing_status, 1}),%,lib_lobby_protocol:change_player_data (SPlayer, LoginName, playing_status, 1),
			
			%% update arena guardian
			%gen_server:call(AguardPid, {create_arena, RoomID, RoomName, PlayerPid, RoomPid}),

			%% requester status here
			gen_server:cast(PguardPid, {update_player_status, LoginName}),
			
			%% update arena owner for self() the arena pid
			gen_server:cast(SPlayer, {set_arena, self(), RoomID}),

			case Rank of
				2 -> io:format("this is room of Ranking Match Then Don't need to Display");
				3 -> io:format("this is room of Tournament Match Then Don't need to Display");
				1 -> 
					gen_server:cast(DispPid, {broadcast_new_arena, [16#81, 16#00, <<RoomID:16>>, 0, RNameSize, RoomName, 1, PlayTime, 0, AllowObserver, HavePwd, SubTime, 1]}),
					gen_server:cast(AguardPid, {set_room_infomation, RoomID, is_ready, 1});
				0 ->
					gen_server:cast(DispPid, {broadcast_new_arena, [16#81, 16#00, <<RoomID:16>>, 0, RNameSize, RoomName, 1, PlayTime, 0, AllowObserver, HavePwd, SubTime, 0]}),
					gen_server:cast(AguardPid, {set_room_infomation, RoomID, is_ready, 1})
					%smo_dispatcher:update_new_arena([16#81, 16#00, <<RoomID:16>>, 0, RNameSize, RoomName, PlayerInRoom, PlayTime, RoomStatus, AllowObserver, HavePwd, SubTime])
			end,
			
			case is_pid(JoinerPid) of
				true -> gen_server:cast(JoinerPid, {arena_to_join, RoomID, self(), TID});
				_ -> ""
			end,
			
			%% add RoomID and third parameters '0' is for SPlayer2
			OptionData = [], 
			StepTimeOut = [15000 , now()], 
			LockMsg = 0,
	
			{ok, #arena_data{	room_id = RoomID, 
								player_lists = [{SPlayer, {LoginName, Avatar}}], 
								login_name = LoginName, 
								rank = Rank, 
								rname_size = RNameSize, 
								room_name = RoomName, 
								password = Password, 
								have_pwd = HavePwd, 
								play_time = PlayTime, 
								number_of_player = NumberOfPlayer, 
								allow_observer = AllowObserver,
								sub_time = SubTime, 
								option_data = OptionData, 
								step_timeout = StepTimeOut, 
								lock_msg = LockMsg}};
		{error, Reason} ->
			smo_logger:fmsg("Can not create room ~w", [Reason]),
			{error, Reason}
	end.
				
%% called when handle_cast returns stop.
terminate(_, _) ->
	%[RoomID, PlayerLists, LoginName, Rank, Rname_size, RoomName, Password, Have_pwd, Play_time, Number_of_player, Allow_observer, Sub_time, OptionData, StepTimeOut, LockMsg] = State,
	AguardPid = get(smo_arena_guardian),

	case gen_server:call(AguardPid, {get_room_id, self()}) of
	%case lib_lobby_protocol:get_room_id(self()) of
		[] -> "";
		[RoomID] -> 	
			%lib_lobby_protocol:remove_room(RoomID), 
			
			gen_server:cast(AguardPid, {delete_arena, RoomID})
			%smo_arena_guardian:delete_arena(RoomID)
			
	end.
	%% update arena guardian

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_time_left (State, PlayerLists, OptionData, StepTimeOut) ->
	calculate_time_left (State, PlayerLists, OptionData, StepTimeOut, [0]).

calculate_time_left (State, PlayerLists, OptionData, [TLeft, LastNow], LockMsg) ->
	TimeDiv = timer:now_diff (now(), LastNow) div 1000,
	NTLeft = TLeft - TimeDiv,
	StepTimeOut = [NTLeft, now()],
	
	%% Check if no-one in the arena then destroy arena.
	case PlayerLists of
		[] -> 
			%% Note : to exit generic server we need cast to destroy_arena
			%% in destroy arena, after clean up all necessary process, the end statement will be 
			%% finish with {stop, normal, state} instead of {noreply, State}
			gen_server:cast(self(), {destroy_arena});
		_ ->
			continue_arena
	end,
	
%	io:format ("Step time out ~p~n", [StepTimeOut]),
	{reply, ok, #arena_data{	room_id = State#arena_data.room_id, 
								player_lists = PlayerLists, 
								login_name = State#arena_data.login_name, 
								rank = State#arena_data.rank, 
								rname_size = State#arena_data.rname_size, 
								room_name = State#arena_data.room_name, 
								password = State#arena_data.password, 
								have_pwd = State#arena_data.have_pwd, 
								play_time = State#arena_data.play_time, 
								number_of_player = State#arena_data.number_of_player, 
								allow_observer = State#arena_data.allow_observer,
								sub_time = State#arena_data.sub_time,
								option_data = OptionData, 
								step_timeout = StepTimeOut, 
								lock_msg = LockMsg}}.
								
cast_calculate_time_left (State, PlayerLists, OptionData, StepTimeOut) ->
	cast_calculate_time_left (State, PlayerLists, OptionData, StepTimeOut, [0]).

cast_calculate_time_left (State, PlayerLists, OptionData, [TLeft, LastNow], LockMsg) ->
	TimeDiv = timer:now_diff (now(), LastNow) div 1000,
	NTLeft = TLeft - TimeDiv,
	StepTimeOut = [NTLeft, now()],
%	io:format ("Step time out ~p~n", [StepTimeOut]),
	{noreply, #arena_data{	room_id = State#arena_data.room_id, 
								player_lists = PlayerLists, 
								login_name = State#arena_data.login_name, 
								rank = State#arena_data.rank, 
								rname_size = State#arena_data.rname_size, 
								room_name = State#arena_data.room_name, 
								password = State#arena_data.password, 
								have_pwd = State#arena_data.have_pwd, 
								play_time = State#arena_data.play_time, 
								number_of_player = State#arena_data.number_of_player, 
								allow_observer = State#arena_data.allow_observer,
								sub_time = State#arena_data.sub_time,
								option_data = OptionData, 
								step_timeout = StepTimeOut, 
								lock_msg = LockMsg}}.
								
% join_arena(PlayerPid, PlayerName, RoomName, HeaderReply, JoinFunction, PlayerLists, SubTime) ->
	% %%%controller(PlayerPid, self()),
	% foreach (	fun ( {OwnerPid, OwnerName} ) -> 
		% mnesia_play:reply_join_room(OwnerPid, OwnerName, PlayerPid, PlayerName, RoomName, HeaderReply, JoinFunction, SubTime)
	% end, PlayerLists),
	% mnesia_play:create_new_player_data(PlayerPid).
	% 
request_player_deck(SPlayer, LoginName) ->
	{SealDeck, MysticDeck} = query_player_deck(LoginName),
	response_deck(SPlayer, SealDeck, MysticDeck).

query_player_deck(LoginName) ->
	SQLCommand = "CALL smno.sp_get_active_cards ('" ++ LoginName ++ "');",
	QueryResult =  lib_database:get_query(SQLCommand),
	%smo_logger:fmsg("~p << == >> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
	%case smo_database:sql_query(SQLCommand) of
		[{selected, _, []} , _]->
			{[], []};
		[{selected, _, Result}, _]->
			separate_player_deck (Result, [], []);
		[{error, Reason}] ->	
			smo_logger:fmsg("Query error : ~w", [Reason])
	end.
	
%{[103,116,48,55],<<103,116,48,55>>,1,undefined,0,995,1,1756}}
response_deck(PlayerPid, Seal, Mystic) ->
	{SealShuffer, MysticShuffer} = lib_arena_play:shuffer_deck(PlayerPid, Seal, Mystic),
	{SealHand, SealDeck} = split (5, SealShuffer),
	{MysticHand, MysticDeck} = split (2, MysticShuffer),
	{SH, MH} = hand_zone:get_hand_data (SealHand, MysticHand),
	HandList = SH ++ MH,
	mnesia_play:set_player_data(PlayerPid, [disallow_first_attack], first, SealDeck, MysticDeck, HandList, [], [], [], [], 0, 0, 0, []).
	%mnesia_play:set_player_data(PlayerPid, [disallow_first_attack], first, SealDeck, MysticDeck, HandList, [], [], [], [], 0, 0).

separate_player_deck ([], SealDeck, MysticDeck) ->
	{SealDeck, MysticDeck};
separate_player_deck ([{CardNo, CardSet, 1} | Cards], SealDeck, MysticDeck) ->
	<<CardID:16>> = <<CardSet, CardNo>>,
	separate_player_deck (Cards, SealDeck ++ [CardID], MysticDeck);
separate_player_deck ([{CardNo, CardSet, 0} | Cards], SealDeck, MysticDeck) ->
	<<CardID:16>> = <<CardSet, CardNo>>,
	separate_player_deck (Cards, SealDeck, MysticDeck ++ [CardID]).
	
create_new_room(RoomID, PlayerPid, LoginName, Mode) ->
	smo_logger:fmsg("Create room ID = ~p Mode ~p ~n", [RoomID, Mode]),
	mnesia_play:set_game_data(self()),
	mnesia_play:create_new_player_data(PlayerPid),
	case Mode of
		[0] ->
			{SealDeck, MysticDeck} = query_player_deck(LoginName),
			response_deck(PlayerPid, SealDeck, MysticDeck);
		_ -> set_precon_deck(PlayerPid, Mode)
	end.
	
set_precon_deck(PlayerPid, [{1, Deck}]) ->
	{SealDeck, MysticDeck} = precon_deck:deck(Deck),
	response_deck(PlayerPid, SealDeck, MysticDeck).
	
is_your_turn (PlayerPid) ->
	{ok, PlayerTurn} = mnesia_play:get_game_data (self(), player_turn),
	case PlayerPid of
		PlayerTurn -> 1;
		_ -> 0
	end.

get_command_string ([], _, GameResult, WinnerName) ->
	case GameResult of
		game_draw -> "'draw'";
		_ ->	"'" ++ WinnerName ++ "'"
	end;
get_command_string ([{PlayerPid, {LoginName, _}} | L], PlayerPid, GameResult, WinnerName) ->
	case GameResult of
		player_loss ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, WinnerName);
		game_draw ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, WinnerName);
		player_win ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, LoginName)
	end;
get_command_string ([{_, {LoginName, _}} | L], PlayerPid, GameResult, WinnerName) ->
	case GameResult of
		player_loss ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, LoginName);
		game_draw ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, WinnerName);
		player_win ->
			"'" ++ LoginName ++ "', " ++ get_command_string (L, PlayerPid, GameResult, WinnerName)
	end.

create_log_play_game ([{_, _}]) -> [];
create_log_play_game ([{_, {LoginName1, _}}, {_, {LoginName2, _}}|_]) ->
	AguardPid = get(smo_arena_guardian),
	RankStatus = gen_server:call(AguardPid, {get_room_rank, self()}),
	case RankStatus of
		[3] -> "";
		_Other ->
			SQLCommand = "Call smno.sp_insertLogGame ('" ++ [LoginName1] ++"','" ++ [LoginName2] ++ "');",
			smo_logger:fmsg("~p~n", [SQLCommand]),
			%smo_database:sql_query(SQLCommand).
			lib_database:get_query(SQLCommand)
	end.
	
create_log_end_game (DataString) ->
	SQLCommand = "Call smno.sp_updateLogGame (" ++ DataString ++ ");",
	%smo_database:sql_query(SQLCommand).
	lib_database:get_query(SQLCommand).
	
send_msg_end_game (PlayerPid, IsYourResult, GameResult) ->
	case {IsYourResult, GameResult} of
		{your_result, player_win} ->
			gen_server:cast(PlayerPid, {send, [16#8b, 16#0f, 1]});
		{your_result, player_loss} ->
			gen_server:cast(PlayerPid, {send, [16#8b, 16#0f, 0]});
		{other_result, player_win} ->
			gen_server:cast(PlayerPid, {send, [16#8b, 16#0f, 0]});
		{other_result, player_loss} ->
			gen_server:cast(PlayerPid, {send, [16#8b, 16#0f, 1]});
		{_, game_draw} ->
			gen_server:cast(PlayerPid, {send, [16#8b, 16#0f, 2]})
	end.

send_player_leave_room (PlayerPid, LeavePid) ->
	case PlayerPid of
		LeavePid -> 
			gen_server:cast(PlayerPid, {send, [16#83, 16#ff, 1]});
		_ -> 
			gen_server:cast(PlayerPid, {send, [16#83, 16#ff, 0]})
	end.
	
% delete(PlayerPid, [{PlayerPid, LoginName}|T], L) ->
	% lib_lobby_protocol:change_player_data (PlayerPid, LoginName, playing_status, 0),
	% gen_server:cast(smo_player_guardian, {update_player_status, LoginName}),
	% %%room_controller ! {player_leave_room, ChannelPid, PlayerPid, self()},
	% {reverse(T, L)};
% delete(PlayerPid, [H|T], L) -> delete(PlayerPid, T, [H|L]);
% delete(_, [], L) -> {"????", L}.

delete(PlayerPid, PlayerList) ->
	{value, {PlayerPid, {LoginName, Avatar}}} = lists:keysearch(PlayerPid, 1, PlayerList),
	PguardPid = get(smo_player_guardian),
	gen_server:cast(PguardPid, {change_player_data, PlayerPid, LoginName, playing_status, 0}),
	%lib_lobby_protocol:change_player_data (PlayerPid, LoginName, playing_status, 0),
	gen_server:cast(PguardPid, {update_player_status, LoginName}),
	{PlayerList -- [{PlayerPid, {LoginName, Avatar}}]}.			

get_game_result([], _, GameResult, WinnerName) ->
	case GameResult of
		game_draw -> ["3"];
		_ ->	[WinnerName]
	end;
get_game_result([{PlayerPid, {LoginName, _}} | L], PlayerPid, GameResult, WinnerName) ->
	case GameResult of
		player_loss ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, WinnerName);
		game_draw ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, WinnerName);
		player_win ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, LoginName)
	end;
get_game_result([{_, {LoginName, _}} | L], PlayerPid, GameResult, WinnerName) ->
	case GameResult of
		player_loss ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, LoginName);
		game_draw ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, WinnerName);
		player_win ->
			[LoginName] ++ get_game_result(L, PlayerPid, GameResult, WinnerName)
	end.

get_leave_game_result([], _) -> [];
get_leave_game_result([{PlayerPid, {LoginName, _}} | L], PlayerPid) -> get_leave_game_result( L, PlayerPid) ++ [LoginName];
get_leave_game_result([{_, {LoginName, _}} | L], PlayerPid) -> [LoginName] ++ get_leave_game_result(L, PlayerPid). 
	
arrange_player([Player1, Player2, GameResult]) ->
	case GameResult of
		Player1 -> [Player1, Player2, "1"];
		Player2 -> [Player2, Player1, "1"];
		_ -> [Player1, Player2, GameResult]
	end.

calculate_bonus_point([PlayerWin, PlayerLoss, GameResult], PlayerLists, LeaveType) ->
	[{Pid1, {Player1, _}}, {Pid2, {_Player2, _}}] = PlayerLists,
	if
		PlayerWin =:= Player1 -> 
			WinnerPid = Pid1,
			LoserPid = Pid2;	
		true -> 
			WinnerPid = Pid2,
			LoserPid = Pid1
	end,
	Rwin = player_rating(PlayerWin),
	Rloss = player_rating(PlayerLoss),
	Dwin = player_dena(PlayerWin),
	Dlose = player_dena(PlayerLoss),
	PlayedSubturn = get(subturn),
	WinnerShrine = shrine_zone:get_shrine_level(WinnerPid),
	if 
		GameResult =:= "0" ->
			LoserShrine = get(leaverShrine);
		true -> 
			LoserShrine = shrine_zone:get_shrine_level(LoserPid)
	end,
	ShrineBonus = (WinnerShrine * 1.2 + LoserShrine * 0.8) / 15,
	if
		PlayedSubturn =:= undefined; PlayedSubturn < 2 ->
			PlayTimeBonus = 0;
		PlayedSubturn > 44 ->
			PlayTimeBonus = 2.1;
		true ->
			PlayTimeBonus = (PlayedSubturn - 2) / 20
	end,
	BonusPoint = (ShrineBonus * 1.2) + (PlayTimeBonus * 0.8),
	Ewin = 1/(1+math:pow(10, (Rloss - Rwin) / 4000)),
	Eloss = 1/(1+math:pow(10, (Rwin - Rloss) / 4000)),
	CrnLose = round(0.5 * (12.5 * ((- 2.5) - Eloss) + BonusPoint * (BonusPoint + 5))),
	if
		GameResult =:= "0"; GameResult =:= "1" ->
			CrnWin = round(BonusPoint * 12.5 * (2.5 - Ewin)),
			CdnWin = round(65 * math:pow(BonusPoint, 2));
		true ->
			CrnWin = CrnLose,
			CdnWin = round(20 * math:pow(BonusPoint, 2))
	end,
	io:format("PlayerWin is ~p, and PlayerLoss is ~p With GameResult ~p~n", [Rwin, Rloss, GameResult]),
	case 	GameResult of
		"1" ->
			NewRwin = round(Rwin + CrnWin),
			NewDenaWin = round(Dwin + CdnWin),
			NewRloss = round(Rloss + CrnLose),
			CdnLose = round(20 * math:pow(BonusPoint, 2)),
			NewDenaLose = round(Dlose + CdnLose),
			LoserCrn = (CrnLose),
			SQLCommand = "CALL smno.sp_update_rating('"++PlayerWin++"'," ++integer_to_list(NewRwin)++"," ++integer_to_list(NewDenaWin)++",'" ++PlayerLoss++"'," ++integer_to_list(NewRloss)++"," ++integer_to_list(NewDenaLose)++"," ++GameResult++");",
			lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p~n", [SQLCommand]),
			win_lost_rating(PlayerLists, [PlayerWin, CrnWin, CdnWin, PlayerLoss, LoserCrn, CdnLose, GameResult]);
		"0"  ->
			case LeaveType of
				disconnect ->
					NewRwin = round(Rwin + CrnWin),
					NewDenaWin = round(Dwin + CdnWin),
					if
						CrnLose < 0 ->
							NewCrnLose = 2 * CrnLose;
						true ->
							NewCrnLose = 0
					end,
					NewRloss = round(Rloss + NewCrnLose),
					CdnLose = 0,
					NewDenaLose = round(Dlose + CdnLose),
					LeaveCase = GameResult;
				_ -> 
					NewRwin = round(Rwin + CrnWin),
					NewDenaWin = round(Dwin + CdnWin),
					if
						CrnLose >= 0 ->
							NewCrnLose = 0;
						true ->
							NewCrnLose = 1.5 * CrnLose
					end,
					NewRloss = round(Rloss + NewCrnLose),
					CdnLose = round(10 * math:pow(BonusPoint, 2)),
					NewDenaLose = round(Dlose + CdnLose),
					LeaveCase = "2"
			end,
			LoserCrn = round(NewCrnLose),
			SQLCommand = "CALL smno.sp_update_rating('"++PlayerWin++"'," ++integer_to_list(NewRwin)++"," ++integer_to_list(NewDenaWin)++",'" ++PlayerLoss++"'," ++integer_to_list(NewRloss)++"," ++integer_to_list(NewDenaLose)++"," ++GameResult++");",
			lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p~n", [SQLCommand]),
			win_lost_rating(PlayerLists, [PlayerWin, CrnWin, CdnWin, PlayerLoss, LoserCrn, CdnLose, GameResult]);
		_ ->
			NewRwin = round(Rwin + CrnWin),
			NewDenaWin = round(Dwin + CdnWin),
			NewRloss = round(Rloss + CrnLose),
			CdnLose = round(20 * math:pow(BonusPoint, 2)),
			NewDenaLose = round(Dlose + CdnLose),
			LoserCrn = (CrnLose),
			SQLCommand = "CALL smno.sp_update_rating('"++PlayerWin++"'," ++integer_to_list(NewRwin)++"," ++integer_to_list(NewDenaWin)++",'" ++PlayerLoss++"'," ++integer_to_list(NewRloss)++"," ++integer_to_list(NewDenaLose)++"," ++GameResult++");",
			lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p~n", [SQLCommand]),
			win_lost_rating(PlayerLists, [PlayerWin, CrnWin, CdnWin, PlayerLoss, LoserCrn, CdnLose, GameResult])
			%%win_lost_rating(PlayerLists, [PlayerWin, NewRwin, PlayerLoss, NewRloss, GameResult])
	end.
	
%%win_lost_rating(PlayerLists, PlayerWin, PlayerLoss, GameResult, CrnWin, CrnLose, CdnWin, CdnLose) ->
	%%NewRwin = player_rating(PlayerWin),
	%%NewRloss = player_rating(PlayerLoss),
	%%NewDenaWin = player_dena(PlayerWin),
	%%NewDenaLose = player_dena(PlayerLoss),
%%	win_lost_rating(PlayerLists, [PlayerWin, NewRwin, NewDenaWin, PlayerLoss, NewRloss, NewDenaLose, GameResult]).
	
win_lost_rating(PlayerLists, [PlayerWin, CrnWin, CdnWin, PlayerLoss, LoserCrn, CdnLose, GameResult]) ->
	if 
		GameResult =:= "1"; GameResult =:= "0" ->
			lists:foreach(
										fun({Pid, {PlayerName, _}}) ->
											card_info:remove_card_info(Pid),
											case PlayerName of
												PlayerWin ->
													gen_server:cast(Pid, {send, [16#8b, 16#0e, 1, <<CrnWin:16>>, <<LoserCrn:16>>, <<CdnWin:16>>, <<CdnLose:16>>]});
												PlayerLoss	 ->
													gen_server:cast(Pid, {send, [16#8b, 16#0e, 0, <<LoserCrn:16>>, <<CrnWin:16>>, <<CdnLose:16>>, <<CdnWin:16>>]})
											end
									 end, PlayerLists);
		true ->
			lists:foreach(
										fun({Pid, {PlayerName, _}}) -> 
											card_info:remove_card_info(Pid),
											case PlayerName of
												PlayerWin ->
													gen_server:cast(Pid, {send, [16#8b, 16#0e, 2, <<CrnWin:16>>, <<LoserCrn:16>>,  <<CdnWin:16>>, <<CdnLose:16>>]});
												PlayerLoss	 ->
													gen_server:cast(Pid, {send, [16#8b, 16#0e, 2, <<LoserCrn:16>>, <<CrnWin:16>>,  <<CdnLose:16>>, <<CdnWin:16>>]})
											end
										end, PlayerLists)
	end.
	
	
	
	
%%win_lost_rating(PlayerLists, [PlayerWin, NewRwin, NewDenaWin, PlayerLoss, NewRloss, NewDenaLose, GameResult]) ->
%%	if 
%%		GameResult =:= "1"; GameResult =:= "0" ->
%%			lists:foreach(
%%										fun({Pid, PlayerName}) ->
%%											card_info:remove_card_info(Pid),
%%											case PlayerName of
%%												PlayerWin ->
%%													gen_server:cast(Pid, {send, [16#8b, 16#0e, 1, <<NewRwin:16>>, <<NewRloss:16>>, <<NewDenaWin:16>>, <<NewDenaLose:16>>]});
%%												PlayerLoss	 ->
%%													gen_server:cast(Pid, {send, [16#8b, 16#0e, 0, <<NewRloss:16>>, <<NewRwin:16>>, <<NewDenaWin:16>>, <<NewDenaLose:16>>]})
%%											end
%%									 end, PlayerLists);
%%		true ->
%%			lists:foreach(
%%										fun({Pid, PlayerName}) -> 
%%											card_info:remove_card_info(Pid),
%%											case PlayerName of
%%												PlayerWin ->
%%													gen_server:cast(Pid, {send, [16#8b, 16#0e, 2, <<NewRwin:16>>, <<NewRloss:16>>,  <<NewDenaWin:16>>, <<NewDenaLose:16>>]});
%%												PlayerLoss	 ->
%%													gen_server:cast(Pid, {send, [16#8b, 16#0e, 2, <<NewRloss:16>>, <<NewRwin:16>>,  <<NewDenaWin:16>>, <<NewDenaLose:16>>]})
%%											end
%%										end, PlayerLists)
%%	end.

player_rating({LoginName, _}) -> player_rating(LoginName);
player_rating(LoginName) ->
	SQLCommand = "SELECT  smno.fc_get_rating('" ++ LoginName ++ "');",
	case lib_database:get_query(SQLCommand) of
		{selected,_,[{Rating}]} -> Rating;
		{error, Reason} ->	io:format("Query error : ~p~n", [Reason])
	end.
player_dena({LoginName, _}) -> player_dena(LoginName);
player_dena(LoginName) ->
	SQLCommand = "CALL smno.sp_get_cash('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
	case  QueryResult of
		[{selected, _, [{_, Denarian}]}, _] -> Denarian;
		{error, Reason} ->	io:format("Query error : ~p~n", [Reason])
	end.
	
% player([]) -> [];
% player([{_,PlayerName}|Tail]) ->
	% [{PlayerName}] ++ player(Tail).
	
calculate_tournament_statistic([PlayerWin, PlayerLoss, GameResult], _PlayerLists, EndType) -> 
	TID = get(tournament_id),

	Match_ID = get(match_ID),
	
	EndResult = 
	case 	GameResult of
		"0" ->
			case EndType of
				disconnect ->
					GameResult;
				_ -> 
					"2"
			end;
		_ -> GameResult
	end,
	
	%%SQLCommand = "CALL smno.sp_league_update_logGame (" ++ integer_to_list(TID) ++  ", '" ++ PlayerWin ++ "', '" ++ PlayerLoss ++ "', "++ EndResult ++");",
	SQLCommand = "CALL smno.sp_league_update_logGame ("++ integer_to_list(Match_ID) ++" , '" ++ PlayerWin ++ "', '" ++ PlayerLoss ++ "', "++ EndResult ++");",
	QResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("update log game ~p -- ~p~n", [SQLCommand, QResult]).

	
get_chat_owner_avatar([],_) -> {0,0};
get_chat_owner_avatar([{SPlayer, {_, Avatar}} | T], SPlayer) -> Avatar;
get_chat_owner_avatar([{_, {_, _}} | T], SPlayer) -> get_chat_owner_avatar(T, SPlayer).
