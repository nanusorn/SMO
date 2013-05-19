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
-module (play_utility).

-import (lists, [flatlength/1, append/2]).

-compile (export_all).

get_opponent (_, []) -> no_opponent_data;
get_opponent (PlayerPid, [{PlayerPid, _} | T]) -> get_opponent (PlayerPid, T);
get_opponent (_, [{OpponentPid, _} | _]) -> OpponentPid;
get_opponent (_, [_]) -> {error}.

get_owner_pid (PlayerPid, WhoCard) ->
	case WhoCard of
		1 -> PlayerPid;
		0 -> mnesia_play:get_opponent_pid (PlayerPid);
		101 -> PlayerPid;
		100 -> mnesia_play:get_opponent_pid (PlayerPid);
		_ -> io:format ("Get owner pid error ~p~n", [WhoCard])
	end.

check_ready_step (PlayerListInto, PlayerPid, StepTo) ->
%	io:format ("Player list ~p PlayerPid ~p~n", [PlayerListInto, PlayerPid]),
	check_ready_step (PlayerListInto, PlayerPid, StepTo, 0).

check_ready_step (PlayerListInto, PlayerPid, StepTo, StepOption) ->
	case is_pid(PlayerPid) of
		true ->
			case is_list (PlayerListInto) of
				true ->
					check_player_ready(PlayerListInto, PlayerPid, StepTo, StepOption);
				false ->
					case is_pid(PlayerListInto) of
						true -> check_player_ready([PlayerListInto], PlayerPid, StepTo, StepOption);
						false -> io:format("require option data ~p as list of pid ~n", [PlayerListInto])
					end
			end;
		false ->
			io:format("require paremeter ~p as pid ~n", [PlayerPid])
	end.
	
check_player_ready (PlayerListInto, PlayerPid, StepTo, StepOption) ->
	case check_ready (PlayerListInto, PlayerPid, []) of
		{ok, not_change} -> List = PlayerListInto;
		{ok, PlayerList} -> List = PlayerList
	end,
	ListSize = flatlength(List),
	case ListSize of
		0 ->	List;
		1 ->	List;
		2 ->	already_player (StepTo, StepOption), []
	end.

already_player (StepTo, OptionData) ->
	case StepTo of
		into_next_step -> into_next_step();
		into_main -> main_step:into_step();
		into_checkup -> check_up_step:checkup_step ();
		into_draw -> draw_step:into_step();
		check_draw -> draw_step:check_draw (OptionData);
		into_discard -> discard_step:into_step();

		% --- Casting Zone ---
		growth_selected -> interfere_step:return_play (check_play_step);
		%growth_material_selected -> interfere_step:return_play (check_play_step);

		select_growth ->
			{PlayerPid, Data} = OptionData,
			casting_card:select_growth (PlayerPid, Data);
		into_sub_interfere -> interfere_step:into_sub_interfere ();
		%mystic_to_shrine -> mystic_card:activate_mystic_to_shrine ();
		
		%check_changing_line_ability -> line_change:check_changing_line_ability();

		%attack_target_selected -> assign_atk:check_select_target_case();
		attack_target_selected -> new_assign_atk:ability_select_when_attacker();
		hand_attacker_updated -> hand_atk:ability_active_when_assign_attack();
		
		
		%response_update_select_ability -> %assign_atk:response_update_select_ability();
		res_update_select_ability -> mod_ability_activate:generalize_effect_to_each_target();
		response_select_attacked_ability -> assign_atk:response_select_attacked_ability();
		
		%response_update_card_use_skill -> new_seal_skill:check_assign_use_skill_ability();
		%response_update_card_use_skill -> seal_skill:response_update_card_use_skill ();
		%response_update_select_skill -> seal_skill:set_seal_use_skill ();
		response_update_select_skill -> new_seal_skill:check_assign_use_skill_ability();
		%response_update_player_mp -> seal_skill:check_ability_activate (); 
		
		count_shrine_level -> shrine_zone:count_shrine_level ();

		%remove_card_to_zone -> interfere_step:return_play (check_play_step);

		%card_ability_affect -> ability_affect:activate_ability_affect ();
		
		checK_other_mystic_effect -> new_mystic_check:select_mystic_target();
		
		return_stack -> interfere_step:return_play(check_play_step);

		_ -> io:format("Step to out of range ~p~n", [StepTo])
	end.

check_ready_step (PlayerListInto, PlayerPid) ->
	case check_ready (PlayerListInto, PlayerPid, []) of
		{ok, not_change} -> List = PlayerListInto;
		{ok, PlayerList} -> List = PlayerList
	end,
	ListSize = flatlength(List),
	case ListSize of
		0 -> {not_ready, List};
		1 -> {not_ready, List};
		2 -> {ready}
	end.

check_ready ([], PlayerPid, List) -> {ok, List ++ [PlayerPid]};
check_ready ([PlayerPid | _], PlayerPid, _) -> {ok, not_change};
check_ready ([H | T], PlayerPid, List) -> check_ready (T, PlayerPid, List ++ [H]).

% check_special_comand (CardOwner, CardOrder, CardID) -> 
	% {ok, CardStatus} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, card_status),
	% {ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	% case check_card_more_attack (CardStatus) of
		% {more_attack, Status} ->
			% card_utility:remove_card_status (CardOwner, CardOrder, CardID, Status),
			% card_utility:add_card_status (CardOwner, CardOrder, CardID, inactive_attack_allow),
			% assign_atk_controller:check_attack_condition (PlayerPid, CardOwner, CardOrder, CardID);
		% no_more_attack ->
			% case ability_effect:check_card_effect (CardOwner, CardOrder, CardID, additional_select_attack) of
				% {ok, have_effect} ->
% %					io:format ("Have an effect additional attack~n"),
% %					assign_atk:player_select_additional_attack ([1]);
					% smo_logger:msg("Logger #1"),
					% gen_server:cast(self(), {activate_select_additional_attack, PlayerPid});
				% {ok, no_effect} ->
					% next_command_to_client ()
			% end
	% end.

% next_command_to_client () ->
	% stack_pool:pop_stack_out (self()),
	% case stack_pool:get_last_stack (self(), play) of
		% {ok, StackPlay} -> interfere_step:return_play(StackPlay);
		% {error, _} -> 
			% smo_logger:msg("Logger #2"),
			% gen_server:cast(self(), {act_next_command})
	% end.

% check_card_more_attack ([]) -> no_more_attack;
% check_card_more_attack ([Status | CardStatus]) ->
	% %io:format("Status ~p~n", [Status]),
	% case is_atom(Status) of
		% true ->
			% case atom_to_list (Status) of
				% % "more_attack_X"
				% [109,111,114,101,95,97,116,116,97,99,107, 95 | _] ->
					% {more_attack, Status};
				% _ ->	check_card_more_attack (CardStatus)
			% end;
		% _ -> check_card_more_attack (CardStatus)
	% end.	

out_of_turn (PlayerPid, StepPlay) ->
	io:format("Player try to -~p- out of step~n", [StepPlay]),
	smo_logger:msg("Logger #3"),
	gen_server:cast(self(), {play_out_of_turn, PlayerPid}).

into_next_step() ->
	case mnesia_play:get_game_data(self(), game_step) of
		{ok, wait} ->
			mnesia_play:set_game_data (self(), game_step, checkup),
			%%smo_logger:fmsg("_______prepare for into checkup______ with arena id = ~w", [self()]),
			gen_server:cast(self(), {update_into_checkup});
		{ok, checkup} ->
			mnesia_play:set_game_data (self(), game_step, draw),
			%%smo_logger:fmsg("________update into draw_______ ~w",[self()]),
			gen_server:cast(self(), {update_into_draw});
		{ok, draw} ->
			mnesia_play:set_game_data (self(), game_step, discard),
			%%NextStep = update_into_discard;
			gen_server:cast(self(), {update_into_discard});
		{ok, discard} ->
			mnesia_play:set_game_data (self(), game_step, inter1),
			%%NextStep = update_into_interfere;
			gen_server:cast(self(), {update_into_interfere});
		{ok, inter1} ->
			mnesia_play:set_game_data (self(), game_step, main),
			%%NextStep = update_into_main;
			gen_server:cast(self(), {update_into_main});
		{ok, main} ->
			mnesia_play:set_game_data (self(), game_step, inter2),
			%%NextStep = update_into_interfere;
			gen_server:cast(self(), {update_into_interfere});
		{ok, inter2} ->
			mnesia_play:set_game_data (self(), game_step, refillmp),
			%%NextStep = update_into_mp_clean;
			gen_server:cast(self(), {update_into_mp_clean});
		{ok, refillmp} ->
			mnesia_play:set_game_data (self(), game_step, eos),
			%%NextStep = update_into_eos;
			gen_server:cast(self(), {update_into_eos});
		{ok, eos} -> mnesia_play:set_game_data (self(), game_step, checkup),
			%%NextStep = update_into_checkup;
			gen_server:cast(self(), {update_into_checkup});
		error ->
			io:format ("<play utility> Get game data error!!!~n"),
			%NextStep= error
			error
	end.
%	io:format ("<play utility> Next step into ~p~n", [NextStep]),
	%%self() ! {NextStep}.

check_option_part (MainCharacter) ->
	case MainCharacter of
%		65 -> %% ???????? A
		elem -> card_element;
%		66 -> %% ???????? B
		naming ->	card_naming;
%		67 -> %% ???????? C
		type -> card_type;
		68 -> %% ???????? D
			curse;
		name -> %% ???????? E
			card_name;
%		76 -> %% ???????? L
		level ->level;
		85 -> %% ??????? Level ??????
			upper_from;
		_ -> io:format ("Select option part out of range - ~p~n", [MainCharacter])
	end.

get_card_infomation({{CardOwner, CardOrder, CardID}, _}, MajorSub, MinorSub) ->
	get_card_infomation({CardOwner, CardOrder, CardID}, MajorSub, MinorSub);
get_card_infomation({CardOwner, CardOrder, CardID}, MajorSub, MinorSub) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			OptionPart = play_utility:check_option_part(MajorSub),
			{ok, CardOption} = card_utility:get_card_option(CardOwner, CardOrder, CardID),
			{ok, CardInfo} = seal_card:get_seal_option(CardOption, information),
			{ok, Field} = seal_card:get_power_type(OptionPart, CardInfo),
			%{ok, Field} = mnesia_odbc:get_seal_data(CardID, OptionPart),
			FieldID = MinorSub,
%			case OptionPart of
%				card_name -> FieldID = get_parse_name ([MajorSub, MinorSub]);
%				_ -> %<<FieldID:16>> = <<MajorSub, MinorSub>>
%					FieldID = {MajorSub, MinorSub}
%			end,
			{Field, FieldID};
		is_not_seal -> {[], 0}
	end.

% check_match (_, []) -> {not_found};
% check_match (FieldID, [FieldID|_]) -> {found};
% check_match (FieldID, [_|T]) -> check_match (FieldID, T).

% get_parse_name (NameID) ->
	% case NameID of
		% "E1" -> "Palanalcar, the Baby Dragon";
		% "E2" -> "Niltinco, the Baby Dragon";
		% "E3" -> "Dimminuial, the Baby Dragon";
		% "E4" -> "Pony Unicorn";
		% "E5" -> "Ratatosk";
		% "E6" -> "Firat";
		% "E7" -> "Sore Wing";
		% "E8" -> "Falkner, The Beast Tamer";
		% "E9" -> "Zea bird";
		% "E0" -> "Souless";
		% _ -> io:format("Name id out of range ~p~n", [NameID])
	% end.

% add_example_room () ->
	% F =	fun() ->
			% lists:foreach (fun mnesia:write/1, example_table())
		% end,
	% mnesia:transaction(F).

% clear_all_room() ->
	% mnesia:clear_table(room).

% example_table () ->
	% [
		% {room, 7, 0, "dd", "", 2, 1, 30, 0, 0},
		% {room, 9, 0, "ee", "", 2, 1, 30, 0, 0},
		% {room, 11, 0, "ff", "", 2, 1, 30, 0, 0},
		% {room, 10, 0, "gg", "", 2, 1, 30, 0, 0},
		% {room, 12, 0, "abc", "", 2, 1, 30, 0, 0},
		% {room, 87, 0, "abcde", "", 2, 1, 30, 0, 0},
		% {room, 54, 0, "??? ??????", "", 2, 1, 30, 0, 0},
		% {room, 19, 0, "????????????", "", 2, 1, 30, 0, 0},
		% {room, 15, 0, "??????????????!!!", "", 2, 1, 30, 0, 0}
	% ].

% separate_comma_data (Data) ->
	% transform_data (Data, [], []).

% transform_data ([], UData, SepData) -> append(SepData, UData);
% transform_data (null, _, SepData) -> SepData;
% transform_data ([44 | T], UData, SepData) ->
	% transform_data(T, [], append(SepData, UData));
% transform_data ([B1, B2 | T], UData, SepData) ->
	% <<B:16>> = <<B1, B2>>,
	% transform_data(T, append(UData, [B]), SepData);
% transform_data ([Data], _, _) ->
	% io:format("<play utility> Data transform error is ~p~n", [Data]).

% decode(Input) ->
	% Translate = erlang:md5(Input),
	% L = binary_to_list(Translate),
	% hex(L).

% hex(L) when list (L) ->
  % lists:flatten([hex(I) || I <- L]);
% hex(I) when I > 16#f ->
  % [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
% hex(I) -> [$0, hex0(I)].

% hex0(10) -> $a;
% hex0(11) -> $b;
% hex0(12) -> $c;
% hex0(13) -> $d;
% hex0(14) -> $e;
% hex0(15) -> $f;
% hex0(I) ->  $0 +I.

% create_connection ()  ->
	% ConnectStr = "DSN=InternalMySQL;UID=summoner;PWD=$umm0ns,f,6-",
	% DBConnectionType = [{timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	% odbc:connect(ConnectStr, DBConnectionType ++ [{auto_commit, on}]).
