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
-module (target_attack).

-compile (export_all).

check_assign_attack_target(PlayerPid, CardOwner, CardOrder, CardID) ->
	OpponentPid = mnesia_play:get_opponent_pid (CardOwner),
	{ControllerPid, UncontrolPid, opponent} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, uncontrol),
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี (ฝั่งนี้จะถูกโจมตี)
	% Check ว่าผู้สั่งโจมตี ได้รับผล ของการสั่งห้ามโจมตี ไป ไลน์ใดๆ หรือไม่
	
	PossibleTarget = card_utility:get_all_card(UncontrolPid, seal_card, arena_zone),
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี ทีไม่ได้่ควบคุมโดยผู้สั่งโจมตี
	PossibleUncon = attacker_uncontrol(ControllerPid, UncontrolPid, PossibleTarget),
	% Seal ทุกใบของฝั่งผู้โจมตี
	OurSeal =card_utility:get_all_card(ControllerPid, seal_card, arena_zone),
	% Seal ทุกใบของฝั่งเราที่จะถูกโจมตี
	OurOppControl = attacker_uncontrol(ControllerPid, UncontrolPid, OurSeal),	
	
	% Seal ทุกใบที่สามารถเป็นเป้าได้บน At line ของฝั่งที่ ผู้สั่งโจมตีไม่ได้ควบคุม
	AtLineTarget =
	case continuous_ability:check_player_got_effect(PlayerPid, player_effect, [{player_assign_atk, {disallow, to_at}}]) of
		have_effect -> AtLinePossible = [], [];
		have_no_effect ->
			case check_line_to_attack(CardFx) of
				at_line -> 
					AtLinePossible = get_card_on_line(PossibleTarget, 1),
					get_card_on_line(PossibleUncon, 1);
				to_ordinary_line -> 
					AtLinePossible = get_card_on_line(PossibleTarget, 1),
					get_card_on_line(PossibleUncon, 1);
				df_line -> 
					AtLinePossible = [],
					[]
			end
	end,
	
	% Seal ทุกใบที่สามารถเป็นเป้าบน Df line ของฝั่งที่ ผู้สั่งโจมตีไม่ได้ควบคุม
	DfLineTarget = 
	case continuous_ability:check_player_got_effect(PlayerPid, player_effect, [{player_assign_atk, {disallow, to_df}}]) of
		have_effect -> [];
		have_no_effect -> get_card_on_line(PossibleUncon, 0)
	end,
	
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี (ฝั่งนี้จะถูกโจมตี) ที่ไม่ติด Dimension Curse
	OppGotDimension = get_card_on_line(PossibleTarget, null),
	% Seal ทุกใบของฝั่งเราที่อยู่บน At Line
	OurOppConATLine = get_card_on_line(OurOppControl, 1),
	% Seal ทุกใบของฝั่งเราที่อยู่บน Df Line
	_OurOppConDFLine = get_card_on_line(OurOppControl, 0),
	
	ToSupportFx = function_utility:is_contain([{attack, to_supporter}], CardFx),
	
	AttackTarget =
	case AtLineTarget of
		% เป้าทั้งหมดที่จะเป็นไปได้ กรณี ไม่มี Seal ที่ At Line ฝั่งตรงข้าม และ ไม่มี Seal ที่ถูกฝั่งตรงข้ามควบคุมบน At Line ฝ่ายเรา
		[] -> 
			case AtLinePossible of
				[] -> get_support_target(DfLineTarget, ToSupportFx) ++ get_support_target(OurOppConATLine, ToSupportFx);
				_ -> 
					CheckFxTarget = check_card_attack_effect(CardOwner, CardOrder, CardID, DfLineTarget, CardFx),
					
					get_support_target(OurOppConATLine, ToSupportFx) ++ get_support_target(CheckFxTarget, ToSupportFx)
			end;
		% เป้าทั้งหมดที่จะเป็นไปได้ กรณี มี Seal ที่ At Line ฝั่งตรงข้าม
		_ ->	
			CheckFxTarget = 
			case continuous_ability:check_player_got_effect(PlayerPid, player_effect, [{player_assign_atk, {disallow, across_to_df}}]) of
				have_effect -> [];
				have_no_effect -> check_card_attack_effect(CardOwner, CardOrder, CardID, DfLineTarget, CardFx)
			end,
			
			get_support_target(AtLineTarget, ToSupportFx) ++
			get_support_target(OurOppConATLine, ToSupportFx) ++
			get_support_target(CheckFxTarget, ToSupportFx)
	end,
	io:format("attack target 1~p~n", [AttackTarget]),
	AtkTarget = AttackTarget -- [{CardOwner, CardOrder, CardID}],
	case AtkTarget of
		% กรณีที่ไม่มีเป้าบนสนาม
		[] ->
			case PossibleTarget of
				% กรณีที่ไม่มีเป้าบนสนาม และไม่มี Seal เหลือในสนามแล้ว
				[] -> 	check_attack_on_hand(UncontrolPid, ControllerPid);
				% กรณีที่ไม่มีเป้าบนสนาม แต่มี Seal เหลือในสนาม
				_ -> 
					case OppGotDimension of
						% ไม่มีเป้า แต่ มี Seal บนสนาม และ ใน Seal เหล่านั้น ไม่ได้มี Seal ตัวไหนที่ติด Dimension Curse แสดงว่า Seal เหล่านั้นติด Charm Curse
						[] -> {no_target, can_not_attack_to_hand};								
						%บนสนามฝั่งตรงข้ามมี Seal อยู่ แต่ ติด Dimension Curse ที่ At Line หรือ Df Line
						_ -> 
							case length(OppGotDimension) < length(PossibleTarget) of
								% หมายความว่า Seal บนสนามมีมากกว่า Seal ที่ติด Dimension แสดงว่า ในจำนวนนั้น ต้องมี Seal ที่ติด Charm ของ ผู้โจมตีอยู่
								true -> {no_target, can_not_attack_to_hand};
								_ -> check_attack_on_hand(UncontrolPid, ControllerPid)
							end
					end
			end;
		% กรณีที่มีเป้าบนสนาม
		_Target ->
			% smo_logger:fmsg("all Target Remain to Attack are ~p~n", [_Target]),
			% smo_logger:fmsg("all OurOppConATLine are ~p~n", [OurOppConATLine]),
			% smo_logger:fmsg("all _OurOppConDFLine are ~p~n", [_OurOppConDFLine]),
			case _Target of
				%แต่การ์ด นั้นเป็นการ์ดของเราที่ฝ่ายตรงข้่ามควบคุม บน AT line ฝ่ายเรา
				% ให้เช็คต่อว่า Card ที่สั่งโจมตี เป็น Seal ที่มีเจ้าของเดียวกันกับ 
				OurOppConATLine -> 
					case check_all_owner(CardOwner, OurOppConATLine)  of
						check_target_zone -> {check_target_zone, OurOppConATLine};
						attack_to_target -> 
							case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, OurOppConATLine, assign_atk) of
								[] -> {no_target, can_not_attack_to_hand};
								_RemainTarget -> {have_target, _RemainTarget}%{have_target, OurOppConATLine}
							end
					end;
				%การ์ดของเราที่ฝ่ายตรงข้่ามควบคุม บน DF line ฝ่ายเรา
				_OurOppConDFLine -> 
					case check_all_owner(CardOwner, _OurOppConDFLine)  of
						check_target_zone -> {check_target_zone, _OurOppConDFLine};
						attack_to_target -> 
							case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, _OurOppConDFLine, assign_atk) of
								[] -> {no_target, can_not_attack_to_hand};
								_RemainTarget -> {have_target, _RemainTarget}%{have_target, _OurOppConDFLine}
							end
					end;
				_ ->
					case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, _Target, assign_atk) of
						[] -> {no_target, can_not_attack_to_hand};
						_RemainTarget -> %smo_logger:fmsg("remain target to attack ~p~n", [_RemainTarget]), 
							{have_target, _RemainTarget}
					end
			end
	end.
	
check_attack_target(_PlayerPid, CardOwner, CardOrder, CardID) ->
	OpponentPid = mnesia_play:get_opponent_pid (CardOwner),
	{ControllerPid, UncontrolPid, opponent} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OpponentPid, uncontrol),
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี (ฝั่งนี้จะถูกโจมตี)
	PossibleTarget = card_utility:get_all_card(UncontrolPid, seal_card, arena_zone),
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี ทีไม่ได้่ควบคุมโดยผู้สั่งโจมตี
	PossibleUncon = attacker_uncontrol(ControllerPid, UncontrolPid, PossibleTarget),
	% Seal ทุกใบของฝั่งผู้โจมตี
	OurSeal =card_utility:get_all_card(ControllerPid, seal_card, arena_zone),
	% Seal ทุกใบของฝั่งเราที่จะถูกโจมตี
	OurOppControl = attacker_uncontrol(ControllerPid, UncontrolPid, OurSeal),	
	
	% Seal ทุกใบที่สามารถเป็นเป้าได้บน At line ของฝั่งที่ ผู้สั่งโจมตีไม่ได้ควบคุม
	AtLineTarget = 
	case check_line_to_attack(CardFx) of
		at_line -> 
			AtLinePossible = get_card_on_line(PossibleTarget, 1),
			get_card_on_line(PossibleUncon, 1);
		to_ordinary_line -> 
			AtLinePossible = get_card_on_line(PossibleTarget, 1),
			get_card_on_line(PossibleUncon, 1);
		df_line -> 
			AtLinePossible = [],
			[]
	end,
	% Seal ทุกใบที่สามารถเป็นเป้าบน Df line ของฝั่งที่ ผู้สั่งโจมตีไม่ได้ควบคุม
	DfLineTarget = get_card_on_line(PossibleUncon, 0),
	
	% Seal ทุกใบของฝั่งตรงข้ามผู้สั่งโจมตี (ฝั่งนี้จะถูกโจมตี) ที่ไม่ติด Dimension Curse
	OppGotDimension = get_card_on_line(PossibleTarget, null),
	% Seal ทุกใบของฝั่งเราที่อยู่บน At Line
	OurOppConATLine = get_card_on_line(OurOppControl, 1),
	% Seal ทุกใบของฝั่งเราที่อยู่บน Df Line
	_OurOppConDFLine = get_card_on_line(OurOppControl, 0),
	
	ToSupportFx = function_utility:is_contain([{attack, to_supporter}], CardFx),
	
	AttackTarget =
	case AtLineTarget of
		% เป้าทั้งหมดที่จะเป็นไปได้ กรณี ไม่มี Seal ที่ At Line ฝั่งตรงข้าม และ ไม่มี Seal ที่ถูกฝั่งตรงข้ามควบคุมบน At Line ฝ่ายเรา
		[] -> 
			case AtLinePossible of
				[] -> get_support_target(DfLineTarget, ToSupportFx) ++ get_support_target(OurOppConATLine, ToSupportFx);
				_ -> 
					CheckFxTarget = check_card_attack_effect(CardOwner, CardOrder, CardID, DfLineTarget, CardFx),
					
					get_support_target(OurOppConATLine, ToSupportFx) ++ get_support_target(CheckFxTarget, ToSupportFx)
			end;
		% เป้าทั้งหมดที่จะเป็นไปได้ กรณี มี Seal ที่ At Line ฝั่งตรงข้าม
		_ ->	
			CheckFxTarget = check_card_attack_effect(CardOwner, CardOrder, CardID, DfLineTarget, CardFx),
			
			get_support_target(AtLineTarget, ToSupportFx) ++
			get_support_target(OurOppConATLine, ToSupportFx) ++
			get_support_target(CheckFxTarget, ToSupportFx)
	end,
	io:format("attack target 1~p~n", [AttackTarget]),
	AtkTarget = AttackTarget -- [{CardOwner, CardOrder, CardID}],
	case AtkTarget of
		% กรณีที่ไม่มีเป้าบนสนาม
		[] ->
			case PossibleTarget of
				% กรณีที่ไม่มีเป้าบนสนาม และไม่มี Seal เหลือในสนามแล้ว
				[] -> 	check_attack_on_hand(UncontrolPid, ControllerPid);
				% กรณีที่ไม่มีเป้าบนสนาม แต่มี Seal เหลือในสนาม
				_ -> 
					case OppGotDimension of
						% ไม่มีเป้า แต่ มี Seal บนสนาม และ ใน Seal เหล่านั้น ไม่ได้มี Seal ตัวไหนที่ติด Dimension Curse แสดงว่า Seal เหล่านั้นติด Charm Curse
						[] -> {no_target, can_not_attack_to_hand};								
						%บนสนามฝั่งตรงข้ามมี Seal อยู่ แต่ ติด Dimension Curse ที่ At Line หรือ Df Line
						_ -> 
							case length(OppGotDimension) < length(PossibleTarget) of
								% หมายความว่า Seal บนสนามมีมากกว่า Seal ที่ติด Dimension แสดงว่า ในจำนวนนั้น ต้องมี Seal ที่ติด Charm ของ ผู้โจมตีอยู่
								true -> {no_target, can_not_attack_to_hand};
								_ -> check_attack_on_hand(UncontrolPid, ControllerPid)
							end
					end
			end;
		% กรณีที่มีเป้าบนสนาม
		_Target ->
			% smo_logger:fmsg("all Target Remain to Attack are ~p~n", [_Target]),
			% smo_logger:fmsg("all OurOppConATLine are ~p~n", [OurOppConATLine]),
			% smo_logger:fmsg("all _OurOppConDFLine are ~p~n", [_OurOppConDFLine]),
			case _Target of
				%แต่การ์ด นั้นเป็นการ์ดของเราที่ฝ่ายตรงข้่ามควบคุม บน AT line ฝ่ายเรา
				% ให้เช็คต่อว่า Card ที่สั่งโจมตี เป็น Seal ที่มีเจ้าของเดียวกันกับ 
				OurOppConATLine -> 
					case check_all_owner(CardOwner, OurOppConATLine)  of
						check_target_zone -> {check_target_zone, OurOppConATLine};
						attack_to_target -> 
							case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, OurOppConATLine, attack) of
								[] -> {no_target, can_not_attack_to_hand};
								_RemainTarget -> {have_target, _RemainTarget}%{have_target, OurOppConATLine}
							end
					end;
				%การ์ดของเราที่ฝ่ายตรงข้่ามควบคุม บน DF line ฝ่ายเรา
				_OurOppConDFLine -> 
					case check_all_owner(CardOwner, _OurOppConDFLine)  of
						check_target_zone -> {check_target_zone, _OurOppConDFLine};
						attack_to_target -> 
							case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, _OurOppConDFLine, attack) of
								[] -> {no_target, can_not_attack_to_hand};
								_RemainTarget -> {have_target, _RemainTarget}%{have_target, _OurOppConDFLine}
							end
					end;
				_ ->
					case check_other_be_target_disallow({CardOwner, CardOrder, CardID}, _Target, attack) of
						[] -> {no_target, can_not_attack_to_hand};
						_RemainTarget -> %smo_logger:fmsg("remain target to attack ~p~n", [_RemainTarget]), 
							{have_target, _RemainTarget}
					end
			end
	end.

get_support_target([], _) -> [];
get_support_target([{MainOwner, MainOrder, MainID}|Support], ToSupportFx) ->
	case ToSupportFx of
		[] -> [{MainOwner, MainOrder, MainID}|Support];
		_ -> %[{MainOwner, MainOrder, MainID}] ++ function_utility:card_match_condition(arena_zone:get_support_seals(MainOwner, MainOrder, MainID), [{card_type, seal}]) ++ get_support_target(Support, ToSupportFx)
			[{MainOwner, MainOrder, MainID}] ++ arena_zone:get_support_seals(MainOwner, MainOrder, MainID) ++ get_support_target(Support, ToSupportFx)
	end.

check_all_owner(_CardOwner, []) -> check_target_zone;
check_all_owner(CardOwner, [{CardOwner, _CardOrder, _CardID}|Tail]) -> check_all_owner(CardOwner, Tail);
check_all_owner(_CardOwner, [{_OtherCardOwner, _CardOrder, _CardID}|_Tail]) -> attack_to_target.
	
	
check_attack_on_hand(UncontrolPid, _ControllerPid) ->
	case mnesia_play:get_player_data(UncontrolPid, hand_cards) of
		{ok, []} -> {no_target, can_not_attack_to_hand};
		_ -> {no_target, attack_to_hand}
	end.

get_cards_controller_arena (PlayerPid, Controller, WhoArena) ->
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	Cards = 
	case WhoArena of
		player ->
			card_utility:get_all_card (PlayerPid, seal_card, arena_zone);
		opponent ->
			card_utility:get_all_card (OpponentPid, seal_card, arena_zone);
		all ->
			card_utility:get_all_card (seal_card, arena_zone)
	end,
	check_cards_controller (PlayerPid, Controller, Cards).
%------------------------------------------------------------------------------------------------------------------------------
%เช็คเป้าหมาย Attack All ดูเจ้าของการ์ดอย่างเดียว ไม่ต้องดูที่ผู้ควบคุม
%-------------------------------------------------------------------------------------------------------------------------------
get_cards_target_arena_all(PlayerPid, CardOwner, CardOrder, CardID, AttackCase) ->
	OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	Cards = card_utility:get_all_card(OpponentPid, seal_card, arena_zone),
	get_card_target_all(Cards, CardOwner, CardOrder, CardID, AttackCase).

get_card_target_all(C, CardOwner, CardOrder, CardID, AttackCase) ->
	get_card_target_all(C, [], CardOwner, CardOrder, CardID, AttackCase).
	
get_card_target_all([{{TCardOwner, TCardOrder, TCardID}, _} | Tail], CardList, CardOwner, CardOrder, CardID, AttackCase) ->
	TCardFx = card_utility:get_all_card_effect(TCardOwner, TCardOrder, TCardID),
	%smo_logger:fmsg("target effect are ~p~n", [TCardFx]),
	%Interest = [{attack_to_s, {disallow, sp_less_than_s}}, {attack_to_s, {disallow, seal_not_combine}}, {attack_to_s, {disallow, all}}, {protect_attack, [all]}],
	%FxRemain = function_utility:is_contain(Interest, TCardFx),
	%smo_logger:fmsg("remain effect to check are ~p~n", [FxRemain]),
	case check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, TCardFx, AttackCase) of
		true -> get_card_target_all(Tail, [{TCardOwner, TCardOrder, TCardID}] ++ CardList, CardOwner, CardOrder, CardID, AttackCase);
		_ -> get_card_target_all(Tail, CardList, CardOwner, CardOrder, CardID, AttackCase)
	end;
get_card_target_all([], CardList, _, _, _, _) -> CardList.
%-------------------------------------------------------------------------------------------------------------------------------
attacker_uncontrol(_, _, []) -> []; 
attacker_uncontrol(AtkPid, UnattackerPid, [{{CardOwner, CardOrder, CardID}, _} | Cards]) ->
	CardOpp = 
	case CardOwner of
		AtkPid -> UnattackerPid;
		_ -> AtkPid
	end,
	{ControllerPid, _, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, CardOpp, controller),
	case ControllerPid of
		UnattackerPid -> [{CardOwner, CardOrder, CardID}] ++attacker_uncontrol(AtkPid, UnattackerPid, Cards);
		_ -> attacker_uncontrol(AtkPid, UnattackerPid, Cards)
	end.	
	
those_seal_we_control(_, _, []) -> []; 
those_seal_we_control(AttackerPid, UnAtkPid, [{{CardOwner, CardOrder, CardID}, _} | Cards]) ->
	{ControllerPid, _, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, UnAtkPid, controller),
	case ControllerPid of
		AttackerPid -> those_seal_we_control(AttackerPid, UnAtkPid, Cards);
		_ -> [{CardOwner, CardOrder, CardID}] ++those_seal_we_control(AttackerPid, UnAtkPid, Cards)
	end.

% ตรวจสอบว่า ผู้ควบคุมของการ์ดใบนั้นเป็นคนเดียวกับที่ต้องการหรือไม่ -
check_cards_controller(_, _, []) -> [];
check_cards_controller(PlayerPid, Controller, [{{CardOwner, CardOrder, CardID}, _} | Cards]) ->
	%PossibleTarget = check_cards_controller (PlayerPid, Controller, Cards),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	case ControllerPid of
		PlayerPid -> player;
		_ -> opponent
	end,
	% case card_utility:check_card_controller(CardOwner, CardOrder, CardID) of
		% controller_allow -> % ผู้ควบคุมเป็นเจ้าของการ์ด
			% case PlayerPid of
				% CardOwner -> % ผู้เล่นกับผู้ควบคุมเป็นคนเดียวกัน
					% CardController = player;
				% _ ->	CardController = opponent
			% end;
		% controller_not_allow -> % ผู้ควบคุมไม่ได้เป็นเจ้าของการ์ด
			% case PlayerPid of
				% CardOwner -> % ผู้เล่นกับผู้ควบคุมเป็นคนละคน
					% CardController = opponent;
				% _ ->	CardController = player
			% end
	% end,
	case ControllerPid of
		Controller -> [{CardOwner, CardOrder, CardID}] ++ check_cards_controller(PlayerPid, Controller, Cards);
		_ ->	check_cards_controller(PlayerPid, Controller, Cards)
	end.
	
% หาการ์ดที่อยู่ในไลน์ที่กำหนด
get_card_on_line ([], _) -> [];
get_card_on_line([{CardOwner, CardOrder, CardID}| Cards], null) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Dimension = function_utility:is_contain([{curse, dimension_curse}], CardFx),
	case Dimension of
		[] -> get_card_on_line(Cards, null);
		_Dimension -> 
			[{CardOwner, CardOrder, CardID}] ++ get_card_on_line(Cards, null)
	end;
	
get_card_on_line([{CardOwner, CardOrder, CardID}| Cards], LineCheck) ->
	CardLine = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, arena_zone),
	case CardLine of
		{ok, LineCheck} ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			Disallow = function_utility:is_contain([{curse, dimension_curse}], CardFx),
			case Disallow of
				[] ->
					[{CardOwner, CardOrder, CardID}] ++ get_card_on_line(Cards, LineCheck);
				_Dimension ->
					get_card_on_line(Cards, LineCheck)
			end;
		_ ->	get_card_on_line(Cards, LineCheck)
	end;
	
get_card_on_line([{{CardOwner, CardOrder, CardID}, _} | Cards], null) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Dimension = function_utility:is_contain([{curse, dimension_curse}], CardFx),
	case Dimension of
		[] -> get_card_on_line(Cards, null);
		_Dimension ->	
			[{CardOwner, CardOrder, CardID}] ++ get_card_on_line(Cards, null)
	end;	
	
get_card_on_line ([{{CardOwner, CardOrder, CardID}, _} | Cards], LineCheck) ->
	CardLine = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, arena_zone),
	case CardLine of
		{ok, LineCheck} ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			Disallow = function_utility:is_contain([{curse, dimension_curse}], CardFx),
			case Disallow of
				[] ->
					[{CardOwner, CardOrder, CardID}] ++ get_card_on_line(Cards, LineCheck);
				_Dimension ->
					get_card_on_line (Cards, LineCheck)					
			end;
		_ ->	get_card_on_line (Cards, LineCheck)
	end.

check_card_attack_effect (_, _, _, _, []) -> [];
check_card_attack_effect (CardOwner, CardOrder, CardID, DefendLineTarget, [{_, Fx, _} | CardFx]) ->
	MoreTarget = get_attack_target_by_effect (CardOwner, CardOrder, CardID, DefendLineTarget, Fx),
	MoreTarget ++ check_card_attack_effect (CardOwner, CardOrder, CardID, DefendLineTarget -- MoreTarget, CardFx).

get_attack_target_by_effect (_CardOwner, _CardOrder, _CardID, _DefendLineTarget, []) -> [];
get_attack_target_by_effect (CardOwner, CardOrder, CardID, DefendLineTarget, [FxCheck | Fx]) ->
	case FxCheck of
		{attack, to_df} ->
			DefendLineTarget;
		{attack, {to_df, CompareType, Power}} ->
		%{attack, to_df_more_sp} ->
			%{ok, MainSpeed} = arena_zone:get_card_power (CardOwner, CardOrder, CardID, speed),
			{ok, MainPower} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, Power),
			%TargetMoreSp = get_match_condition_target (DefendLineTarget, more, MainSpeed),
			TargetMatch = get_match_condition_target(DefendLineTarget, CompareType, MainPower, Power),
			%AddOnTarget = get_attack_target_by_effect(CardOwner, CardOrder, CardID, DefendLineTarget -- TargetMoreSp, Fx),
			AddOnTarget = get_attack_target_by_effect(CardOwner, CardOrder, CardID, DefendLineTarget -- TargetMatch, Fx),
			TargetMatch ++ AddOnTarget;
		% {attack, to_df_less_sp} ->
			% {ok, MainSpeed} = arena_zone:get_card_power (CardOwner, CardOrder, CardID, speed),
			% TargetLessSp = get_match_condition_target (DefendLineTarget, less, MainSpeed),
			% AddOnTarget = get_attack_target_by_effect (CardOwner, CardOrder, CardID, DefendLineTarget -- TargetLessSp, Fx),
			% TargetLessSp ++ AddOnTarget;
		% {attack, to_df_less_df} ->
			% {ok, MainDefend} = arena_zone:get_card_power (CardOwner, CardOrder, CardID, defend),
			% TargetLessDf = get_match_condition_target(DefendLineTarget, less, MainDefend),
			% AddOnTarget = get_attack_target_by_effect (CardOwner, CardOrder, CardID, DefendLineTarget -- TargetLessSp, Fx),
			% TargetLessSp ++ AddOnTarget;
		_ ->	[]
	end.
	
check_line_to_attack([]) -> to_ordinary_line;
check_line_to_attack([{_, Fx, _} | CardFx]) ->
	case check_line(Fx) of
		no_assing_line_target -> check_line_to_attack(CardFx);
		to_df -> df_line;
		to_at -> at_line
	end.

check_line([]) -> no_assing_line_target;
check_line([FxCheck|Fx]) ->
	case FxCheck of
		{attack, only_to_df} ->  to_df;
		{attack, only_to_at} ->  to_at;
		_ -> check_line(Fx)
	end.
	
	

get_match_condition_target ([], _, _, _) -> [];
get_match_condition_target ([{CardOwner, CardOrder, CardID} | Target], CompareType, MainPower, Power) ->
	{ok, TargetPower} = arena_zone:get_card_power (CardOwner, CardOrder, CardID, Power),
	case CompareType of
		more ->
			if
				TargetPower > MainPower ->
					[{CardOwner, CardOrder, CardID}] ++ get_match_condition_target(Target, CompareType, MainPower, Power);
				true ->
					get_match_condition_target(Target, CompareType, MainPower, Power)
			end;
		less ->
			if
				TargetPower < MainPower ->
					[{CardOwner, CardOrder, CardID}] ++ get_match_condition_target(Target, CompareType, MainPower, Power);
				true ->
					get_match_condition_target(Target, CompareType, MainPower, Power)
			end
	end.
%---------------- Check เป้าการโจมตีของการ โจมตี All
check_target_attack_all(PlayerPid, CardOwner, CardOrder, CardID, AttackCase) ->
	%PossibleTarget = get_cards_controller_arena (PlayerPid, opponent, all),
	% AttackCase แบ่งเป็น Case ของการ สั่งโจมตี กับ การโจมตี (คือการเรียก Check อีกครั้งหลังผ่าน Interfere การโจมตีไปแล้ว)
	PossibleTarget = get_cards_target_arena_all(PlayerPid, CardOwner, CardOrder, CardID, AttackCase),
	ExcludeTarget = get_card_on_line(PossibleTarget, null),
	PosTarget = PossibleTarget -- ExcludeTarget, 
	check_attack_all_target(PosTarget, CardOwner, CardOrder, CardID).

check_attack_all_target([], _, _, _) -> [];
check_attack_all_target([{TPid, TOrder, TID} | Cards], CardOwner, CardOrder, CardID) ->
	case ability_effect:check_card_effect(TPid, TOrder, TID, protect_attack_all) of
		{ok, have_effect} ->
			check_attack_all_target(Cards, CardOwner, CardOrder, CardID);
		{ok, no_effect} ->
			[{TPid, TOrder, TID}] ++ check_attack_all_target(Cards, CardOwner, CardOrder, CardID)
	end.
% check ว่า Seal ใบไหนที่ไม่สามารถเป็นเป้าของการโจมตีได้ โดยใช้ AttackCase เป็นตัวระบุว่า เป็นเป้่าของการสั่งโจมตี หรือการโจมตี ที่ผ่าน Interfere ไปแล้ว
check_other_be_target_disallow({_CardOwner, _CardOrder, _CardID}, [], _) -> [];
check_other_be_target_disallow({CardOwner, CardOrder, CardID}, [{TCardOwner, TCardOrder, TCardID}|Target], AttackCase) ->
	CardZone  = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	TCardFx = 
	case CardZone of
		support_cards -> [];
		arena_zone -> card_utility:get_all_card_effect(TCardOwner, TCardOrder, TCardID)
	end,
	case check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, TCardFx, AttackCase) of
		true -> [{TCardOwner, TCardOrder, TCardID}]++check_other_be_target_disallow({CardOwner, CardOrder, CardID}, Target, AttackCase);
		_ -> check_other_be_target_disallow({CardOwner, CardOrder, CardID}, Target, AttackCase)
	end.
	
check_remain_disallow(_, _, [], _) -> true;
check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, [{_, CardFx, _} | RemainFx], AttackCase) ->
	case CardFx of
		% กรณี attack_to_s จะเป็นเป้าไม่ได้ทั้งการสั่งโจมตีหรือ การโจมตี ไม่ต้อง check AttackCase 
		[{attack_to_s, {disallow, Condition}}] -> 
			case Condition of
				% ถ้า Seal ที่โจมตีมี Speed มากกว่า Target ให้ Target เป็นเป้าได้
				sp_less_than_s ->
					AttackerSp = game_info:card_sp({arena_zone, {CardOwner, CardOrder, CardID}}),
					case attribute_check:sp_check({arena_zone, {TCardOwner, TCardOrder, TCardID}}, AttackerSp, less_than_s) of
						% กรณี Target Speed มากกว่า จะ Return true และ S เป็นเป้าไม่ได้
						true -> cannot_be_target;
						_Res -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
					end;
				seal_not_combine ->
					case attribute_check:combine_check({arena_zone, {CardOwner, CardOrder, CardID}}, y) of
						% กรณี Seal ที่โจมตีรวมร่างอยู่ S เป็นเป้าได้
						true -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase);
						_Res -> _Res
					end;
				all -> cannot_be_target;
				{RequireType, Value} ->
					case RequireType of
						naming ->	{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_naming);
						type ->		{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_type);
						elem ->		{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_element);
						_ ->	InterestData = []
					end,
					case Value -- InterestData of
						[] -> cannot_be_target;
						_ -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
					end;
				_ -> false
			end;
		% กรณี assign_attack_to_s จะเป็นเป้าไม่ได้เฉพาะการสั่งโจมตี เท่านั้น แต่หากหลังจาก Interfere ได้รับ Effect assign_attack_to_s มา
		% ก็จะยังสามารถเป็นเป้่าได้อยู่ จึงต้่อง check AttackCase
		[{assign_attack_to_s, {disallow, Condition}}] ->
			case AttackCase of
				assign_atk ->
					case card_utility:check_card_status(CardOwner, CardOrder, CardID, assign_attack_success, arena_zone) of
						{ok, have_no_status} ->
							case Condition of
								% ถ้า Seal ที่โจมตีมี Speed มากกว่า Target ให้ Target เป็นเป้าได้
								sp_less_than_s ->
									AttackerSp = game_info:card_sp({arena_zone, {CardOwner, CardOrder, CardID}}),
									case attribute_check:sp_check({arena_zone, {TCardOwner, TCardOrder, TCardID}}, AttackerSp, less_than_s) of
										% กรณี Target Speed มากกว่า จะ Return true และ S เป็นเป้าไม่ได้
										true -> cannot_be_target;
										_Res -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
									end;
								seal_not_combine ->
									case attribute_check:combine_check({arena_zone, {CardOwner, CardOrder, CardID}}, y) of
										% กรณี Seal ที่โจมตีรวมร่างอยู่ S เป็นเป้าได้
										true -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase);
										_Res -> _Res
									end;
								all -> cannot_be_target;
								{RequireType, Value} ->
									case RequireType of
										naming ->	{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_naming);
										type ->		{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_type);
										elem ->		{ok, InterestData} = mnesia_odbc:get_seal_data(CardID, card_element);
										_ ->	InterestData = []
									end,
									case Value -- InterestData of
										[] -> cannot_be_target;
										_ -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
									end;
								_ -> false
							end;
						{ok, have_status} -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
					end;
				attack -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
			end;
		[{protect_attack, [all]}] ->	cannot_be_target;
		_ -> check_remain_disallow({CardOwner, CardOrder, CardID}, {TCardOwner, TCardOrder, TCardID}, RemainFx, AttackCase)
	end.