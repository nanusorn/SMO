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
-module (curse).

-export ([check_curse_status/2]).

check_curse_status ([], _) -> {ok, allow};
check_curse_status ([{_, Effect, _} | T], Condition) ->
%	io:format("Effect is ~p~n", [Effect]),
	case check_effect_cause (Effect, Condition) of
		{ok, allow} -> check_curse_status (T, Condition);
		{ok, disallow} -> {ok, disallow}
	end.

check_effect_cause ([], _) -> {ok, allow};
check_effect_cause ([{Condition, disallow} | _], Condition) -> {ok, disallow};
check_effect_cause ([{curse, Curse} | T], Condition) ->
%	io:format ("Curse condition ~p ~p ~n", [Curse, Condition]),
	case get_curse_condition (Curse, Condition) of
		allow -> check_effect_cause (T, Condition);
		disallow -> {ok, disallow}
	end;
check_effect_cause ([_ | T], Condition) -> check_effect_cause (T, Condition). 

%% Summoner Curse = {stone_curse, freeze_curse, charm_curse, poison_curse, death_curse, last_dance_curse, dimension_curse}
get_curse_condition (Curse, Condition) ->
	case Curse of
		stone_curse -> check_stone_condition (Condition);
		freeze_curse -> check_freeze_condition (Condition);
		{charm_curse, _} -> check_charm_condition (Condition);
		poison_curse -> allow;
		death_curse -> allow;
		{last_dance_curse, _} -> allow;
		dimension_curse -> check_dimension_condition (Condition);
		_ ->	allow
	end.

%% 709.3. Stone Curse
%% 709.3.1. Seal ที่ติด Stone Curse จะไม่สามารถถูกสั่งการได้
%% 709.3.2. Seal ที่ติด Stone Curse ไม่สามารถโจมตี, ใช้ Skill, Combination, Break Combination และ กำหนด Line โดยผู้เล่น
%% 709.3.3. Seal ที่ติด Stone Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Abilityตามปกติ
%% 709.3.4. หาก Seal ที่ติด Stone Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้
%% 709.3.5. Ability ของ Seal ที่ติด Stone Curse ยังคงทำงานตามปกติ
check_stone_condition (Condition) ->
	case Condition of
		assign_atk -> disallow;
		attacker -> disallow;
		use_skill -> disallow;
		combine -> disallow;
		break_combine -> disallow;
		assign_line -> disallow;
		_ -> allow
	end.

check_freeze_condition (Condition) ->
	case Condition of
		assign_atk -> disallow;
		assign_line -> disallow;
		_ -> allow
	end.

check_charm_condition (Condition) ->
	case Condition of
		combine -> disallow;
		break_combine -> disallow;
		_ -> allow
	end.

%% 709.9. Dimension Curse
%% 709.9.1. Seal ที่ติด Dimension Curse ไม่อยู่ใน Line ใดๆ แต่ยังคงอยู่ในสนาม เว้นแต่มี Effect อื่นใดกำหนดให้ผิดไปจากนี้
%% 709.9.2. Seal ที่ติด Dimension Curse ไม่สามารถถูกสั่งการได้
%% 709.9.3. Seal ที่ติด Dimension Curse ไม่สามารถโจมตี, ใช้ Skill, Combination, Break Combination และ กำหนด Line โดยผู้เล่น
%% 709.9.4. Seal ที่ติด Dimension Curse ไม่ตกเป็นเป้าหมายของการโจมตี และจะไม่รับความเสียหายจากการโจมตี
%% 709.9.5. Seal ที่ติด Dimension Curse ยังคงตกเป็นเป้าหมายของ Mystic Card, Skill หรือ Ability ตามปกติ
%% 709.9.6. Ability ของ Seal ที่ติด Dimension Curse ยังคงทำงานตามปกติ
%% 709.9.7. เมื่อ Seal หายจาก Dimension Curse หาก Seal นั้นไม่อยู่ใน Line ใด ๆ ให้ผู้ควบคุม Seal นั้น กำหนด Line ให้กับ Seal ใบนั้น
%% ถ้า Seal นั้นอยู่ใน Line ใด ๆ ก่อนแล้วไม่ต้องทำการกำหนด Line ใหม่
check_dimension_condition (Condition) ->
	case Condition of
		assign_atk -> disallow;
		use_skill -> disallow;
		combine -> disallow;
		break_combine -> disallow;
		assign_line -> disallow;
		attack_target -> disallow;
		_ -> allow
	end.