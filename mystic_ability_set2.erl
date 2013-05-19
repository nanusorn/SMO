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
-module(mystic_ability_set2).
-compile(export_all).

ability() ->
	[
		%Seal ทุกใบฝ่ายเราสลับ At กับ Df (ผลที่ทำให้ At เปลี่ยนแปลงไปเปลี่ยนแปลง Df และผลที่ทำให้ Df เปลี่ยนแปลงไปเปลี่ยนแปลง At)
		{mystic_ability,
			s2_no094_a1, 606, 1, n,
				n, [],
				n, null, [], null, null, [], null,
					null,
						y,
							n, null, {}, [], null,
							n, null, null, {}, [], null, 										 
							n, null, {}, [], null,
							n, null, [], null, null, [],
								null, {}, [], null,
							y, owner, [arena_zone], seal, null, [],
								immediately, {do_not_need_select, 0}, [{combat, {when_attack, {df, power}}}, {combat, {when_target, {power, df}}}], null
		},
		%ผู้เล่นทุกคนนำ Seal 1 ใบในสนามฝ่ายตนที่มี At มากที่สุดออกจากเกม (ไม่สามารถร่าย [S] ได้ถ้าเราไม่มี Seal ในสนาม)
		{mystic_ability,
			s2_no095_a1a, 607, 1, n,
				%y, [{seal_owner_arena_include_self, {[], 1}}],
				y, [{{owner, [arena_zone], seal, y}, {[], 1}}],
				y, opponent, [arena_zone], seal, nulll, [], 1,
					null,
						null,
							n, null, {}, [], null,
							n, null, null, {}, [], null, 										
							n, null, {}, [], null,
							n, null, [null], null, null, [],
								null, {}, [], null,
							y, owner, [arena_zone], seal, null, [{attack, own_arena_highest}],
								immediately, {player_select_exact_target, 1}, [{player_action, move_to_remove_zone}], 0
		},
		{mystic_ability,
			s2_no095_a1b, 607, 1, n,
				y, [{{owner, [arena_zone], seal, y}, {[], 1}}],
				y, opponent, [arena_zone], seal, nulll, [], 1,
					null,
						null,
							n, null, {}, [], null,
							n, null, null, {}, [], null, 										
							n, null, {}, [], null,
							n, null, [null], null, null, [],
								null, {}, [], null,
							y, opponent, [arena_zone], seal, null, [{attack, opp_arena_highest}],
								immediately, {opponent_select_exact_target, 1}, [{player_action, move_to_remove_zone}], 0
		},
		%Seal ที่ [S] ติด Mp ค่าใช้ Skill -3 และสามารถใช้ Skill ได้โดยไม่ต้องตรงตามเงื่อนไขการรวมร่างใน Subturn นี้
		{mystic_ability,
			s2_no096_a1, 608, 1, n,
				n, [],
				y, null, [arena_zone], seal, null, [{action, {n, card_casting}}], 1,
					null,
						null,
							n, null, {}, [], null,
							n, null, null, {}, [], null, 										
							n, null, {}, [], null,
							y, null, [arena_zone], seal, null, [{action, {n, card_casting}}],
								null, {player_select_exact_target, 1}, [{ms, -3}, {skill, without_combine}], end_of_subturn,
							n, null, [null], null, null, [],
								null, {}, [], null
								
		},
		%เมื่อ [S] ตก Shrine จากสนาม เรา Mp +3
		{mystic_ability,
			s2_no097_a1, 609, 1, n,
				n, [],
				n, null, [], null, null, [], null,
					null,
						null,
							n, null, {}, [], null,
							n, null, null, {}, [], null, 										
							y, immediately, {do_not_need_select, 0}, [], depend_on_s,
							n, null, [], null, null, [],
								null, {}, [], nul,
							n, null, [], null, null, [],
								null, {}, [], null
								
		},
		%ดูการ์ดทุกใบในมือฝ่ายตรงข้าม จากนั้นเลือกทิ้ง Mystic Card 1 ใบในมือฝ่ายตรงข้าม
		{mystic_ability,
			s2_no098_a1a, 610, 1, n,
				n, [],
					y, opponent, [hand_cards], null, null, [], 1,
						s2_no098_a1b,
							null,
								y, immediately, {do_not_need_select, 0}, [{reveal_card_on_hand, {opponent, all}}], 0,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], nul,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		{mystic_ability,
			s2_no098_a1b, 610, 1, n,
				n, [],
					y, opponent, [hand_cards], mystic, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, opponent, [hand_cards], mystic, null, [],
									immediately, {player_select_exact_target, 1}, [{action, discard}], 0
		},
		%เราจำนวนการ์ดในมือสูงสุด +1
		{mystic_ability,
			s2_no099_a1, 611, 1, n,
				n, [],
					n, null, [], null, null, [], null,
						null,
							null,
								y, immediately, {do_not_need_select, 0}, [{card_on_hand, 1}], depend_on_s,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], nul,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		% Seal ที่ [S] ติดโจมตีด้วย Df เมื่อ Seal ที่ [S] ติดถูกโจมตี เทียบ Df ของ Seal ที่ [S] ติดกับค่าพลังของ Seal ที่โจมตีจนออกจากการโจมตีนั้น
		% Seal ที่ [S] ติด สลับ At กับ Df (ผลที่ทำให้ At เปลี่ยนแปลงไปเปลี่ยนแปลง Df และผลที่ทำให้ Df เปลี่ยนแปลงไปเปลี่ยนแปลง At) [interfere]
		%ยกเว้น: Seal ฝ่ายตรงข้าม
		{mystic_ability,
			s2_no100_a1, 612, 1, n,
				n, [],
					y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}],
									immediately, {player_select_exact_target, 1}, [{swap_power, {attack, defend}}], depend_on_s,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%Seal ที่ [S] ติด แยกการรวมร่าง
		{mystic_ability,
			s2_no101_a1, 613, 1, y,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {combine, {card_type, is_seal}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {combine, {card_type, is_seal}}],
									immediately, {player_select_exact_target, 1}, [{action, break_combine}], 0,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%เลือก 1 อย่าง
			%- ถ้า Seal ที่ Crescent ติดคือ [Dark] Seal นั้น At +2 
			%- ถ้า Seal ที่ Crescent ติดคือ [Water] Seal นั้น At +1, Mp -1 
			%ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine] 
		{mystic_ability,
			s2_no102_a1, 614, 1, n,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}], 1,
						null,
							y,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}],
									immediately, {player_select_exact_target, 1}, [{check_condition, {[{elem, 5}], {at, 2}}}], depend_on_s,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		{mystic_ability,
			s2_no102_a2, 614, 2, n,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}], 1,
						null,
							y,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}],
									immediately, {player_select_exact_target, 1}, [{check_condition, {[{elem, 2}], {at, 1}}}, {check_condition, {[{elem, 2}], {ma, -1}}}, {check_condition, {[{elem, 2}], {ms, -1}}}], depend_on_s,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%Seal ที่ [S] ติด ติด Dimension Curse 
		{mystic_ability,
			s2_no103_a1, 615, 1, n,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {protect_curse, {n, dimension_curse}}, {curse, {n, dimension_curse}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {protect_curse, {n, dimension_curse}}, {curse, {n, dimension_curse}}],
									immediately, {player_select_exact_target, 1}, [{curse, dimension_curse}], depend_on_s,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%Seal ที่ [S] ติด At +2 
		{mystic_ability,
			s2_no104_a1, 616, 1, y,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {level, "12"}, {type, {n, "Machine"}}],
									immediately, {player_select_exact_target, 1}, [{at, 2}], depend_on_s,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%ผู้เล่นฝ่ายตรงข้าม 1 คน ทิ้ง Mystic Card 1 ใบในมือแบบสุ่มจากนั้นผู้เล่นนั้นจั่ว Mystic Card 1 ใบ
		{mystic_ability,
			s2_no105_a1a, 617, 1, n,
				n, [],
					y, opponent, [hand_cards], null, null, [], 1,
						s2_no105_a1b,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, opponent, [hand_cards], mystic, null, [],
									immediately, {random_select_exact_target, 1}, [{action, discard}], 0
		},
		{mystic_ability,
			s2_no105_a1b, 617, 1, n,
				n, [],
					y, opponent, [mystic_deck], null, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								y, opponent, immediately, {do_not_need_select, 0}, [{draw_mystic, 1}], 0, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], nul,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%ถ้า Seal ที่ [S] ติดคือ [Machine] ที่เป็น Inactive Seal Seal นั้นกลายเป็น Active Seal
		{mystic_ability,
			s2_no106_a1, 618, 1, n,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}],
									immediately, {player_select_exact_target, 1}, [{check_condition, {[{active, inactive}, {type, "Machine"}], {action, be_active}}}], 0,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%เมื่อ Seal ที่ [S] ติดถูกโจมตีใน Subturn นี้ Seal ที่ [S] ติด At +2 Df +2 จนจบ ยกเว้น Machine
		{mystic_ability,
			s2_no107_a1, 619, 1, y,
				n, [],
					y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {type, {n, "Machine"}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, {n, card_casting}}, {type, {n, "Machine"}}],
									immediately, {player_select_exact_target, 1}, [{when_attack_target, {at, 2}}, {when_attack_target, {df, 2}}], end_of_subturn,
								n, null, [], null, null, [],
									null, {}, [], null						
		},
		%Seal ที่กำลังรวมร่างที่ [S] ติดกลายเป็น Inactive Seal
		{mystic_ability,
			s2_no108_a1, 620, 1, y,
				n, [],
					y, null, [arena_zone], seal, null, [{action, being_combine}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, null, [arena_zone], seal, null, [{action, being_combine}],
									immediately, {player_select_exact_target, 1}, [{action, be_inactive}], 0,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%Promo
		%Seal ที่ [S] ติด ติด Freeze Curse จนจบ Subturn ยกเว้น: Seal ฝ่ายตรงข้าม
		{mystic_ability,
			s2_no112_a1, 624, 1, y,
				n, [],
					y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}, {protect_curse, {n, freeze_curse}}, {curse, {n, freeze_curse}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}, {protect_curse, {n, freeze_curse}}, {curse, {n, freeze_curse}}],
									immediately, {player_select_exact_target, 1}, [{curse, freeze_curse}], 1,
								n, null, [], null, null, [],
									null, {}, [], null							
		},
		%Promo
		%Sacrifice Seal 1 ใบ; นำ Seal ที่ไม่ใช่ [Evil] หรือ [Machine] 1 ใบใน Shrine เราขึ้นมือ 
		{mystic_ability,
			s2_no113_a1b, 625, 1, n,
				n, [],
					y, owner, [arena_zone], seal, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, owner, [arena_zone], seal, null, [],
									immediately, {player_select_exact_target, 1}, [{player_action, sacrifice}], 0
		},
		{mystic_ability,
			s2_no113_a1a, 625, 1, n,
				n, [],
					y, owner, [shrine_cards], seal, null, [{type, {n, "Evil"}}, {type, {n, "Machine"}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, owner, [shrine_cards], seal, null, [{type, {n, "Evil"}}, {type, {n, "Machine"}}],
									immediately, {player_select_exact_target, 1}, [{player_action, move_to_hand}], null
		},
		%Promo
		%Sacrifice Seal 1 ใบ; Seal 1 ใบฝ่ายเรา ติด Last Dance Curse At +3 / 3 Turn
		{mystic_ability,
			s2_no114_a1b, 626, 1, n,
				n, [],
					y, owner, [arena_zone], seal, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, owner, [arena_zone], seal, null, [],
									immediately, {player_select_exact_target, 1}, [{player_action, sacrifice}], 0
		},
		{mystic_ability,
			s2_no114_a1a, 626, 1, n,
				n, [],
					y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								y, owner, [arena_zone], seal, null, [{action, {n, card_casting}}],
									immediately, {player_select_exact_target, 1}, [{curse, {last_dance_curse, {at, 3}}}], 6,
								n, null, [], null, null, [],
									null, {}, [], null	
		},
		%Promo
		%Sacrifice Seal 1 ใบ; นำการ์ด 1 ใบใน Shrine ออกจากเกม
		{mystic_ability,
			s2_no114_a2b, 626, 2, n,
				n, [],
					y, owner, [arena_zone], seal, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, owner, [arena_zone], seal, null, [],
									immediately, {player_select_exact_target, 1}, [{player_action, sacrifice}], 0
		},
		{mystic_ability,
			s2_no114_a2a, 626, 2, n,
				n, [],
					y, null, [shrine_cards], null, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, null, [shrine_cards], null, null, [],
									immediately, {player_select_exact_target, 1}, [{player_action, move_to_remove_zone}], 0
		},
		% ขันสงกรานต์
		% Seal ที่ติดป้องกัน Skill 1 Subturn
		% Seal ที่ติดไม่สามารถเปลี่ยน Line ได้ 2 turn
		{mystic_ability,
			s2_no115_a1, 627, 1, y,
				n, [],
					y, null, [arena_zone], seal, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, null, [arena_zone], seal, null, [],
									immediately, {player_select_exact_target, 1}, [{protect_skill, [all]}], 1
		},
		{mystic_ability,
			s2_no115_a2, 627, 2, y,
				n, [],
					y, null, [arena_zone], seal, null, [], 1,
						null,
							null,
								n, null, {}, [], null,
								n, null, null, {}, [], null, 										
								n, null, {}, [], null,
								n, null, [], null, null, [],
									null, {}, [], null,
								y, null, [arena_zone], seal, null, [],
									immediately, {player_select_exact_target, 1}, [{change_line, disallow}], 4
		}
	].
	
	
example() ->
	[
		{mystic_ability,
			m_ability_id, card_id, m_ability_number, can_interfere,
				need_check_self, self_condition_check,
				need_other_check, player_card_check, player_card_zone_check, card_tyep_check, include_this_check, other_condition_check, number_of_require,
						then_do_id,
							continuous_type,
								have_fx_to_owner, fx_owner_got_type, owner_fx_select, fx_to_owner, owner_fx_duration,
								have_fx_to_any_player, player_got_fx, fx_player_got_type, player_fx_select, fx_to_player, player_fx_duration, 										
								have_fx_to_this, fx_this_got_type, this_fx_select, fx_to_this, this_fx_duration,
								have_fx_to_target, target_card_owner, target_card_zone_check, target_card_type, target_include_this, target_condition,
									fx_target_got_type, target_fx_select, fx_target_receive, target_fx_duration,
								have_fx_to_beyond_target, beyond_target_card_owner, beyond_target_card_zone, beyond_target_card_type, beyond_target_include_this, beyond_target_condition,
									fx_beyond_target_got_type, beyond_target_fx_select, fx_beyond_target_receive, beyond_target_fx_duration	
		},
	
		%Seal ทุกใบฝ่ายเราสลับ At กับ Df (ผลที่ทำให้ At เปลี่ยนแปลงไปเปลี่ยนแปลง Df และผลที่ทำให้ Df เปลี่ยนแปลงไปเปลี่ยนแปลง At)
		{mystic_ability,
			s2_no094_a1, 606, 1, n,
				% casting condition
				n, [],
				y, owner, [hand_cards], seal, null, [], 1,
					% after this mystic_ability_id do
					null,
						% ผลของ Ability นี้ส่งผลแบบ Continuous อย่างไร เช่น ส่งผลนาน 1 Turn หรือ ส่งผลเมื่อ Condition ยังคงถูกต้องอยู่
						% ถ้าเป็นแบบ Check Duration ให้ใส่ Duration ถ้าส่งผลเมื่อ condition สมบูรณ์ใส่ y ถ้า ไม่เป็น contionuous ใส่ null
						% Effect to Target
						null,
							n, [], null, % card owner effect
							n, null, [], null,% any player effect 										
							n, [], null, % this card effect
							y, owner, [arena_zone], seal, null, [], % target card effect อาจต้องเลือกเป้า หรือไม่ต้องเลือกขึ้นกับ do_not_need_select, player_select_exact_target
								{do_not_need_select, 0, [{action, {when_attack, {df, power}}}, {action, {when_target, {power, df}}}]}, depend_on_s,
							n, null, [null], null, null, [], % Effect ที่จะเกิดกับการ์ด อืน ที่ไม่ใช่เป้าหมายที่เลือก
								[], null
		}
	].
