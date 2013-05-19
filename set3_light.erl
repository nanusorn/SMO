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
-module(set3_light).
-export([light3_ability/0]).

light3_ability() -> 
[
	%*1 Growth: Pony Unicorn > [S] ยกเลิก Curse
	{card_ability, 
		s3_light_no001_a1, 769, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}, {growth, y}],													
						n, null, [], null, [], null,
							y, [s_growth],
								n, null, [], null,
									n, null, null, [],	null,												
										y, y, {do_not_need_select, 0, [{at, 1}, {df, 1}, {sp, 1}, {cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%*3 เมื่อ [S] เข้ามาในสนาม เราดู Mystic Card ทุกใบในมือฝ่ายตรงข้าม 
	{card_ability, 
		s3_light_no003_a1, 771, 1,
			n, [],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],
						n, null, [], null, [], null,
							n, [],
								y, y,{do_not_need_select, 0, [{player_action, {show_opponent_card, mystic}}]}, null,                        
										n, null, null, [], null,												
											n, null,[], null,											
												n, null, [null], null, [],
													null, [] ,null	
	},
	%*3 [S] ยกเลิก Curse
	{card_ability, 
		s3_light_no003_a2, 771, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [],	null,												
										y, y, {do_not_need_select, 0, [{cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%*4 [S] ยกเลิก Curse
	{card_ability, 
		s3_light_no004_a1, 772, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [],	null,												
										y, y, {do_not_need_select, 0, [{cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%*5 [S] +At ตามจำนวน Charge Counter ของ [S]
	{card_ability, 
		s3_light_no005_a1, 773, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [],	null,												
										y, y, {do_not_need_select, 0, [{at, {'+', s_charge_counter}}]}, null,	
											n, null, [null], null, [],
												null, [] ,null									
	},
	%*5 เมื่อ [S] จบการต่อสู้ นำ Charge Counter ทั้งหมดออกจาก [S]
	{card_ability, 
		s3_light_no005_a2, 773, 2,
			n,[],
				n, null, [],									
					y, [{action, end_of_fighting}],											
						n, null, [null], null, [], null,
							n, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{remove_counter, all}]}, null,
											n, null, [null], null, [],
												null, [] ,null
	},
	%{card_ability, 
	%	s3_light_no005_a3, 773, 3,
	%		n,[],
	%			n, null, [],									
	%				y, [{zone, [arena_zone]}],											
	%					y, owner, [arena_zone], n, [{action, cast_success}], 1,
	%						n, [],
	%							n, null, [], null,
	%								n, null, null, [], null,
	%									y, y, {do_not_need_select, 0, [{counter, 1}]}, null,
	%										n, null, [null], null, [],
	%											null, [] ,null
	%},
	%*7 [S] -Mp ตามจำนวน Damica, the Black Wood Tamer ในสนาม
	{card_ability, 
		s3_light_no007_a1, 775, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{mc, {'-', {seal_card, name, ["Damica, the Black Wood Tamer"], zone, arena_zone, owner, all}}},{ma, {'-', {seal_card, name, ["Damica, the Black Wood Tamer"], zone, arena_zone, owner, all}}},{ms, {'-', {seal_card, name, ["Damica, the Black Wood Tamer"], zone, arena_zone, owner, all}}}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%*11 ตราบเท่าที่มี [Gri] ใบอื่นในสนาม [S] At +2
	{card_ability, 
		s3_light_no011_a1, 779, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,												
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{at, 2}]}, null,											
											n, null, [null], null, [],
												null, [] ,null					
	},
	%*11 ผู้เล่นไม่สามารถสั่ง [Gri] โจมตี [S] ได้
	{card_ability, 
		s3_light_no011_a2, 779, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,												
										y, y, {do_not_need_select, 0, [{assign_attack_to_s, {disallow, {naming, ["Griffin"]}}}]}, null,
											n, null, [null], null, [],
												null, [] ,null									
	},
	%*12 Growth : Pony Unicorn > [S] ยกเลิก Curse
	{card_ability,
		s3_light_no012_a1, 780, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}, {growth, y}],													
						n, null, [], null, [], null,
							y, [s_growth],
								n, null, [], null,
									n, null, null, [],	null,												
										y, y, {do_not_need_select, 0, [{cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%*13 เมื่อ [S] เข้ามาในสนาม Seal 1 ใบในสนามป้องกัน Curse 1 Turn
	{card_ability, 
		s3_light_no013_a1, 781, 1,
			n, [],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],
						n, null, [], null, [], null,
							n, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, null, [arena_zone], y, [{card_type, seal}],
												y, {owner_select_exact_target, 1, [{protect_curse, [all]}]}, 2
	},
	%*15 เมื่อ [S] โจมตีขึ้นมือฝ่ายตรงข้ามสำเร็จ เราสามารถร่าย Seal 1 ใบจากกองการ์ด Seal เราโดยไม่ต้องจ่าย Mp ค่าร่าย
	%{card_ability, 
	%	s3_light_no015_a1, 783, 1,
	%		n,[],
	%			n, null, [],									
	%				y, [{zone, [arena_zone]}, {action, hand_attack_success}],
	%					y, owner, [seal_deck], null, [], 1,
	%						n, [],
	%		               n, null, [], null,
	%								n, null, null, [], null,												
	%				               n, n, [], null,											
	%										y, owner, [seal_deck], null, [],
	%											y, {owner_can_select_exact_target, 1, [{action, cast_without_paying_cost}]}, null									 
	%},
	%16 เมื่อ [S] เข้ามาในสนามจากการร่าย เราสามารถแสดง [Mo] ธาตุ [Li] 2 ใบในมือเรา จากนั้น Seal 1 ใบในสนามติด Dimension Curse 1 Turn
	{card_ability, 
		s3_light_no016_a1a, 784, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {action, card_casting}, {stack_check, self}],													
						n, null, [null], null, [], null,
							n, [],
							%{then_do, [s3_light_no016_a2a]}, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {owner_can_activate_ability, 0, []}, null,										
											n, null, [null], null, [],
												null, [] ,null							 
	},
	{card_ability, 
		s3_light_no016_a1b, 784, 1,
			n,[],
				n, null, [],
					y, [{action, into_arena}, {action, card_casting}, {{owner, [hand_cards], seal, n}, {[{type, "Monster"}, {elem, 6}], 2}}, {stack_check, self}],													
						y, null, [arena_zone], y, [{protect_curse, {n, dimension_curse}}, {curse, {n, dimension_curse}}], 1,
							%{then_do, s3_light_no016_a1c}, [],
							n, [],
								n, null, [], null,
									n, null, null, [],	null,												
										n, n, [], null,											
											y, owner, [hand_cards], null, [{card_type, seal}, {type, "Monster"}, {elem, 6}],
												%y, {owner_select_exact_target, 2, [{action, {show, null, [{card_type, seal}, {type, "Monster"}, {elem, 6}], null, null}}]}, null
												y, {owner_select_exact_target, 2, [{action, show}]}, null
	},
	{card_ability, 
		s3_light_no016_a1c, 784, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {action, card_casting}, {{owner, [hand_cards], seal, n}, {[{type, "Monster"}, {elem, 6}], 2}}, {stack_check, self}],													
						y, null, [arena_zone], y, [{protect_curse, {n, dimension_curse}}, {curse, {n, dimension_curse}}], 1,
							n, [],
								n, null, [], null,
									n, null, null, [],	null,												
										n, n, [], null,											
											y, null, [arena_zone], y, [{protect_curse, {n, dimension_curse}}, {curse, {n, dimension_curse}}],
												y, {owner_select_exact_target, 1, [{curse, dimension_curse}]}, 2
	},

	%*17 เมื่อ [S] เข้ามาในสนาม Seal ที่ชื่อ [S] ทุกใบในสนามยกเลิก Curse 1 Turn
	{card_ability, 
		s3_light_no017_a1, 785, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],											
						n, null, [null], null, [], null,
							n, [],
								n, null, [], null,
									n, null, null, [],	null,
										n, null, [], null,											
											y, null, [arena_zone], y, [{card_type, seal}, {name, "Ratatosk"}],
												y, {do_not_need_select, 0, [{cancel_curse,[all]}]}, 2					
	},
	%17 เมื่อ [S] เข้ามาในสนามจากกองการ์ด Seal ทุกใบในสนามยกเลิก Curse 1 Turn
	{card_ability, 
		s3_light_no017_a2, 785, 2,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {action, deck_to_arena}, {stack_check, self}],											
						n, null, [null], null, [], null,
							n, [],
								n, null, [], null,
									n, null, null, [],	null,
										n, null, [], null,											
											y, null, [arena_zone], y, [{card_type, seal}],
												y, {do_not_need_select, 0, [{cancel_curse,[all]}]}, 2					
	}

].