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
-module(set1_water).
-export([water1_ability/0]).

water1_ability() ->
[
	%เมื่อ [S] เข้ามาในสนามนำ Seal ทุกใบในสนามที่มี Sp น้อยกว่า [S] ไปที่ Df Line (ผลจาก Ability นี้ไม่นับว่าเป็นการกำหนด Line)
	{card_ability, 
		s1_water_no46_a1, 302, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						y, null, [arena_zone], n, [{line, 1}, {speed, less_than_s}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{line, 1}, {speed, less_than_s}],													
												y, {do_not_need_select, 0, [{action, to_df_line}]}, null									
	},
	%ขณะที่ [S] ต่อสู้ เทียบค่าพลังของ [S] กับ Df ของ Seal ที่ต่อสู้กับ [S]
	{card_ability,
		s1_water_no47_a1, 303, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						n, null, [null], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{combat, {when_fight_attack, {power, df}}}, {combat, {when_fight_target, {df, power}}}]}, end_of_fighting,											
											n, null, [], null, [],													
												null, [] ,null									
	},
	%[Water] ใบอื่นทุกใบในสนามฝ่ายเรา At +
	{card_ability, 
		s1_water_no49_a1, 305, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, owner, [arena_zone], n, [{elem, 2}],													
												y, {do_not_need_select, 0, [{at, 1}]} , null
	},
	%Seal ทุกใบฝ่ายตรงข้ามที่มี Df น้อยกว่า [S] At -1
	{card_ability, 
		s1_water_no50_a1, 306, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, opponent, [arena_zone], n, [{defend, less_than_s}],													
												y, {do_not_need_select, 0, [{at, -1}]}, null						
	},
	%เมื่อ [S] ถูกโจมตีด้วย Seal ที่มี Df น้อยกว่า [S] [S] At +2 จนออกจากการโจมตีนั้น
	{card_ability,
		s1_water_no51_a1, 307, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, attacked}],													
						y, opponent, [arena_zone], null, [{action, attacker}, {defend, less_than_s}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,											
										y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null													
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่ติด Curse [S] At +2
	{card_ability, 
		s1_water_no51_a2, 307, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {curse, y}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,											
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null													
	},
	%เมื่อ [S] ตก Shrine จากสนาม เรา Mp +1
	{card_ability,
		s1_water_no53_a1, 309, 1,
			n,[],
				n, null, [],									
					y, [{action, arena_to_shrine}, {action, into_shrine}, {stack_check, card_destroy}],													
						n, null, [], null, [], null,
							n, [],
			               y, y, {do_not_need_select, 0, [{mp, 1}]}, null,
									n, null, null, [],	null,											
					               n, null, [], null,											
											n, null, [null], null, [],
												null, [] ,null													
	},
	%เมื่อ [S] เข้ามาในสนาม เราสามารถแสดง [Insect] ที่ไม่ใช่ชื่อ [S] 1 ใบจากกองการ์ด Seal เราแล้วนำขึ้นมือ
	{card_ability,
		s1_water_no56_a1, 312, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						y, owner, [seal_deck], n, [{type, "Insect"}, {name, {n, "Aqualo"}}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,											
					               n, null, [], null,											
											y, owner, [seal_deck], n, [{type, "Insect"}, {name, {n, "Aqualo"}}],
												y, {owner_can_select_exact_target, 1, [{action, move_to_hand}]} ,null													
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่ติด Curse  [S] At +2 
	{card_ability, 
		s1_water_no58_a1, 314, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {curse, y}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,											
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null													
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่ติด Freeze Curse [S] At +2
	{card_ability, 
		s1_water_no59_a1, 315, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {curse, freeze_curse}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,											
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null													
	},
	%[S] ยกเลิก Freeze Curse
	{card_ability, 
		s1_water_no59_a2, 315, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_curse, [freeze_curse]}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%ตราบเท่าที่มี Seal ติด Freeze Curse [S] At +1 Df -1
	{card_ability, 
		p1_water_no116_a1, 372, 1,
			n, [],
				n, null, [],									
					y, [{action, on_arena}],
						n, null, [nul], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [],	null,										
										y, y, {do_not_need_select, 0, [{at, 1}, {df, -1}]}, null, 
											n, null, [null], null, [],
												n, [], null
	},
	%เมื่อ [S] เข้ามาในสนาม Seal 1 ใบในสนามที่มี Df น้อยกว่า [S] ติด Freeze Curse จนจบ Subturn
	{card_ability, 
		p1_water_no117_a1, 373, 1,
			n, [],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],
						y, null, [arena_zone], y, [{defend, less_than_s}, {curse, {n, freeze_curse}}, {protect_curse, {n, freeze_curse}}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, null, [arena_zone], y, [{defend, less_than_s}, {curse, {n, freeze_curse}}, {protect_curse, {n, freeze_curse}}],
												y, {owner_select_exact_target, 1, [{curse, freeze_curse}]}, 1
	},
	%เมื่อเข้าสู่ Subturn ป้องกันของเรา [Water] ใบอื่นทุกใบฝ่ายเรา Df +1 จนจบ Subturn
	{card_ability, 
		p1_water_no117_a2, 373, 2,
			y, [{turn, df}, {pharse, checkup}],
				n, null, [],									
					y, [{zone, [arena_zone]}],
						y, null, [arena_zone], n, [{elem, 2}], 1,
							n, [],
								n, null, [], null,
									n, null, null, [], null,										
										n, null, [], null,
											y, owner, [arena_zone], n, [{elem, 2}],
												y, {do_not_need_select, 0, [{df, 1}]}, 1
	}
].