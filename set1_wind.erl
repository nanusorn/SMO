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
-module(set1_wind).
-export([wind1_ability/0]).

wind1_ability() ->
[	
	%เมื่อ [S] เข้ามาในสนาม [S] At +1 / 1 Turn
	{card_ability, 
		s1_wind_no61_a1, 317, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						n, null, [null], null, [], null,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}]}, 2,											
											n, null, [null], null, [],													
												null, [], null									
	},
	%[S] [S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Sp น้อยกว่า [S] 
	{card_ability, 
		s1_wind_no61_a2, 317, 2,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
							   n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{attack, {to_df, less, speed}}]}, null,											
											n, null, [null], null, [],													
												null, [], null									
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่อยู่ใน Df Line [S] At +2
	{card_ability, 
		s1_wind_no61_a3, 317, 3,
			n,[],
				n, null, [],									
					y, [{action, fight}],													
						y, null, [arena_zone], n, [{line, 0}, {action, fight}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],													
												null, [], null									
	},
	%เมื่อ [S] โจมตี Seal ที่ถูก [S] โจมตี Sp =0 จนออกจากการโจมตีนั้น
	{card_ability,  
		s1_wind_no63_a1, 319, 1,
			n,[],
				n, null, [],									
					y, [{action, attacker}],													
						y, null, [arena_zone], n, [{action, attacked}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{action, attacked}],													
												y, {do_not_need_select, 0, [{sp, {equal, 0}}]}, end_of_fighting									
	},
	%เมื่อ [S] เข้ามาในสนามนำ Seal ทุกใบในสนามที่มี Sp น้อยกว่า [S] ไปที่ At Line (ผลจาก Ability นี้ไม่นับว่าเป็นการกำหนด Line)
	{card_ability, 
		s1_wind_no63_a2, 319, 2,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						y, null, [arena_zone], n, [{speed, less_than_s}, {can_move_to, 1}, {line, null}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{speed, less_than_s}, {can_move_to, 1}, {line, null}],													
												y, {do_not_need_select, 0, [{action, to_at_line}]}, null									
	},
	%ผู้เล่นทุกคนไม่สามารถกำหนด Line Seal ที่มี Df มากกว่า [S] ให้เปลี่ยน Line ได้ (At Line)
	{card_ability, 
		s1_wind_no65_a1, 321, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],													
						n, null, [null], n, [], null,
							y, [],
				            n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{defend, more_than_s}],
											%y, null, [arena_zone], y, [],
												y, {do_not_need_select, 0, [{disallow, assign_line}]}, null									
	},
	%เมื่อ [S] ตก Shrine จากสนามทำลาย Seal 1 ใบที่มี At น้อยกว่า [S]
	{card_ability, 
		s1_wind_no70_a1, 326, 1,
			n,[],
				n, null, [],									
					y, [{action, arena_to_shrine}, {action, into_shrine}, {stack_check, card_destroy}],													
						y, null, [arena_zone], n, [{card_type, seal}, {attack, less_than_s}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{card_type, seal}, {attack, less_than_s}],
												y, {owner_select_exact_target, 1, [{action, destroy}]}, null										
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่มี At เท่ากับ [S] [S] Sp +1
	{card_ability, 
		s1_wind_no71_a1d, 327, 1,
			n,[],
				n, null, [],									
					y, [{action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {attack, equal_to_s}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{sp, 1}]}, end_of_fighting,											
											n, null, [null], null, [],													
												null, [], null										
	},
	%เมื่อ [S] เข้ามาในสนาม [S] Sp+1 / 1 Turn
	{card_ability, 
		s1_wind_no72_a1, 328, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						n, null, [null], null, [], null,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{sp, 1}]}, 2,											
											n, null, [null], null, [],													
												null, [], null										
	},
	%[S] +Sp ตามจำนวน Mystic Card ในมือผู้ควบคุม
	{card_ability, 
		s1_wind_no73_a1, 329, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{sp, {'+', {{target, controller, [hand_cards], mystic, null}, []}}}]}, null,											
											n, null, [null], null, [],													
												null, [], null										
	},
	%ตราบเท่าที่ [S] รวมร่าง Seal ทุกใบในสนามฝ่ายเรา Sp = 4
	{card_ability, 
		s1_wind_no74_a1, 330, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, owner, [arena_zone], y, [{card_type, seal}],													
												y, {do_not_need_select, 0, [{sp, {equal, 4}}]}, null										
	},
	%ตราบเท่าที่เรามี [S] อยู่ในสนามตั้งแต่ 2 ใบขึ้นไป ผู้เล่นฝ่ายตรงข้ามทุกคนการ์ดในมือสูงสุด -1 (ถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนามฝ่ายเรา เลือก Ability นี้ทำงานได้เพียง 1 ใบเท่านั้น)
	{card_ability, 
		s1_wind_no75_a1, 331, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									y, y, opponent, {do_not_need_select, 0, [{check, {player_target_effect, s1_wind_no75_a1, {less_than, 1}, {card_on_hand, -1}}}]},	null,												
					               n, null, [], null,											
											n, null, [null], null, [],													
												null, [], null										
	},
	%ตราบเท่าที่ [S] มี [Dragon] เป็น Seal ใบรองรวมร่าง [S] สามารถโจมตีข้ามไปยัง Df Line ได้
	{card_ability, 
		s1_wind_no111_a1, 367, 1,
			n, [],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],											
							n, null, [null], null, [], null,
								y, [],
									n, null, [], null,
										n, null, null, [], null,
											y, y, {do_not_need_select, 0, [{attack, to_df}]}, null,											
												n, null, [null], null, [],
													null, [] ,null
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่อยู่ใน Df Line [S] At +2 Df +2
	{card_ability, 
		s1_wind_no111_a2, 367, 2,
			n,[],
				n, null, [],									
					y, [{action, fight}],													
						y, null, [arena_zone], n, [{line, 0}, {action, fight}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 2}, {df, 2}]}, end_of_fighting,											
											n, null, [null], null, [],													
												null, [], null									
	},
	%N[Charles] ทุกใบในสนาม Sp +1
	{card_ability, 
		p1_wind_no118_a1, 374, 1,
			n, [],
				n, null, [],									
					y, [{action, on_arena}],
						n, null, [], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,										
										n, null, [], null,
											y, null, [arena_zone], y, [{card_type, seal}, {naming, "Charles"}],
												y, {do_not_need_select, 0, [{sp, 1}]}, null
	},	
	%เมื่อ [S] เข้ามาในสนาม N[Charles] 1 ใบในสนาม At +1 / 1 Turn
	{card_ability, 
		p1_wind_no118_a2, 374, 2,
			n, [],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],
						y, null, [arena_zone], y, [{card_type, seal}, {naming, "Charles"}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [], null,										
										n, null, [], null,
											y, null, [arena_zone], y, [{card_type, seal}, {naming, "Charles"}],
												y, {owner_select_exact_target, 1, [{at, 1}]}, 2
	}
].