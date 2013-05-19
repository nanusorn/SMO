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
-module(set2_water).
-export([water2_ability/0]).

water2_ability() ->
[
	%ขณะที่ [S] ต่อสู้กับ [Earth] [S] At –3
	{card_ability, 
		s2_water_no47_a1, 559, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {elem, 1}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -3}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%เมื่อ [S] โจมตีสำเร็จ Seal ที่มี Df น้อยกว่า [S] ติด Freeze Curse 1  Turn
	{card_ability, 
		s2_water_no48_a1, 560, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, attack_success}],													
						y, null, [arena_zone], n, [{defend, less_than_s}, {protect_curse, {n, freeze_curse}}, {curse, {n, freeze_curse}}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{defend, less_than_s}, {protect_curse, {n, freeze_curse}}, {curse, {n, freeze_curse}}],
												y, {do_not_need_select, 0, [{curse, freeze_curse}]}, 2									 
	},
	%ขณะที่ [S] ต่อสู้กับ [Fire] [S] At +2 
	{card_ability, 
		s2_water_no48_a2, 560, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {elem, 4}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%Charging 2
	{card_ability, 
		s2_water_no49_a1, 561, 1,
			y,[{turn, at}, {pharse, checkup}],
				n, null, [],									
					y, [{zone, [arena_zone]}, {counter, {less_than, 2}}],													
						n, null, [null], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{counter, 1}]}, null,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%Charging 2
	{card_ability, 
		s2_water_no51_a1, 563, 1,
			y, [{turn, at}, {pharse, checkup}],
				n, null, [],									
					y, [{zone, [arena_zone]}, {counter, {less_than, 2}}],													
						n, null, [null], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{counter, 1}]}, null,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%ผู้เล่นไม่สามารถสั่ง Seal ที่ไม่ได้รวมร่างโจมตี [S] ได้
	{card_ability, 
		s2_water_no52_a1, 564, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], 1,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{assign_attack_to_s, {disallow, seal_not_combine}}]}, null,											
											n, null, [null], null, [],
												n, [], null									 
	},
	%ขณะที่ [S] ต่อสู้กับ [Earth] [S] At -2
	{card_ability, 
		s2_water_no52_a2, 564, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],
						y, null, [arena_zone], n, [{action, fight}, {elem, 1}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,
											n, null, [null], null, [],
												null, [], null									 
	},
	%ขณะที่ [S] ต่อสู้กับ [Earth] [S] At -2
	{card_ability, 
		s2_water_no53_a1, 565, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {elem, 1}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%[Dragon] ธาตุ [Water] ทุกใบในสนาม Mp ค่าใช้ Skill -1(ถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนามเลือกให้ Ability นี้ทำงานได้เพียง 1 ใบเท่านั้น)
	{card_ability, 
		s2_water_no54_a1, 566, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], null, [{type,"Dragon"}, {elem, 2}],
												y, {do_not_need_select, 0, [{check, {card_target_effect, s2_water_no54_a1, {less_than, 1}, {ms, -1}}}]}, null									 
	},
	%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
	{card_ability, 
		s2_water_no54_a2, 566, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [], null,												
					               y, y, {do_not_need_select, 0, [{assign_atk, disallow}]}, null,
											n, null, [null], null, [],
												null, [] ,null									
	},
	%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Df น้อยกว่า [S]
	{card_ability, 
		s2_water_no55_a1, 567, 1,			
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{attack, {to_df, less, defend}}]}, null,											
											n, null, [null], null, [],													
												null, [], null									
	},
	%เมื่อ [S] โจมตี Seal ที่อยู่ใน Df Line [S] At -1 จนจบ Subturn
	{card_ability, 
		s2_water_no55_a2, 567, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, attacker}],													
						y, null, [arena_zone], n, [{action, attacked}, {line, 0}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -1}]}, end_of_subturn,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%Seal ทุกใบที่อยู่ใน Df Line ป้องกันการโจมตี (All) และ Skill (All) (At Line)
	{card_ability, 
		s2_water_no56_a1, 568, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{card_type, seal}, {line, 0}],
												y, {do_not_need_select, 0, [{protect_attack, [attack_all]}, {protect_skill, [skill_all]}]}, null									 
	},
	%ขณะที่ [S] ต่อสู้กับ [Fire] [S] At +1
	{card_ability, 
		s2_water_no57_a1, 569, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {elem, 4}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%ขณะที่ [S] ต่อสู้กับ [Tamer] [S] At -2
	{card_ability, 
		s2_water_no57_a2, 569, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {type, "Tamer"}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่ติด Curse [S] At+1
	{card_ability, 
		s2_water_no60_a1, 572, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {curse, y}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%ขณะที่ [S] ต่อสู้กับ [Insect] [S] At +2
	{card_ability, 
		s2_water_no61_a1, 573, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {type, "Insect"}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									 
	},
	%[S] ยกเลิก Skill ของ [Insect]
	{card_ability, 
		s2_water_no61_a2, 573, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_skill, [{type, "Insect"}]}]}, null,											
											n, null, [null], null, [],
												null, [], null										 
	}	
].