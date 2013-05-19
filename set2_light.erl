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
-module(set2_light).
-export([light2_ability/0]).

light2_ability() -> 
[
	%[S] +At ตามจำนวน Seal ในสนามฝ่ายตรงข้าม
	{card_ability, 
		s2_light_no001_a1, 513, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, {'+', {{target, opponent, [arena_zone], seal, null}, []}}}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%ขณะที่ [S] ต่อสู้กับ [Knight] [S] At -2
	{card_ability, 
		s2_light_no001_a2, 513, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {type, "Knight"}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%[S] ยกเลิก Curse 
	{card_ability, 
		s2_light_no002_a1, 514, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%เมื่อ [S] เข้ามาในสนาม [S] ป้องกันการโจมตีและ Curse 1 Turn (Ability นี้จะไม่ทำงานถ้ามี Seal ตั้งแต่ 4 ใบขึ้นไปในสนามฝ่ายเรา)
	{card_ability, 
		s2_light_no003_a1, 515, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						y, owner, [arena_zone], y, [{card_type, seal}], {less_than, 4},
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{protect_attack, [all]}, {protect_curse, [all]}]}, 2,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%[S] +At ตาม N[Penguin] ใบอื่นในสนาม
	{card_ability, 
		s2_light_no003_a2, 515, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, {'+', {{target, null, [arena_zone], seal, n}, [{naming, "Penguin"}]}}}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%[S] [S] ยกเลิก Curse และ Mystic Card
	{card_ability, 
		s2_light_no004_a1, 516, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_mystic, [all]}, {cancel_curse, [all]}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%[S] -Mp ค่าโจมตีตามจำนวน Mystic Card ในมือฝ่ายตรงข้าม
	{card_ability, 
		s2_light_no005_a1, 517, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{ma, {'-', {{target, opponent, [hand_cards], mystic, null}, []}}}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%ตราบเท่าที่ฝ่ายตรงข้ามมีการ์ดในมือมากกว่าเรา [S] At +1 
	{card_ability, 
		s2_light_no005_a2, 517, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}]}, null,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%เมื่อ [S] โจมตี +At ตามจำนวน Mystic Card ในมือผู้ควบคุมจนจบ Subturn
	{card_ability, 
		s2_light_no005_a3, 517, 3,
			n,[],
				n, null, [],									
					y, [{action, attacker}, {growth, y}],													
						n, null, [], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, {'+', {{target, controller, [hand_cards], mystic, null}, []}}}]}, end_of_subturn,											
											n, null, [null], null, [],
												null, [] ,null									
	},
	%Seal ที่มี Sp น้อยกว่า [S] ไม่สามารถโจมตี [S] ได้
	{card_ability, 
		s2_light_no006_a1, 518, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], n, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{attack_to_s, {disallow, sp_less_than_s}}]}, null,											
											n, null, [null], null, [],
												n, [] ,null									
	},
	%ขณะที่ [S] ต่อสู้กับ [Monster] [S] At -2
	{card_ability, 
		s2_light_no007_a1, 519, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {type, "Monster"}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									
	},
	%เมื่อ [S] เข้ามาในสนาม เราสามารถนำ Mystic Card ชนิด [PS] 1 ใบในมือเรา มาติดที่ [S]
	{card_ability, 
		s2_light_no008_a1, 520, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						y, owner, [hand_cards], null, [{paste_type, 1}, {can_paste_to_seal, self}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, owner, [hand_cards], null, [{paste_type, 1}, {can_paste_to_seal, self}],
												y, {owner_can_select_exact_target, 1, [{action, attach_to_s}]}, null									
	},
	%[S] +At ตามจำนวน Seal ที่รวมร่างในสนามฝ่ายตรงข้าม
	{card_ability, 
		s2_light_no009_a1, 521, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, {'+', {{target, opponent, [arena_zone], seal, null}, [{combine, y}]}}}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%ขณะที่ [S] ต่อสู้กับ [Dark] [S] At +1 Df +2
	{card_ability, 
		s2_light_no011_a1, 523, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {elem, 5}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}, {df, 2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									
	},
	%ขณะที่ [S] ต่อสู้กับ [Divine] [S] At -2
	{card_ability, 
		s2_light_no011_a2, 523, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {type, "Divine"}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
											n, null, [null], null, [],
												null, [], null									
	},
	%[S] ยกเลิก Curse และ Mystic Card
	{card_ability, 
		s2_light_no012_a1, 524, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_curse, [all]}, {cancel_mystic, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
	{card_ability, 
		s2_light_no012_a2, 524, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [], null,												
					               y, y, {do_not_need_select, 0, [{assign_atk, disallow}]}, null,
											n, null, [null], null, [],
												null, [], null									
	},
	%[S] ยกเลิก Steal
	{card_ability, 
		s2_light_no013_a1, 525, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{cancel_steal, [all]}]}, null,											
											n, null, [null], null, [],
												null, [], null									
	},
	%Growth: [Dragon’s Egg] หรือ [Light] [S] ยกเลิก Curse
	{card_ability, 
		s2_light_no013_a2, 525, 2,
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
	%เมื่อ [S] ติด Mystic Card ชนิด [PS] ฝ่ายตรงข้าม ผู้ควบคุม [S] Mp +1
	{card_ability, 
		s2_light_no014_a1, 526, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, paste_mystic_success}, {mystic_recent_paste, {owner, opponent}}],													
						n, null, [], null, [], null,
							n, [],
			               n, null, [], null,
									y, y, controller, {do_not_need_select, 0, [{mp, 1}]}, null,												
					               n, null, [], null,
											n, null, [null], null, [],
												null, [], null									
	},
	%ตราบเท่าที่ [S] รวมร่างกับ [Water] เมื่อ [S] ถูกโจมตีสำเร็จ Seal ที่โจมตี [S] ติด Charm Curse 2 Turn เมื่อออกจากการโจมตีนั้น
	{card_ability, 
		s2_light_no015_a1, 527, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, attacked}, {combine, {elem, 2}}],													
						y, null, [arena_zone], n, [{action, attack_success}, {protect_curse, {n, charm_curse}}, {curse, {n, charm_curse}}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{action, attack_success}, {protect_curse, {n, charm_curse}}, {curse, {n, charm_curse}}],
												y, {do_not_need_select, 0, [{curse, charm_curse}]}, 4								
	}
].