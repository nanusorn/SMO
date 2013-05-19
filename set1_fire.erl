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
-module(set1_fire).
-export([fire1_ability/0]).

fire1_ability() ->
[
	%เมื่อ [S] โจมตี [S] + At ตามจำนวน Seal ใน At Line ฝ่ายเราจนจบ Subturn
	{card_ability, 
		s1_fire_no032_a1, 288, 1,
			n,[],
				n, null, [],									
					y, [{action, attacker}],											
						n, null, [null], null, [], null,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,
										y, y, {do_not_need_select, 0, [{at, {'+', {{target, owner, [arena_zone], seal, y}, [{line, 1}]}}}]}, end_of_subturn,											
											n, null, [null], null, [],
												null, [] ,null
	},
	%Seal ที่มี Mp ค่าร่ายน้อยกว่า [S] ทุกใบในสนามฝ่ายตรงข้าม At –1 Df –1
	{card_ability, 
		s1_fire_no033_a2, 289, 2,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [],	null,
										n, null, [], null,											
											y, opponent, [arena_zone], null, [{card_type, seal}, {mpcast, less_than_s}],
												y, {do_not_need_select, 0, [{at, -1}, {df, -1}]}, null					
	},
	%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Sp มากกว่า [S]
	{card_ability, 
		s1_fire_no033_a1, 289, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{attack, {to_df, more, speed}}]}, null,			
											n, null, [null], null, [],
												null, [] ,null
	},
	%เมื่อ [S] โจมตี Seal ที่มี Sp มากกว่า [S] ผู้ควบคุม [S] สามารถเลือกให้ [S] เทียบค่าพลังของ [S] กับ At หรือ Df ของ Seal ที่ถูก [S] โจมตี
	{card_ability,
		s1_fire_no034_a1, 290, 1,
			n,[],
				n, null, [],									
					y, [{action, attacker}],											
						y, null, [arena_zone], n, [{speed, more_than_s}, {action, attacked}], 1,
							n, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {controller_can_activate_ability, 0, [{action, can_compare_to_at_or_df_of_attacked}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%เมื่อ [S] โจมตี ผู้ควบคุม [S] สามารถเลือกให้ [S] เทียบค่าพลังของ [S] กับ At หรือ Df ของ Seal ที่ถูก [S]โจมตี 
	{card_ability, 
		s1_fire_no036_a1, 292, 1,
			n, [],
				n, null, [],									
					y, [{action, attacker}], 											
						n, null, [null], null, [], null,
							n, [],
								n, null, [], null,
									n, null, null, [], null,										
										y, y, {controller_can_activate_ability, 0, [{action, can_compare_to_at_or_df_of_attacked}]}, null, 
											n, null, [null], null, [],
												n, [], null
	},
	%เมื่อ [S] โจมตี [S] At +2 จนจบ Subturn
	{card_ability, 
		s1_fire_no037_a1, 293, 1,
			n,[],
				n, null, [],									
					y, [{action, attacker}],											
						n, null, [null], null, [], null,
							c, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_subturn,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%ตราบเท่าที่ [S] อยู่ในมือ [S] -Mp ค่าร่ายตามจำนวน [Evil] ใน Shrine เรา
	{card_ability, 
		s1_fire_no038_a1, 294, 1,
			n,[],
				n, null, [],									
					y, [{zone, [hand_cards]}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{mc, {'-', {{target, owner, [shrine_cards], seal, null}, [{type, "Evil"}]}}}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%เมื่อ [Fire] ฝ่ายเราตก Shrine จากสนาม [S] At +2 จนจบ Subturn(Ability นี้ทำงาน 1 ครั้งใน 1 Turn)
	{card_ability,
		s1_fire_no040_a1, 296, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {activate_ability, {1, 1}}], % 1 ครั้ง ใน 2 Subturn
						%y, owner, [shrine_cards], n, [{action, into_shrine}, {action, arena_to_shrine}, {elem, 4}], 1,
						y, owner, [shrine_cards], n, [{action, into_shrine}, {action, arena_to_shrine}, {elem, 4}, {stack_check, card_destroy}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_subturn,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%ตราบเท่าที่ Seal ในสนามฝ่ายตรงข้ามมากกว่าเรา [S] At +3 
	{card_ability, 
		s1_fire_no042_a1, 298, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{at, 3}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%ตราบเท่าที่ Seal ในสนามฝ่ายตรงข้ามน้อยกว่าเรา [S] At -3 
	{card_ability, 
		s1_fire_no042_a2, 298, 2,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{at, -3}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%[S] สามารถโจมตีจาก Df Line ได้
	{card_ability, 
		s1_fire_no044_a1, 300, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{attack, from_df}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%ตราบเท่าที่ [S] รวมร่าง [S] สามารถโจมตีข้ามไปยัง Df Line ได้ (At Line)
	{card_ability, 
		s1_fire_no044_a2, 300, 2,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										y, y, {do_not_need_select, 0, [{attack, to_df}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%[S] +At ตามจำนวน [S] ใบอื่นในสนาม
	{card_ability, 
		s1_fire_no045_a1, 301, 1,
			n,[],
				n, null, [],									
					y, [{action, on_arena}],											
						n, null, [null], null, [], null,
							y, [],
								n, null, [], null,
									n, null, null, [], null,
										%y, y, {do_not_need_select, 0, [{at, {'+', same_name_as_s_arena}}]}, null,
										y, y, {do_not_need_select, 0, [{at, {'+', {{target, null, [arena_zone], seal, n}, [{name, "Souless"}]}}}]}, null,										
											n, null, [null], null, [],
												null, [] ,null
	},
	%เมื่อ [S] โจมตี เราสามารถ Sacrifice Seal 1 ใบ และ Seal 1 ใบในสนามที่มี Sp มากกว่า [S] แยกการรวมร่าง
	{card_ability, 
		p1_fire_no114_a1a, 370, 1,
			n, [],
				n, null, [],									
					y, [{zone,[arena_zone]}, {action, attacker}, {{null, [arena_zone], seal, n}, {[{combine, {card_type, is_seal}}, {speed, more_than_s}], 1}}],											
						y, controller, [arena_zone], n, [{card_type, seal}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, owner, [arena_zone], n, [{card_type, seal}],
												y, {owner_can_select_exact_target, 1, [{player_action, sacrifice}]}, null
	},
	{card_ability, 
		p1_fire_no114_a1b, 370, 1,
			n, [],
				n, null, [],									
					y, [{zone,[arena_zone]}, {action, attacker}, {{null, [arena_zone], seal, n}, {[{combine, {card_type, is_seal}}, {speed, more_than_s}], 1}}], 											
						y, controller, [arena_zone], n, [{card_type, seal}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, null, [arena_zone], n, [{card_type, seal}, {combine, {card_type, is_seal}}, {speed, more_than_s}],
												y, {owner_select_exact_target, 1,[{action, break_combine}]}, null
	},
	%เมื่อ [S] เข้ามาในสนาม [Fire] ใบอื่น 1 ใบในสนามฝ่ายเรา At +2 จนจบ Subturn
	{card_ability, 
		s1_fire_no120_a1, 376, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],													
						%y, owner, [arena_zone], n, [{elem, 4}], 1,
						y, owner, [arena_zone], n, [{elem, 4}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, n, {}, null,											
											%y, owner, [arena_zone], n, [{elem, 4}],	
											y, owner, [arena_zone], n, [{elem, 4}],
												y, {owner_select_exact_target, 1, [{at, 2}]} , end_of_subturn
	},
	%[S] ยกเลิก Skill ของ [Wind] และ Death Curse
	{card_ability, 
		s1_fire_no120_a2, 376, 2,
				n,[],
					n, null, [],									
						y, [{zone,[arena_zone]}, {action, on_arena}],					
							n, null, [null], null, [], null,
								y, [],
									n, null, [], null,                        
										n, null, null, [], null,												
											%y, y, {do_not_need_select, 0, [{cancel_skill, [{elem, 3}]}, {cancel_curse, [death_curse]}]}, null,	
											y, y, {do_not_need_select, 0, [{cancel_skill, [{elem, 3}]}, {cancel_curse, [death_curse]}]}, null,	
												n, null, [null], null, [],
													null, [] ,null					
	}
].

