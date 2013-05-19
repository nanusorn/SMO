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
-module(set1_dark).
-export([dark1_ability/0]).

dark1_ability() -> 
[
	%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Sp มากกว่า [S] 
	{card_ability, 
		s1_dark_no76_a1, 332, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{attack, {to_df, more, speed}}]}, null,											
											n, null, [null], null, [],													
												null, [], null
	},
	%ขณะที่ [S] ต่อสู้กับ Seal ที่อยู่ใน Df Line [S] At +2
	{card_ability, 
		s1_dark_no76_a2, 332, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						y, null, [arena_zone], n, [{action, fight}, {line, 0}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 2}]}, end_of_fighting,											
											n, null, [null], null, [],													
												null, [], null										
	},
	% charging2
	{card_ability, 
		s1_dark_no77_a1, 333, 1,
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
	%เมื่อ [S] ตก Shrine จากสนาม เราสามารถนำ Mystic Card 1 ใบใน Shrine เราขึ้นมือ 
	{card_ability, 
		s1_dark_no80_a1, 336, 1,
			n,[],
				n, null, [],									
					y, [{zone, [shrine_cards]}, {action, arena_to_shrine}, {action, into_shrine}, {stack_check, card_destroy}],													
						y, owner, [shrine_cards], null, [{card_type, mystic}], 1,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, owner, [shrine_cards], null, [{card_type, mystic}],
												y, {owner_can_select_exact_target, 1, [{action, move_to_hand}]} ,null										
	},
	%ตราบเท่าที่ [S] อยู่ใน Shrine ไม่สามารถนำ [S] เข้ามาในสนามจาก Shrine ได้ 
	{card_ability, 
		s1_dark_no80_a2, 336, 2,
			n,[],
				n, null, [],									
					y, [{zone, [shrine_cards]}, {action, in_shrine}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{disallow, shrine_to_arena}]}, null,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ 
	{card_ability, 
		s1_dark_no82_a1, 338, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{attack, to_df}]}, null,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%ผู้ควบคุม [S] ไม่สามารถกำหนด Line ให้ [S] อยู่ที่ Df Line ได้ 
	{card_ability, 
		s1_dark_no82_a2, 338, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, on_arena}],													
						n, null, [null], null, [], null,
							y, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{disallow, assign_line_to_df}]}, null,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%ขณะที่ [S] ต่อสู้ Seal ที่ต่อสู้กับ [S] Sp -1 จนจบ Subturn
	{card_ability,  
		s1_dark_no83_a1, 339, 1,
			n, [],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, fight}],													
						n, null, [null], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               n, null, [], null,											
											y, null, [arena_zone], n, [{action, fight}],
												y, {do_not_need_select, 0, [{sp, -1}]} ,end_of_subturn										
	},
	% charging3
	{card_ability, 
		s1_dark_no86_a1, 342, 1,
			y, [{turn, at}, {pharse, checkup}],
				n, null, [],									
					y, [{zone, [arena_zone]}, {counter, {less_than, 3}}],													
						n, null, [null], null, [], null,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{counter, 1}]}, null,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%ตราบเท่าที่ [S] ติด Mystic Card ประเภท [Tarot]: The Moon [S] At +2
	{card_ability, 
		s1_dark_no87_a1, 343, 1,
			n, [],
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
	%เมื่อ [S] ถูกโจมตี [S] ติด Last Dance Curse At +2 / 1 Turn
	{card_ability,  
		s1_dark_no88_a1, 344, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}, {action, attacked}, {protect_curse, {n, last_dance_curse}}, {curse, {n, last_dance_curse}}],													
						n, null, [null], null, [], null,
							c, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{curse, {last_dance_curse, {at, 2}}}]}, 2,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%เมื่อ Seal ใบอื่นโจมตี [S] At +1 / 1 Turn
	{card_ability,
		s1_dark_no89_a1, 345, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}],													
						y, null, [arena_zone], n, [{action, attacker}], 1,
							n, [],
			               n, null, [], null,
									n, null, null, [],	null,												
					               y, y, {do_not_need_select, 0, [{at, 1}]}, 2,											
											n, null, [null], null, [],
												null, [] ,null										
	},
	%เมื่อ [S] เข้ามาในสนามจากกองการ์ด Seal 1 ใบสูญเสีย 1 Charge Counter และ Seal 1 ใบได้รับ 1 Charge Counter
	{card_ability,
		s1_dark_no90_a1a, 346, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {action, deck_to_arena}, {stack_check, self}],													
						y, null, [arena_zone], y, [{counter, {more_than, 1}}], 1,
							n, [],
			               n, n, [], null,
									n, null, null, [],	null,												
					               n, n, [], null,											
											y, null, [arena_zone], y, [{counter, {more_than, 1}}],
												y, {owner_select_exact_target, 1, [{counter, -1}]} ,null										
	},
	%เมื่อ [S] เข้ามาในสนามจากกองการ์ด Seal 1 ใบสูญเสีย 1 Charge Counter และ Seal 1 ใบได้รับ 1 Charge Counter
	{card_ability, 
		s1_dark_no90_a1b, 346, 1,
			n,[],
				n, null, [],									
					y, [{action, into_arena}, {action, deck_to_arena}, {stack_check, self}],													
						y, null, [arena_zone], y, [{counter, {more_than, 1}}], 1,
							n, [],
			               n, n, [], null,
									n, null, null, [],	null,												
					               n, n, [], null,											
											y, null, [arena_zone], y, [],
												y, {owner_select_exact_target, 1, [{counter, 1}]} ,null										
	},
	%เมื่อ [S] ตก Shrine จากกองการ์ด Seal 1 ใบสูญเสีย 1 Charge Counter และ Seal 1 ใบได้รับ 1 Charge Counter
	{card_ability, 
		s1_dark_no90_a1c, 346, 1,
			n,[],
				n, null, [],									
					y, [{zone, [shrine_cards]}, {action, into_shrine}, {action, deck_to_shrine}, {stack_check, card_destroy}],													
						y, null, [arena_zone], y, [], 1,
							c, [],
			               n, n, [], null,
									n, null, null, [],	null,												
					               n, n, [], null,											
											y, null, [arena_zone], y, [],
												y, {owner_select_exact_target, 1, [{counter, 1}]} ,null				
	},
	%เมื่อ [S] ตก Shrine จากกองการ์ด Seal 1 ใบสูญเสีย 1 Charge Counter และ Seal 1 ใบได้รับ 1 Charge Counter
	{card_ability, 
		s1_dark_no90_a1d, 346, 1,
			n,[],
				n, null, [],									
					y, [{zone, [shrine_cards]}, {action, into_shrine}, {action, deck_to_shrine}, {stack_check, card_destroy}],													
						y, null, [arena_zone], y, [], 1,
							c, [],
			               n, n, [], null,
									n, null, null, [],	null,												
					               n, n, [], null,											
											y, null, [arena_zone], y, [],
												y, {owner_select_exact_target, 1, [{counter, -1}]} ,null										
	},
	%เมื่อ [S] ตก Shrine จากสนาม Seal 1 ใบในสนาม At -1 / Subturn
	{card_ability, 
		p1_dark_no115_a1, 371, 1,
			n, [],
				n, null, [],									
					y, [{zone, [shrine_cards]}, {action, arena_to_shrine}, {action, into_shrine}, {stack_check, card_destroy}], 											
						y, null, [arena_zone], y, [{card_type, seal}], 1,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, null, [arena_zone], n, [{card_type, seal}],
												y, {owner_select_exact_target, 1, [{at, -1}]}, 1
	},
	%เมื่อ [S] เข้ามาในสนาม Seal 1 ใบในสนาม At -1 / Subturn
	{card_ability, 
		p1_dark_no115_a2, 371, 2,
			n, [],
				n, null, [],									
					y, [{action, into_arena}, {stack_check, self}],
						n, null, [], null, [], null,
							c, [],
								n, null, [], null,
									n, null, null, [],	null,										
										n, null, [], null, 
											y, null, [arena_zone], y, [{card_type, seal}],
												y, {owner_select_exact_target, 1, [{at, -1}]}, 1
	}
].

