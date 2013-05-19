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
-module(set2_dark).
-export([dark2_ability/0]).

dark2_ability() -> 
	[
		%ขณะที่ [S] ต่อสู้กับ [Light] [S] At –2
		{card_ability, 
			s2_dark_no078_a1, 590, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, fight}],													
							y, null, [arena_zone], n, [{action, fight}, {elem, 6}], 1,
								n, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, -2}]}, end_of_fighting,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ตราบเท่าที่ [S] ติด Stone Curse [S] At +2 Df +2 
		{card_ability, 
			s2_dark_no079_a1, 591, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, 2}, {df, 2}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ตราบเท่าที่ [S] ติด Freeze Curse [S] Df +2 
		{card_ability, 
			s2_dark_no079_a2, 591, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{df, 2}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ตราบเท่าที่ [S] ติด Charm Curse [S] At +2
		{card_ability, 
			s2_dark_no079_a3, 591, 3,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, 2}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ตราบเท่าที่ [S] ติด Poison Curse [S] At +3 
		{card_ability, 
			s2_dark_no079_a4, 591, 4,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, 3}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ตราบเท่าที่ [S] ติด Last Dance Curse [S] At +2 
		{card_ability, 
			s2_dark_no079_a5, 591, 5,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, 2}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%Charging 2
		{card_ability, 
			s2_dark_no080_a1, 592, 1,
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
		%[S] สามารถโจมตี Seal ใบรองรวมร่างได้ 
		{card_ability, 
			s2_dark_no083_a1, 595, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{attack, to_supporter}]}, null,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ขณะที่ [S] ต่อสู้กับ [Divine] [S] At -2
		{card_ability, 
			s2_dark_no088_a1, 600, 1,
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
		%Growth: [Dragon] + [Dark] เมื่อ [S] เข้ามาในสนาม Seal ทุกใบที่อยู่ใน At Line ติด Poison Curse 3 Turn
		{card_ability, 
			s2_dark_no088_a2, 600, 2,
				n,[],
					n, null, [],									
						y, [{growth, y}, {action, into_arena}, {stack_check, self}],
						%y, [{action, into_arena}, {stack_check, self}],
							y, null, [arena_zone], y, [{line, 1}, {protect_curse, {n, poison_curse}}, {curse, {n, poison_curse}}], 1,
								c, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               n, null, [], null,											
												y, null, [arena_zone], y, [{line, 1}, {protect_curse, {n, poison_curse}}, {curse, {n, poison_curse}}],
													y, {do_not_need_select, 0, [{curse, poison_curse}]}, 6								 
		},
		%เมื่อ [S] ถูกทำลาย Curse ทุกชนิดที่ติดบน Seal ทุกใบในสนามนับว่าผ่านไป 1 Turn
		{card_ability, 
			s2_dark_no088_a3, 600, 3,
				n,[],
					n, null, [],									
						y, [{growth, y}, {action, destroyed}],
							y, null, [arena_zone], n, [{curse, y}], 1,
								n, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               n, null, [], null,											
												y, null, [arena_zone], n, [{curse, y}],
													y, {do_not_need_select, 0, [{{curse_timer, y}, -2}]}, null									 
		},
		%ขณะที่ [S] ต่อสู้กับ [Knight] [S] At +1
		{card_ability, 
			s2_dark_no090_a1, 602, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, fight}],													
							y, null, [arena_zone], n, [{action, fight}, {type, "Knight"}], 1,
								n, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, 1}]}, end_of_fighting,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%ขณะที่ [S] ต่อสู้กับ [Light] [S] At -3
		{card_ability, 
			s2_dark_no090_a2, 602, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, fight}],													
							y, null, [arena_zone], n, [{action, fight}, {elem, 6}], 1,
								n, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               y, y, {do_not_need_select, 0, [{at, -3}]}, end_of_fighting,											
												n, null, [null], null, [],
													null, [], null									 
		},
		%เมื่อ [S] ตก Shrine จากสนาม นำ Mystic Card 1 ใบใน Shrine ออกจากเกม 
		{card_ability, 
			s2_dark_no091_a1, 603, 1,
				n,[],
					n, null, [],									
						y, [{zone, [shrine_cards]}, {action, arena_to_shrine}, {action, into_shrine}, {stack_check, card_destroy}],													
							y, null, [shrine_cards], null, [{card_type, mystic}], 1,
								n, [],
				               n, null, [], null,
										n, null, null, [],	null,												
						               n, null, [], null,											
												y, null, [shrine_cards], null, [{card_type, mystic}],
													y, {owner_select_exact_target, 1, [{action, move_to_remove_zone}]}, null									 
		},
		%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
		{card_ability, 
			s2_dark_no091_a2, 603, 2,
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
		}
	].