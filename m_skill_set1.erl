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
-module(m_skill_set1).
-compile(export_all).

skill() ->
	[
		%[Skill]-Sacrifice Seal 1 เน?เธ?; 
		{mystic_skill,
			m1_no094_s1a, 350, 1, 0, n,
				y, 
					null, m1_no094_s1b,	
						n, [],
							n, null, [],									
								y, [{zone, arena_zone}], 											
									y, owner, [arena_zone], seal, null, [], 1,			                        
										n, [], null,
											n, null, [], null,										
												n, [], null, 
													y, owner, [arena_zone], seal, null, [],
														{player_select_exact_target, 1, [{player_action, sacrifice}]} ,0,
															n, null, [null], null, null, [],
																[], null	
		},
		%เรา +Mp ตาม Mp ค่าร่ายของ Seal นั้น (Mp 0) เมื่อ [S] ใช้ Skill สำเร็จครบ 3 ครั้ง [S]
		{mystic_skill,
			m1_no094_s1b, 350, 1, 0, n,
				y, 
					null, null,	
						n, [],
							n, null, [],									
								y, [{zone, arena_zone}], 											
									y, owner, [arena_zone], seal, null, [], 1,			                        
										y, {do_not_need_select, 0, [{mp, u_Q_3skill_used}]}, 0,
											n, null, [], null,										
												n, [], null, 
													n, null, [null], null, null, [],
														{}, null,
															n, null, [null], null, null, [],
																[], null	
		},
		%[Skill]-เลือกธาตุ 1 ธาตุ จากนั้น Seal 1 ใบฝ่ายเราเปลี่ยนเป็นธาตุที่เลือก ? Turn (Mp 2)
		{mystic_skill,
			m3_no094_s1, 862, 1, 2, n,
				y, 
					null, null,	
						n, [],
							n, null, [],									
								y, [{zone, arena_zone}], 											
									y, owner, [arena_zone], seal, null, [], 1,			                        
										y, {do_not_need_select, 0, [{action, select_one_element}]}, 0,
											n, null, [], null,										
												n, [], null, 
													y, owner, [arena_zone], seal, null, [],
														{player_select_exact_target, 1, [{change_element, {player_select_exact_element, 1}}]}, infinite,
															n, null, [null], null, null, [],
																[], null	
		}
	].