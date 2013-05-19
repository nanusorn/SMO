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
-module(set2_fire_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] +At ตามจำนวน Seal ที่อยู่ใน At Line ฝ่ายเรา
		{continue_condition,
			s2_fire_no31_a1, 543, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],											
								n, null, [null], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%[S] -At ตามจำนวน Seal ที่อยู่ใน At Line ฝ่ายตรงข้าม
		{continue_condition,
			s2_fire_no31_a2, 543, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],											
								n, null, [null], null, [], null,
									e,
										[{active_zone, [arena_zone]}]
		},								
		%ตราบเท่าที่ [S] รวมร่างกับ [Dragon] [S] สามารถโจมตีข้ามไปยัง Df Line ได้
		{continue_condition,
			s2_fire_no33_a1, 545, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}, {combine, {type, "Dragon"}}],													
								n, null, [null], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},		
		%[Fire] ทุกใบในสนามฝ่ายเรา At +1 Df -3 (Df Line)
		{continue_condition,
			s2_fire_no34_a1, 546, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}, {line, 0}],													
								n, null, [null], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		{continue_condition,
			s2_fire_no35_a2, 547, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		{continue_condition,
			s2_fire_no38_a1, 550, 1,
					n,[],
						n, null, [],									
							y, [{growth, y}],													
								n, null, [null], null, [], null,
									n, 
										[{growth, y}]
		},
		%ตราบเท่าที่มี [Fire] ใน Shrine เรา [S] At +1
		%{continue_condition,
		%	s2_fire_no39_a1, 551, 2, 
		%		n, [],
		%			n, null, [],									
		%				y,[{zone, [arena_zone]}], 											
		%					y, owner, [shrine_cards], n, [{elem, 4}], 1,
		%						n, 
		%							[{active_zone, [arena_zone]}]
		%},
		%ตราบเท่าที่มี [Fire] ใน Shrine เรา [S] At +1
		{continue_condition,
			s2_fire_no39_a1, 551, 1, 
				n, [],
					n, null, [],									
						y,[{zone, [arena_zone]}], 											
							y, owner, [shrine_cards], n, [{elem, 4}], 1,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี [Knight] ใน Shrine เรา [S] At +2
		{continue_condition,
			s2_fire_no39_a2, 551, 2, 
				n, [],
					n, null, [],									
						y,[{zone, [arena_zone]}], 											
							y, owner, [shrine_cards], n, [{type, "Knight"}], 1,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี [Dragon] ตั้งแต่ 3 ใบขึ้นไปในสนามฝ่ายเรา [S] At +2
		{continue_condition,
			s2_fire_no40_a1, 552, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								y, owner, [arena_zone], y, [{type, "Dragon"}], 3,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%Growth: [Dragon's Egg] ผู้เล่นฝ่ายตรงข้ามไม่สามารถสั่ง Seal โจมตีข้ามไปยัง Df Line ได้ (At line)
		{continue_condition,
			s2_fire_no42_a3, 554, 3,
					n,[],
						n, null, [],									
							y, [{growth, y}, {line, 1}],													
								n, null, [null], null, [], null,
									n, 
										[{growth, y}]
		},
		%[S] +At ตามจำนวน [S] ใบอื่นใน Shrine เรา
		{continue_condition,
			s2_fire_no45_a1, 557, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
		{continue_condition,
			s2_fire_no46_a2, 558, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		}
	].