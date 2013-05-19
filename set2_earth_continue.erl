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
-module(set2_earth_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] ยกเลิก Freeze Curse
		{continue_condition,
			s2_earth_no19_a1, 531, 1, 
				n, [],
					n, null, [],									
						y,[{zone, [arena_zone]}], 											
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},				
		%ตราบเท่าที่มี Seal ในสนามฝ่ายตรงข้ามน้อยกว่าเรา ผู้เล่นไม่สามารถสั่ง Seal โจมตี [S] ได้
		{continue_condition,
			s2_earth_no21_a1, 533, 1, 
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {arena_count, {opponent, seal, less}}],											
							n, null, [null], null, [], null,
								n,
									[{active_zone, [arena_zone]}]
		},
		%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
		{continue_condition,
			s2_earth_no21_a2, 533, 2, 
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,							
								n, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ Angorion, the Earth Spirit อยู่ในสนาม [Earth] ทุกใบในสนามฝ่ายเรา Df +1
		{continue_condition,
			s2_earth_no22_a1, 534, 1, 
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, null, [arena_zone], null, [{card_type, seal}, {name, "Angorion, the Earth Spirit"}], 1,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] อยู่ในสนาม Angorion, the Earth Spirit ทุกใบในสนาม At +1 Sp +1 Mp ค่าใช้ Skill –1 (Effect นี้ยังคงส่งผลแม้ว่า [S] อยู่ใน Shrine)
		{continue_condition,
			s2_earth_no22_a2a, 534, 2, 
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] อยู่ใน Shrine Angorion, the Earth Spirit ทุกใบในสนาม At +1 Sp +1 Mp ค่าใช้ Skill –1 (Effect นี้ยังคงส่งผลแม้ว่า [S] อยู่ใน Shrine)
		{continue_condition,
			s2_earth_no22_a2b, 534, 2, 
				n,[],
					n, null, [],									
						y, [{zone, [shrine_cards]}],													
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [shrine_cards]}]
		},
		%Growth: [Dragon] ธาตุ [Earth] + [Monster] [S] At +1 Df +1 Sp -1
		{continue_condition,
			s2_earth_no24_a4, 536, 4, 
				n,[],
					n, null, [],									
						y, [{growth, y}],													
							n, null, [], n, [], null,
								n, 
									[{growth, y}]
		},
		%[S] สามารถโจมตีจาก Df Line ได้
		{continue_condition,
			s2_earth_no25_a1, 537, 1, 
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%Growth: [Dragon's Egg] [S] สามารถโจมตีจาก Df Line ได้
		{continue_condition,
			s2_earth_no26_a3, 538, 3, 
				n,[],
					n, null, [],									
						y, [{growth, y}],													
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%Growth: [Dragon's Egg]%[S] สามารถโจมตีข้ามไปยัง Df Line ได้
		{continue_condition,
			s2_earth_no26_a4, 538, 4, 
				n,[],
					n, null, [],									
						y, [{growth, y}],													
							n, null, [null], null, [], null,
								n, 
									[{growth, y}]
		},
		%[S] สามารถโจมตีจาก Df Line ได้
		{continue_condition,
			s2_earth_n28_a2, 540, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],												
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%[S] สามารถโจมตีจาก Df Line ได้
		{continue_condition,
			s2_earth_no29_a2, 541, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],												
							n, null, [null], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		}
	].