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
-module(set3_water_continue).
-compile(export_all).

continue_condition() ->
	[	
		%[Beast] ทุกใบในมือเรา Mp ค่าร่าย -1 (Ability นี้จะไม่ทำงานถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนาม)
		{continue_condition, 
			s3_water_no050_a1, 818, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, null, [arena_zone], y, [{card_type, seal}, {name, "Albino Gryphon"}], {less_than, 2},
								n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] อยู่ในสนาม N[Pegasus] ทุกใบในมือฝ่ายเรา Mp ค่าใช้ Skill -1 (Ability นี้จะไม่ทำงานถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนาม)
		{continue_condition, 
			s3_water_no055_a1, 823, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, null, [arena_zone], y, [{card_type, seal}, {name, "Blue Sky Pegasus"}], {less_than, 2},
								n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี [Earth] ใบอื่นในสนาม ผู้ควบคุม [S] สามารถสั่ง [S] โจมตีด้วย Df ได้
		{continue_condition, 
			s3_water_no058_a1, 826, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, null, [arena_zone], n, [{elem, 1}], 1,
								n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี [S] ตั้งแต่ 2 ใบขึ้นไปในสนามฝ่ายเรา Seal ทุกใบในสนามฝ่ายตรงข้าม Sp = 0 
		{continue_condition, 
			s3_water_no059_a1, 827, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, owner, [arena_zone], y, [{card_type, seal}, {name, "Coy Crab"}], 2,
								n,
									[{active_zone, [arena_zone]}]
		}
	].
