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
-module(set3_dark_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] +At ตามจำนวน Dark Dream Pegasus ใน Shrine
		{continue_condition, 
			s3_dark_no080_a1, 848, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								e, 
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] รวมร่าง [S] สามารถโจมตีข้ามไปยัง Df Line ได้ (At Line)
		{continue_condition, 
			s3_dark_no081_a1, 849, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {combine, y}, {line, 1}],											
							n, null, [null], null, [], null,
								n,
									[{active_zone, [arena_zone]}]
		},
		%ไม่สามารถสั่ง [S] โจมตีได้ถ้าผู้ควบคุมไม่มี Mystic Card ใน Shrine
		{continue_condition, 
			s3_dark_no082_a1, 850, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],											
							y, controller, [shrine_cards], null, [{card_type, mystic}], {equal_to, 0},
								n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี Grace, the Valkyrie อยู่ในสนาม [S] สามารถโจมตีข้ามไปยัง Df Line ได้ 
		{continue_condition, 
			s3_dark_no087_a1, 855, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],											
							y, null, [arena_zone], null, [{card_type, seal}, {name, "Grace, the Valkyrie"}], 1,
								n,
									[{active_zone, [arena_zone]}]
		},
		%Growth: [Dragon] ธาตุ [Dark] + [Monster] ตราบเท่าที่ [S] รวมร่าง [S] สามารถโจมตี Seal ใบรองรวมร่างได้
		% {continue_condition, 
			% s3_dark_no088_a1, 856, 1, 
				% n,[],
					% n, null, [],									
						% y, [{zone, [arena_zone]}, {combine, y}],											
							% n, null, [null], null, [], null,
								% n,
									% [{active_zone, [arena_zone]}]
		% },
		%ตราบเท่าที่ [S] อยู่ใน Shrine [Beast] ทุกใบในสนามฝ่ายเรา At +1
		%(ถ้ามี [S] มากกว่า 1 ใบใน Shrine เรา ให้เลือก Ability นี้ทำงานได้เพียง 1 ใบเท่านั้น)	
		{continue_condition,
			s3_dark_no089_a1, 857, 1, 
				n,[],
					n, null, [],									
						y, [{zone, [shrine_cards]}],													
							n, null, [], null, [], null,
								n, 
									[{zone, [shrine_cards]}]
		}	
	].
