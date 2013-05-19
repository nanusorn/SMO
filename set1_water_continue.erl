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
-module(set1_water_continue).
-compile(export_all).

continue_condition() ->
	[
		%[Water] ใบอื่นทุกใบในสนามฝ่ายเรา At +
		{continue_condition, 
			s1_water_no49_a1, 305, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								n,
								[{active_zone, [arena_zone]}]
		},
		%Seal ทุกใบฝ่ายตรงข้ามที่มี Df น้อยกว่า [S] At -1
		{
			continue_condition, 
				s1_water_no50_a1, 306, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									n,
									[{active_zone, [arena_zone]}]
		},
		%[S] ยกเลิก Freeze Curse
		{
			continue_condition, 
				s1_water_no59_a2, 315, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่มี Seal ติด Freeze Curse [S] At +1 Df -1
		{
			continue_condition, 
				p1_water_no116_a1, 372, 1,
					n, [],
						n, null, [],									
							y, [{zone, [arena_zone]}],
								y, null, [arena_zone], y, [{curse, freeze_curse}], 1,
									n,
									[{active_zone, [arena_zone]}]
		}
	].
