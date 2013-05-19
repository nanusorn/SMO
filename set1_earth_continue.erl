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
-module(set1_earth_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] +At ตามจำนวน Seal ที่ติด Curse ในสนาม
		{continue_condition, 
			s1_earth_no019_a1, 275, 1,
				n, [],
					n, null, [],									
						y, [{zone, [arena_zone]}], 											
							n, null, [null], null, [], null,
								e,
								[{active_zone, [arena_zone]}]
		},
			%ตราบเท่าที่ [S] อยู่ที่ At Line สามารถโจมตีข้ามไปยัง Seal ฝ่ายตรงข้ามที่อยู่ใน Df Line
		{continue_condition, 
			s1_earth_no022_a1, 278, 1,
					n, [],
						n, null, [],									
							y, [{zone, [arena_zone]}, {line, 1}],											
								n, null, [null], null, [], null,
									n,
									[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] อยู่ที่ Df Line สามารถโจมตีจาก Df ได้
		{continue_condition, 
			s1_earth_no022_a2, 278, 2,
					n, [],
						n, null, [],									
							y, [{zone, [arena_zone]}, {line, 0}],											
								n, null, [null], null, [], null,
									n,
									[{active_zone, [arena_zone]}]
		},
		%[S] สามารถโจมตีจาก Df Line ได้
		{continue_condition, 
			s1_earth_no109_a1, 365, 1,
					n, [],
						n, null, [],									
							y, [{zone, [arena_zone]}],											
								n, null, [null], null, [], null,
									n,
									[{active_zone, [arena_zone]}]
		}
	].
