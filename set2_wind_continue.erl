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
-module(set2_wind_continue).
-compile(export_all).
continue_condition() ->	
	[
	%[S] ยกเลิก Freeze Curse
		{continue_condition, 
			s2_wind_no068_a1, 580, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
	%Growth: Dimminuial, the Baby Dragon [S] At +1
		{continue_condition, 
			s2_wind_no068_a2, 580, 2,
				n,[],
					n, null, [],									
						y, [{growth, y}],													
							n, null, [], n, [], null,
								n, 
									[{growth, y}]
		},
	%ผู้เล่นไม่สามารถสั่ง [S] โจมตีได้
		{continue_condition, 
			s2_wind_no070_a2, 582, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [], null, [], null,
								n, 
									[{active_zone, [arena_zone]}]
		},
		%071   Tanorod, the Wasteland Dragon
		%[S] +At ตามจำนวน Seal ที่ติด Poison Curse
		{continue_condition, 
			s2_wind_no071_a1, 583, 1,
				n, [],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [], null, [], null,
								e, 
									[{active_zone, [arena_zone]}]
		}
	].