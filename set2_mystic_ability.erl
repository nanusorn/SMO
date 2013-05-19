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
-module(set2_mystic_ability).
-export([mystic/0]).

mystic() -> 
	[
		%เมื่อ S ตก Shrine จากสนาม เรา mp +3
		{card_ability, 
			s2_no097_a1, 609, 1,
				n,[],
					n, null, [],									
						y, [{zone, [shrine_cards]}, {action, into_shrine}, {action, arena_to_shrine}, {stack_check, card_destroy}],													
							n, null, [], n, [], null,
								n, [],
				               y, y, {do_not_need_select, 0, [{mp, 3}]}, null,
										n, null, null, [],	null,												
						               n, n, {}, null,											
												n, null, [null], null, [],
													null, [], null									 
		}
	].
