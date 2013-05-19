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
-module(added_ability).
-compile(export_all).

ps_ability() ->
[
	{card_ability, 
		s1_independ_no001_s1, xxx, 1,
			null, null,
				n, [],
					n, null, [],									
						y, [{action, on_arena}],													
							y, owner, [arena_zone], null, [{elem, 2}, {action, skill_success}], 1,
								y, {do_not_need_select, 0,[{mp, '+1'}]},
				                    n, null, [], null,
										n, null, null, [],	null,												
						                    y, y, {do_not_need_select, 0, [{at, '+F'}]}, null,											
												n, null, [null], null, [],
													null, [], null,
														n, null, [null], null, [],
															null, [], null
	}															
].