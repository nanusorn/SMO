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
-module(added_skill).
-compile(export_all).

ps_ability() ->
[
	{card_skill,
			s1_mystic_no001_s1, xxx, 1, 3, n,
				y, 
					null, null,	
						n, [],
							n, null, [],									
								y, [{combine, {elem, 2}}], 											
									y, null, [arena_zone], seal, y, [{speed, "2345"}], 1,			                        
										n, [], null,
											n, null, [], null,										
												n, [], null, 
													y, null, [arena_zone], seal, y, [{speed, "2345"}],
														{player_select_exact_target, 1, [{curse, freeze_curse}]} , 4,
															n, null, [null], null, null, [],
																[], null	
	}
].