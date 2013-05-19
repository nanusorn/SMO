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
-record(card_skill, 
			{skill_id, card_id, skill_no,
				skill_condition_need_check, 
					then_assign, then_do,
						playerown_must_check, playerown_check,
							player_must_check, player_side_check, player_check,									
								owner_must_check, owner_check, 											
									other_must_check, other_player_check, other_present_check, other_card_type_check, other_self_include_check, other_check, other_match_count,			                        
										playerown_have_effect, playerown_effect, playerown_duration_effect,
											player_have_effect, player_which_got_effect, player_effect,	player_duration_effect,										
												owner_have_effect, owner_effect, owner_duration_effect, 
													other_target_have_effect, target_player_check, target_present_check, target_card_type_check, target_self_include_check, target_check,
														other_effect ,other_duration_effect,
															or_target_have_effect, or_target_player_check, or_target_present_check, or_target_card_type_check, or_self_include_check, or_check,
																or_result_effect, or_duration
			}
		).