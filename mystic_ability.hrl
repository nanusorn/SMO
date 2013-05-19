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
-record(mystic_ability,
					{	m_ability_id, card_id, m_ability_number, can_interfere,
							need_check_self, self_condition_check,
							need_other_check, player_card_check, player_card_zone_check, card_tyep_check, include_this_check, other_condition_check, number_of_require,
								then_do_id,
									continuous_type,
										have_fx_to_owner, fx_owner_got_type, owner_fx_select, fx_to_owner, owner_fx_duration,
										have_fx_to_any_player, player_got_fx, fx_player_got_type, player_fx_select, fx_to_player, player_fx_duration, 										
										have_fx_to_this, fx_this_got_type, this_fx_select, fx_to_this, this_fx_duration,
										have_fx_to_target, target_card_owner, target_card_zone_check, target_card_type, target_include_this, target_condition,
											fx_target_got_type, target_fx_select, fx_target_receive, target_fx_duration,
										have_fx_to_beyond_target, beyond_target_card_owner, beyond_target_card_zone, beyond_target_card_type, beyond_target_include_this, beyond_target_condition,
											fx_beyond_target_got_type, beyond_target_fx_select, fx_beyond_target_receive, beyond_target_fx_duration	
					}
				).
