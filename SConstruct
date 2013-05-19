# Copyright (c) 2008-2013, Nanusorn Photpipat <nanusorn@photpipat.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.

# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module (ability_effect).
import os
env = Environment(ENV = os.environ)

#######################################################################
# Erlang Builder
#ERLANG_BIN = ""
#ERLC = os.path.join(ERLANG_BIN, 'erlc')
ERLC = 'erlc'

beam_builder = Builder(
	generator =
		lambda source, target, env, for_signature:
			ERLC +
			# Append include entries
			reduce(lambda acum, path: acum+' -I'+str(path),
				[""] + env.get('ERLINCLUDE', [])) +
			# Append path entries
			reduce(lambda acum, path: acum+' -pa'+str(path),
				[""] + env.get('ERLPATH', [])) +
			' -o $TARGET $SOURCES' ,
   	    suffix = '.beam',
           src_suffix = '.erl')

env.Append(BUILDERS = {'Beam': beam_builder})

#######################################################################
# Example use
env.Append(ERLINCLUDE = [Dir("."), Dir("./subsystem")])
#env.Append(ERLPATH = [Dir("/path/to/any/library"), Dir("/path/to/other/library")])

erlang_sources = Split("""
			main_smo.erl
			smo.erl
			smo_logger.erl
			smo_acceptor_sup.erl
			smo_acceptor.erl
			smo_report_handler.erl
			smo_dispatcher.erl
			smo_database.erl
			smo_friend.erl
			smo_player.erl
			smo_player_sup.erl
			smo_player_guardian.erl
			smo_arena.erl
			smo_arena_sup.erl
			smo_arena_guardian.erl
			smo_supervisor.erl
			smo_mysql.erl
			mysql.erl
			mysql_auth.erl
			mysql_conn.erl
			mysql_recv.erl
			ascii_unicode.erl
			lib_arena.erl
			lib_arena_play.erl
			lib_deck_edit.erl
			lib_msg_controller.erl
			lib_ascii_unicode.erl
			lib_player_login.erl
			lib_player_profile.erl
			lib_lobby_protocol.erl
			lib_utf8.erl
			mnesia_odbc.erl
			mnesia_play.erl
			mnesia_table.erl
			move_to_arena.erl
			move_to_hand.erl
			move_to_library.erl
			hand_zone.erl
			seal_card.erl
			mystic_card.erl
			mystic_card_controller.erl
			mystic_check.erl
			mystic_effect.erl
			mystic_effect_activate_set1.erl
			stack_pool.erl
			play_utility.erl
			check_up_step.erl
			arena_zone.erl
			card_utility.erl
			ability_activate.erl
			ability_utility.erl
			ability_effect.erl
			ability_affect.erl
			seal_card.erl
			mystic_card.erl
			effect_activate.erl
			interfere_step.erl
			mod_ability_activate.erl
			mod_ability_effect.erl
			update_ability.erl
			card_effect_controller.erl
			card_info.erl
			card_list.erl
			card_utility.erl
			function_utility.erl
			draw_step.erl
			discard_step.erl
			main_step.erl
			casting_card.erl
			casting_controller.erl
			mystic_check.erl
			m_ability.erl
			m_ability_set2.erl
			m_skill_target.erl
			m_ability_set3.erl
			m_skill_check.erl
			m_ability_effect.erl
			m_ability_target.erl
			m_skill_effect.erl
			m_ability_set1.erl
			m_skill.erl
			m_skill_set1.erl
			growth.erl
			growth_set2.erl
			growth_set3.erl
			set1_dark.erl
			set1_earth.erl
			set1_fire.erl
			set1_light.erl
			set1_mystic.erl
			set1_seal_dark.erl
			set1_seal_earth.erl
			set1_seal_fire.erl
			set1_seal_light.erl
			set1_seal_water.erl
			set1_seal_wind.erl
			set1_water.erl
			set1_wind.erl
			set2_dark.erl
			set2_earth.erl
			set2_fire.erl
			set2_light.erl
			set2_mystic.erl
			set2_seal_dark.erl
			set2_seal_earth.erl
			set2_seal_fire.erl
			set2_seal_light.erl
			set2_seal_water.erl
			set2_seal_wind.erl
			set2_water.erl
			set2_wind.erl
			set3_dark.erl
			set3_earth.erl
			set3_fire.erl
			set3_light.erl
			set3_mystic.erl
			set3_seal_dark.erl
			set3_seal_earth.erl
			set3_seal_fire.erl
			set3_seal_light.erl
			set3_seal_water.erl
			set3_seal_wind.erl
			set3_water.erl
			set3_wind.erl
			set_precon_seal.erl
			precon_1.erl
			precon_skill.erl
			skill_card_list.erl
			skill_effect.erl
			skill_fx_player.erl
			skill_set1.erl
			skill_target_check.erl
			s_skill.erl
			s_skill_check.erl
			s_skill_effect.erl
			s_skill_set1.erl
			s_skill_set2.erl
			s_skill_set3.erl
			s_skill_target.erl
			combination.erl
			combination_set1.erl
			combination_set2.erl
			combination_set3.erl
			s_ability.erl
			s_ability_effect.erl
			s_ability_target.erl
			check_other.erl
			check_self.erl
			game_info.erl
			attribute_check.erl
			query_ability.erl
			mp_clean_step.erl
			end_of_subturn.erl
			effect_value.erl
			discard.erl
			assign_atk.erl
			assign_atk_controller.erl
			curse.erl
			curse_activation.erl
			target_attack.erl
			target_check.erl
			line_change.erl
			material_search.erl
			remove_zone.erl
			reveal_controller.erl
			s_added_skill.erl
			seal_skill.erl
			shrine_zone.erl
			using_skill_controller.erl
			break_combine.erl
			hand_atk.erl
			combat.erl
			destroy.erl
			attack_all.erl
			added_ability.erl
			added_skill.erl
			delete_counter.erl
			dialog_text.erl
	""")

beams = []

for erlang_source in erlang_sources:
	beam = env.Beam(erlang_source)
	beams.append(beam)

DIST_BEANS="./ebin"

install_beans = Install(DIST_BEANS, beams)
Alias('install', install_beans)

