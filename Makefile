# Copyright (c) 2008-2013, Nanusorn Photpipat <nanusorn@photpipat.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.-module (ability_effect).
# ----------------------------------------------------------
# 
#
#
# ----------------------------------------------------------
.SUFFIXES: .erl .beam .hrl .yrl

ESRC=.
EBIN=
ERL=erl -boot start_clean
ERLC=erlc

ifeq (${TYPE},release)
EBIN=release
TYPE_FLAGS=
else
EBIN=debug
TYPE_FLAGS=-DDEBUG +debug_info
endif

MODS = \
		main_smo \
		smo \
		smo_logger \
		smo_acceptor_sup \
		smo_acceptor \
		smo_report_handler \
		smo_dispatcher \
		smo_database \
		smo_friend \
		smo_player \
		smo_player_sup \
		smo_player_guardian \
		smo_arena \
		smo_arena_sup \
		smo_arena_guardian \
		smo_supervisor \
		smo_mysql \
		mysql \
		mysql_auth \
		mysql_conn \
		mysql_recv \
		ascii_unicode \
		lib_arena \
		lib_arena_play \
		lib_deck_edit \
		lib_msg_controller \
		lib_ascii_unicode \
		lib_player_login \
		lib_player_profile \
		lib_lobby_protocol \
		lib_utf8 \
		mnesia_odbc \
		mnesia_play \
		mnesia_table \
		move_to_arena \
		move_to_hand \
		move_to_library \
		hand_zone \
		seal_card \
		mystic_card \
		mystic_card_controller \
		mystic_check \
		mystic_effect \
		mystic_effect_activate_set1 \
		stack_pool \
		play_utility \
		check_up_step \
		arena_zone \
		card_utility \
		ability_activate \
		ability \
		ability_utility \
		ability_effect \
		ability_affect \
		seal_card \
		mystic_card \
		effect_activate \
		interfere_step \
		mod_ability_activate \
		mod_ability_effect \
		update_ability \
		card_effect_controller \
		card_info \
		card_list \
		card_utility \
		function_utility \
		draw_step \
		discard_step \
		main_step \
		casting_card \
		casting_controller \
		mystic_check \
		m_ability \
		m_ability_set2 \
		m_skill_target \
		m_ability_set3 \
		m_skill_check \
		m_ability_effect \
		m_ability_target \
		m_skill_effect \
		m_ability_set1 \
		m_skill \
		m_skill_set1 \
		growth \
		growth_set2 \
		growth_set3 \
		set1_dark \
		set1_earth \
		set1_fire \
		set1_light \
		set1_mystic \
		set1_seal_dark \
		set1_seal_earth \
		set1_seal_fire \
		set1_seal_light \
		set1_seal_water \
		set1_seal_wind \
		set1_water \
		set1_wind \
		set2_dark \
		set2_earth \
		set2_fire \
		set2_light \
		set2_mystic \
		set2_seal_dark \
		set2_seal_earth \
		set2_seal_fire \
		set2_seal_light \
		set2_seal_water \
		set2_seal_wind \
		set2_water \
		set2_wind \
		set3_dark \
		set3_earth \
		set3_fire \
		set3_light \
		set3_mystic \
		set3_seal_dark \
		set3_seal_earth \
		set3_seal_fire \
		set3_seal_light \
		set3_seal_water \
		set3_seal_wind \
		set3_water \
		set3_wind \
		set_precon_seal \
		precon_1 \
		precon_skill \
		skill_card_list \
		skill_effect \
		skill_fx_player \
		skill_set1 \
		skill_target_check \
		s_skill \
		s_skill_check \
		s_skill_effect \
		s_skill_set1 \
		s_skill_set2 \
		s_skill_set3 \
		s_skill_target \
		combination \
		combination_set1 \
		combination_set2 \
		combination_set3 \
		s_ability \
		s_ability_effect \
		s_ability_target \
		check_other \
		check_self \
		game_info \
		attribute_check \
		query_ability \
		mp_clean_step \
		end_of_subturn \
		effect_value \
		discard \
		assign_atk \
		assign_atk_controller \
		curse \
		curse_activation \
		target_attack \
		target_check \
		line_change \
		material_search \
		remove_zone \
		reveal_controller \
		s_added_skill \
		seal_skill \
		shrine_zone \
		sort \
		using_skill_controller \
		break_combine \
		hand_atk \
		combat \
		destroy \
		attack_all \
		added_ability \
		added_skill \
		delete_counter \
		dialog_text \
		lib_database
				
TARGET_FILES= ${MOD:%=${EBIN}/%.beam}

# ----------------------------------------------------------
# FLAGS
# ----------------------------------------------------------
CFLAGS += -pa ${EBIN} -W ${TYPE_FLAGS} +warn_obsolete_guard

# ----------------------------------------------------------
# Targets
# ----------------------------------------------------------

all: 	${MODS:%=${EBIN}/%.beam}

server: all
		${ERL} -pa '${EBIN}' -smp -s main_smo start

$(EBIN)/%.beam: ${ESRC}/%.erl
		${ERLC} ${CFLAGS} -o${EBIN} $<

clean:
		rm -f debug/*
		
		

