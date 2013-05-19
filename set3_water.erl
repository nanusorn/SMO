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
-module(set3_water).
-export([water3_ability/0]).

water3_ability() ->
	[	
		%049 Silk Fur Unicorn 	
		%เมื่อผู้ควบคุม ร่าย [Unicorn] สำเร็จ สามารถนำ [Unicorn] 1 ใบเข้ามาในสนามจากมือผู้ควบคุม จากนั้น [Unicorn] ที่นำเข้ามานั้นกลายเป็น Inactive Seal (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no049_a1, 817, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {{controller, [hand_cards], seal, n}, {[{naming, "Unicorn"}], 1}}],													
							y, controller, [arena_zone], y, [{naming, "Unicorn"}, {action, cast_success}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,		
												y, controller, [hand_cards], n, [{naming, "Unicorn"}],
													y, {controller_can_select_exact_target, 1, [{action, move_to_arena_inactive}]}, null									 
		},
		%050 Albino Gryphon 
		%[Beast] ทุกใบในมือเรา Mp ค่าร่าย -1 (Ability นี้จะไม่ทำงานถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนาม)
		%(ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no050_a1, 818, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],													
							n, null, [null], null, [], null,
								y, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [hand_cards], y, [{card_type, seal}, {type,"Beast"}],
													y, {do_not_need_select, 0, [{mc, -1}]}, null										 
		},
		%051 Phoaviros
		%เมื่อ [S] ตก Shrine จากมือ Seal 1 ใบในสนามมี Sp ตามที่เรากำหนด 1 Turn (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no051_a1, 819, 1,
				n,[],
					n, null, [],									
						y, [{zone, [shrine_cards]}, {action, hand_to_shrine}, {action, into_shrine}],													
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,                        
										n, null, null, [],	null,												
											n, n, [], null,											
												y, null, [arena_zone], n, [{card_type, seal}],
													y, {owner_select_exact_target, 1, [{power_change, {equal, {value, sp}}}]}, null									 
		},
		%052 Frimas Castor
		%เมื่อ [S] เข้ามาในสนาม Seal 1 ใบกลายเป็น Inactive Seal ในต้น Subturn โจมตีต่อไปของผู้ควบคุม Seal นั้น (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no052_a1, 820, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, into_arena}],													
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, null, [arena_zone], n, [{card_type, seal}],												
													y, {owner_select_exact_target, 1, [{{check_up_step, 0}, {action, inactive_seal}}]}, null									 
		},
		%053 Ketos
		%ตราบเท่าที่ [S] รวมร่าง ถ้า Seal ฝ่ายเราขึ้นมือจากสนามใน Subturn นี้ เมื่อจบ Subturn เราสามารถนำ [Monster] 1 ใบเข้ามาในสนามจากมือเรา (At Line) (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no053_a1, 821, 1,
				n,[],
					n, null, [],								
						y, [{zone, [arena_zone]}, {combine, y}, {line, 1}, {check_flag, {n, seal_return_to_hand}}],
							y, owner, [hand_cards], n, [{card_type, seal}, {action, arena_to_hand}, {action, moving_to_hand}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,
											y, y, {do_not_need_select, 0, [{add_flag, seal_return_to_hand}]}, null,											
												n, null, [null], null, [],													
													null, [], null
		},
		{card_ability, 
			s3_water_no053_a2, 821, 2,
				y, [{turn, at}, {pharse, eos}],
					n, null, [],								
						y, [{zone, [arena_zone]}, {combine, y}, {line, 1}, {check_flag, seal_return_to_hand}],
							y, owner, [hand_cards], n, [{card_type, seal}, {type, "Monster"}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,	
												y, owner, [hand_cards], n, [{card_type, seal}, {type, "Monster"}],
		 											y, {owner_can_select_exact_target, 1, [{action, move_to_arena}]}, null										 
		},
		{card_ability, 
			s3_water_no053_a3, 821, 3,
				y, [{turn, df}, {pharse, checkup}],
					n, null, [],								
						y, [{zone, [arena_zone]}, {check_flag, seal_return_to_hand}],
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,
											y, y, {do_not_need_select, 0, [{clear_flag, seal_return_to_hand}]}, null,											
												n, null, [null], null, [],													
													null, [], null
		},
		{card_ability, 
			s3_water_no053_a4, 821, 4,
				y, [{turn, df}, {pharse, eos}],
					n, null, [],	
						y, [{zone, [arena_zone]}, {combine, y}, {line, 1}, {check_flag, seal_return_to_hand}],
							y, owner, [hand_cards], n, [{card_type, seal}, {type, "Monster"}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,	
												y, owner, [hand_cards], n, [{card_type, seal}, {type, "Monster"}],
		 											y, {owner_can_select_exact_target, 1, [{action, move_to_arena}]}, null										 
		},
		{card_ability, 
			s3_water_no053_a5, 821, 5,
				y, [{turn, at}, {pharse, checkup}],
					n, null, [],								
						y, [{zone, [arena_zone]}, {check_flag, seal_return_to_hand}],
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,
											y, y, {do_not_need_select, 0, [{clear_flag, seal_return_to_hand}]}, null,											
												n, null, [null], null, [],													
													null, [], null
		},
		%055 Blue Sky Pegasus 
		%ตราบเท่าที่ [S] อยู่ในสนาม N[Pegasus] ทุกใบในมือฝ่ายเรา Mp ค่าใช้ Skill -1 (Ability นี้จะไม่ทำงานถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนาม)
		{card_ability, 
			s3_water_no055_a1, 823, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],	
							n, null, [null], null, [], null,
								y, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
											 	y, owner, [hand_cards], n, [{card_type, seal}, {naming, "Pegasus"}],
													y, {do_not_need_select, 0, [{ms, -1}]}, null	
		},
		%056 Butterfly Stingray	
		%เมื่อมีการ์ดตก Shrine ฝ่ายตรงข้าม เราสามารถทิ้ง Seal 1 ใบและ Mystic Card 1 ใบในมือเรา จากนั้นนำ Mystic Card 1 ใบใน Shrine เข้ากองการ์ด (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no056_a1a, 824, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {{owner, [shrine_cards], mystic, n}, {[], 1}}, {{owner, [hand_cards], mystic, n}, {[], 1}}],
							y, opponent, [shrine_cards], n, [{action, into_shrine}, {action, arena_to_shrine}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [hand_cards], n, [{card_type, seal}],
													y, {owner_can_select_exact_target, 1, [{action, discard}]}, null	
		},
		{card_ability, 
			s3_water_no056_a1b, 824, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {{owner, [shrine_cards], mystic, n}, {[], 1}}, {{owner, [hand_cards], seal, n}, {[], 1}}],	
							y, opponent, [shrine_cards], n, [{action, into_shrine}, {action, arena_to_shrine}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [hand_cards], n, [{card_type, mystic}],
													y, {owner_select_exact_target, 1, [{action, discard}]}, null	
		},
		{card_ability, 
			s3_water_no056_a1c, 824, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {{owner, [shrine_cards], mystic, n}, {[], 1}}, {{owner, [hand_cards], seal, n}, {[], 1}}, {{owner, [hand_cards], mystic, n}, {[], 1}}],	
							y, opponent, [shrine_cards], n, [{action, into_shrine}, {action, arena_to_shrine}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [shrine_cards], n, [{card_type, mystic}],
													y, {owner_select_exact_target, 1, [{action, move_to_deck}]}, null	
		},
		%057 Herapoht
		%เมื่อ [S] ต่อสู้ ผู้ควบคุมสามารถ Sacrifice [Water] และ/หรือ [Divine] 1 ใบ จากนั้น Seal 1 ใบในสนามติด Freeze Curse 1 Turn
		{card_ability, 
			s3_water_no057_a1a, 825, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, fight}],
							y, owner, [arena_zone], y, [{elem_or_type, [2, "Divine"]}], 1,
								{then_do, s3_water_no057_a1b}, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [arena_zone], y, [{elem_or_type, [2, "Divine"]}],
													y, {owner_can_select_exact_target, 1, [{player_action, sacrifice}]}, null	
		},
		{card_ability, 
			s3_water_no057_a1b, 825, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, fight}],
							y, null, [arena_zone], y, [{card_type, seal}, {curse, {n, freeze_curse}}, {protect_curse, {n, freeze_curse}}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,											
											n, n, [], null,											
												y, null, [arena_zone], y, [{card_type, seal}, {curse, {n, freeze_curse}}, {protect_curse, {n, freeze_curse}}],
													y, {owner_select_exact_target, 1, [{curse, freeze_curse}]}, 2														
		},
		%เมื่อ [S] ต่อสู้ ผู้ควบคุมสามารถ Sacrifice [Dark] และ/หรือ [Evil] 1 ใบ จากนั้น Seal 1 ใบในสนามติด Poison Curse 1 Turn
		% {card_ability, 
			% s3_water_no057_a2a, 825, 2,
				% n,[],
					% n, null, [],									
						% y, [{zone, [arena_zone]}, {action, fight}],	
							% y, owner, [arena_zone], n, [{elem_or_type, [5, "Evil"]}], 1,
								% {then_do, s3_water_no057_a2b}, [],
									% n, null, [], null,
										% n, null, null, [],	null,												
											% n, n, [], null,											
												% y, owner, [arena_zone], n, [{elem_or_type, [5, "Evil"]}],
													% y, {owner_can_select_exact_target, 1, [{player_action, sacrifice}]}, null	
		% },
		% {card_ability, 
			% s3_water_no057_a2b, 825, 2,
				% n,[],
					% n, null, [],									
						% y, [{zone, [arena_zone]}],	
							% y, null, [arena_zone], y, [{card_type, seal}, {curse, {n, poison_curse}}, {protect_curse, {n, poison_curse}}], 1,
								% n, [],
									% n, null, [], null,
										% n, null, null, [],	null,												
											% n, n, [], null,											
												% y, null, [arena_zone], y, [{card_type, seal}, {curse, {n, poison_curse}}, {protect_curse, {n, poison_curse}}],
													% y, {owner_select_exact_target, 1, [{curse, poison_curse}]}, 2
			%},
		%058 Cat Crab
		%ตราบเท่าที่มี [Earth] ใบอื่นในสนาม ผู้ควบคุม [S] สามารถสั่ง [S] โจมตีด้วย Df ได้ (ok, เสร็จแล้ว)
		{card_ability, 
			s3_water_no058_a1, 826, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, attacker}],	
							y, null, [arena_zone], n, [{elem, 1}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,
											y, y, {owner_can_activate_ability, 0, [{combat, {when_fight_attack, {df, power}}}]}, end_of_fighting,											
												n, null, [null], null, [],													
													null, [], null
		},
		%059 Coy Crab 
		%ตราบเท่าที่มี [S] ตั้งแต่ 2 ใบขึ้นไปในสนามฝ่ายเรา Seal ทุกใบในสนามฝ่ายตรงข้าม Sp = 0  (ok เสร็จแล้ว)
		{card_ability, 
			s3_water_no059_a1, 827, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, on_arena}],	
							n, null, [null], null, [], null,
							%y, owner, [arena_zone], y, [{card_type, seal}, {name, "Coy Crab"}], 2,
								y, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, null, [], null,											
												y, opponent, [arena_zone], n, [{card_type, seal}],													
													y, {do_not_need_select, 0, [{sp, {equal, 0}}]}, null	
		},
		%061 Sharp Horn Goat	
		%เมื่อ [S] เข้ามาในสนามจากมือ ผู้เล่นทุกคนเลือก Seal 1 ใบในสนาม Seal นั้นไม่สามารถใช้ Skill ได้ 2 Turn (ok เสร็จแล้ว)
		{card_ability, 
			s3_water_no061_a1a, 829, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, into_arena}],	
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, null, [arena_zone], y, [{card_type, seal}],
													y, {owner_select_exact_target, 1, [{skill, {cannot_use, [all]}}]}, 4	
		},
		{card_ability, 
			s3_water_no061_a1b, 829, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {action, into_arena}],	
							n, null, [null], null, [], null,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,		
												y, null, [arena_zone], y, [{card_type, seal}],	
													y, {opponent_select_exact_target, 1, [{skill, {cannot_use, [all]}}]}, 4	
		},
		%062 Aqua Otter
		%เมื่อ [Monster] และ/หรือ [Water] ขึ้นมือจากสนามฝ่ายเรา เราสามารถนำ [S] เข้ามาในสนามจากมือ ( ok เสร็จแล้ว)
		{card_ability, 
			s3_water_no062_a1, 830, 1,
				n,[],
					n, null, [],								
						y, [{zone, [hand_cards]}],													
							y, owner, [hand_cards], n, [{elem_or_type, [2, "Monster"]}, {action, arena_to_hand}], 1,
								n, [],
									n, null, [], null,
										n, null, null, [],	null,												
											n, n, [], null,											
												y, owner, [hand_cards], y, [{name, "Aqua Otter"}],
													y, {owner_can_select_exact_target, 1, [{action, move_to_arena}]}, null										 
		}	
].