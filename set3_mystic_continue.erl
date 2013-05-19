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
-module(set3_mystic_continue).
-export([continue_condition/0]).

% e_check ใช้กับ PS ที่เป็น Continuous โดยมีคำว่ายกเว้่น หรือไม่ก็ได้ 
	% แต่ต้อง มีการ ตรวจสอบ Condition ของเป้าหมายที่ Mystic นั้น paste ตลอดเวลา 
	% เช่น ถ้า seal ที่ติด เป็น Dark  ต้องตรวจสอบ ว่ายังคงเป็น Dark หรือไม่ เพราะ เมื่อไม่เป็น Dark แล้ว ค่าพลังจะไม่เปลี่ยน
	% e_check ต้อง renew แม้ เป้าหมาย ไม่เปลี่ยนแปลงเพราะ เป้าหมายจะ เป็น เป้าที่ Mystic นั้น  Paste เท่านั้น
% ถ้าเป็น PS แต่ไม่มีคำว่ายกเว้น ไม่ต้องทำเป็น Continuous
% e ใช้กับ Continuous ที่ต้อง check สภาพแวดล้อม เช่น การ Check จำนวน ของการ์ด ใดๆ ณ. Zone ใดๆ

continue_condition() ->
	[
		% 096   Atimazo Sword
		% Seal ที่ [S] ติด - At ตามจำนวน Seal ที่ผู้ควบคุม Seal นั้นควบคุมอยู่ [interfere]
		{continue_condition, 
			s3_no096_a1, 864, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [], null, [], null,
								e, 
									[{zone, [arena_zone]}]
		},
		
		% % 100 Pege Lagoon
		% %2. ถ้าไม่มี [Unicorn] และ/หรือ [Pegasus] ในสนาม Sacrifice [S]
		% {continue_condition,
			% s3_no100_a1, 868, 1,
				% n,[],
					% n, null, [],									
						% y, [{zone, [arena_zone]}],											
							% y, null, [arena_zone], null, [{naming_or, ["Unicorn", "Pegasus"]}], {less_than, 1},
							% %y, null, [arena_zone], null, [], 1,
								% n,
								% [{zone, [arena_zone]}]
		% },
		
		% % 101  Benediction
		% % นำ Seal 1 ใบใน Shrine เราขึ้นมือ
		% % ยกเว้น: [Evil] และ/หรือ [Machine]
		% {continue_condition,
			% s3_no101_a1, 869, 1,
				% n,[],
					% n, null, [],									
						% y, [{zone, [arena_zone]}, {paste_on, {[{type, {n, "Evil"}}, {type, {n, "Machine"}}], 1}}],											
							% n, null, [null], null, [], null,
								% n,
								% [{active_zone, [arena_zone]}]
		% },
		% 102   Chaotic World
		% เลือกธาตุ 1 ธาตุ จากนั้น Seal ที่ [S] ติดเปลี่ยนเป็นธาตุที่เลือก [interfere]
		% ยกเว้น: [Divine]
		{continue_condition,
			s3_no102_a1, 870, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on,{[{type, {n, "Divine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}]
		},
		% 103   Holy Sun 
		% เลือก 1 อย่าง 
			% - ถ้า Seal ที่ Holy Sun ติดคือ [Light] Seal นั้น At + 2
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine
		{continue_condition, 
			s3_no103_a1, 871, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{elem, 6}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
			% - ถ้า Seal ที่ Holy Sun ติดคือ [Fire] Seal นั้น ติด At + 2
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine
		{continue_condition, 
			s3_no103_a2, 871, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{elem, 4}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
			% - ถ้า Seal ที่ Holy Sun ติดคือ [Divine] Seal นั้น ติด At + 2 
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine
		{continue_condition, 
			s3_no103_a3, 871, 3,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{type, "Divine"}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
		% 104   Light Tear Whip
		% ตราบเท่าที่ผู้เล่นฝ่ายตรงข้ามควบคุม Seal มากกว่าเจ้าของ Seal ที่ [S] ติด Seal ที่ [S] ติด At +1 [interfere]
		{continue_condition, 
			s3_no104_a1, 872, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}]
		},
		% 107   Beauty & the Beast
		% เลือก 1 อย่าง
			% - ถ้า Seal ที่ [S] ติดคือ [Monster] Seal นั้น At +2
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine]
			{continue_condition, 
			s3_no107_a1, 875, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{type, "Monster"}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
			% - ถ้า Seal ที่ [S] ติดคือ [Beast] Seal นั้น At +2 
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine]
		{continue_condition, 
			s3_no107_a2, 875, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{type, "Beast"}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
			% - ถ้า Seal ที่ [S] ติดคือ [Dragon] Seal นั้น At +1 
			% ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine]
		{continue_condition, 
			s3_no107_a3, 875, 3,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{type, "Dragon"}, {type, {n, "Machine"}}], 1}}],											
							n, null, [], null, [], null,
								e_check,
								[{active_zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		}
	].


