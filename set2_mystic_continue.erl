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
-module(set2_mystic_continue).
-compile(export_all).

% e_check ใช้กับ PS ที่เป็น Continuous โดยมีคำว่ายกเว้่น หรือไม่ก็ได้ 
	% แต่ต้อง มีการ ตรวจสอบ Condition ของเป้าหมายที่ Mystic นั้น paste ตลอดเวลา 
	% เช่น ถ้า seal ที่ติด เป็น Dark  ต้องตรวจสอบ ว่ายังคงเป็น Dark หรือไม่ เพราะ เมื่อไม่เป็น Dark แล้ว ค่าพลังจะไม่เปลี่ยน
	% e_check ต้อง renew แม้ เป้าหมาย ไม่เปลี่ยนแปลงเพราะ เป้าหมายจะ เป็น เป้าที่ Mystic นั้น  Paste เท่านั้น
% ถ้าเป็น PS แต่ไม่มีคำว่ายกเว้น ไม่ต้องทำเป็น Continuous
% e ใช้กับ Continuous ที่ต้อง check สภาพแวดล้อม เช่น การ Check จำนวน ของการ์ด ใดๆ ณ. Zone ใดๆ

continue_condition() ->
	[
	%Seal ทุกใบฝ่ายเราสลับ At กับ Df (ผลที่ทำให้ At เปลี่ยนแปลงไปเปลี่ยนแปลง Df และผลที่ทำให้ Df เปลี่ยนแปลงไปเปลี่ยนแปลง At)
		{continue_condition, 
			s2_no094_a1, 606, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								n, 
									[{zone, [arena_zone]}]
		},
		%เลือก 1 อย่าง
			%- ถ้า Seal ที่ Crescent ติดคือ [Dark] Seal นั้น At +2 
			%ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine] 
		{continue_condition, 
			s2_no102_a1, 614, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{elem, 5}, {type, {n, "Machine"}}], 1}}],													
							n, null, [null], null, [], null,
								e_check, 
									[{zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
			%- ถ้า Seal ที่ Crescent ติดคือ [Water] Seal นั้น At +1, Mp -1 
			%ยกเว้น: Seal ตั้งแต่ Lv 3 ขึ้นไป และ/หรือ [Machine] 
		{continue_condition, 
			s2_no102_a2, 614, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{elem, 2}, {type, {n, "Machine"}}], 1}}],
							n, null, [null], null, [], null,
								e_check, 
									[{zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		},
		%Seal ที่ [S] ติด At +2
		{continue_condition, 
			s2_no104_a1, 616, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}],
							n, null, [null], null, [], null,
								n, 
									[{zone, [arena_zone]}, {paste_on, {[{type, {n, "Machine"}}], 1}}]
		}
	].