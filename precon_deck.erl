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
-module(precon_deck).
-export([
						deck/1
					]).
					
deck(Deck) ->
	case Deck of	
		[3] -> deck_earthly_force();
		[2] -> watery_grace();
		[1] -> windy_pride();
		[0] -> fiery_rage()
	end.
	
deck_earthly_force() ->
	Seal = 
		[
			277, 273, 277, 272, 277,
			279, 278, 279, 278, 369,
			284, 283, 369, 284, 540,
			368, 528, 368, 801, 796,
			542, 542, 796, 801, 540],
	Mystic = 
		[
			606, 354, 612, 613,
			866, 869, 353, 356,
			364, 360, 619, 874,
			873, 876, 620, 620,
			615, 615, 617, 363	],
	{Seal, Mystic}.
			
watery_grace() ->
	Seal = 
		[
			302, 304, 559, 307, 373, 
			308, 822, 308, 854, 307,
			854, 313, 313, 570, 570,
			314, 373, 572, 571, 572,
			571, 822, 372, 372, 314
		],
	Mystic = 
		[
			349, 354, 614, 866,
			869, 353, 356, 364,
			616, 873, 874, 362,
			619, 620, 870, 620,
			872, 617, 363, 875
		],
	{Seal, Mystic}.
	
windy_pride() ->
	Seal = 
		[
			831, 317, 839, 375, 832,
			325, 325, 322, 328, 327,
			588, 374, 322, 588, 842,
			782, 782, 842, 267, 328,
			267, 374, 375, 839, 327
		],
	Mystic = 
		[
			348, 611, 355, 613,
			866, 869, 353, 356,
			364, 361, 873, 874,
			359, 876, 616, 620,
			875, 872, 872, 363
		],
	{Seal, Mystic}.
	
fiery_rage() ->
	Seal = 
		[
			545, 546, 543, 293, 292,
			292, 294, 815, 551, 371,
			298, 298, 556, 554, 342,
			342, 815, 815, 294, 370,
			297, 370, 371, 808, 297
		],
	Mystic = 
		[
			350, 354, 620, 355,
			866, 869, 353, 356,
			364, 616, 361, 874,
			362, 619, 620, 613,
			871, 872, 872, 363
		],
	{Seal, Mystic}.
							
		
