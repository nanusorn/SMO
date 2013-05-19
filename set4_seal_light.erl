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
-module(set4_seal_light).
-export([light/0]).
light() ->
[ 
	{seal_card,1025,4,1,["Metallica Dragon"],["Dragon", "Machine"],[6],[],4,2,3,7,8,4,3},
	{seal_card,1026,4,2,["Imsarn, the Shining Dragogriff T-II"],["Beast", "Dragon"],[6],["Dragogriff"],3,3,2,7,8,4,3},
	{seal_card,1027,4,3,["Angel of Sacred Rule T-II"],["Titan"],[6],["Angel"],4,3,3,9,10,3,3},
	{seal_card,1028,4,4,["Caelnavis, the Airship of Prophet"],["Machine"],[6],["Prophet"],2,1,2,6,7,4,3},
	{seal_card,1029,4,5,["Kipp, the Bug Tamer"],["Tamer"],[6],["Bug Tamer"],1,1,1,4,5,1,3},
	{seal_card,1030,4,6,["Palanalcarion, the Light Dragon T-II"],["Dargon"],[6],[],3,1,2,7,8,3,2},
	{seal_card,1031,4,7,["Rebecca, the Dragon Breeder"],["Tamer"],[6],[],1,1,1,4,5,3,2},
	{seal_card,1032,4,8,["Pegasus Chariot T-II"],["Machine"],[6],["Pegasus"],2,1,2,6,6,5,2},
	{seal_card,1033,4,9,["Neos Guardian"],["Machine"],[6],[],3,1,2,4,7,4,2},
	{seal_card,1034,4,10,["Crystal Wing Fairy"],["Divine"],[6],["Fairy"],2,1,2,6,6,4,2},
	{seal_card,1035,4,11,["Alpha-A"],["Machine"],[6],[],1,1,1,4,4,3,1},
	{seal_card,1036,4,12,["Mountain Goat Centaur"],["Knight"],[6],["Centaur"],2,1,2,6,6,4,1},
	{seal_card,1037,4,13,["White Drillhead"],["Fish"],[6],[],2,1,2,6,7,3,1},
	{seal_card,1038,4,14,["Fairy Music Box T-II"],["Machine"],[6],["Fairy"],1,1,1,3,5,0,1},
	{seal_card,1039,4,15,["Mountain Goat"],["Beast"],[6],[],2,1,1,5,6,3,1}
].

%{seal_card, card_id, card_set, card_no, card_name, card_type, card_element, card_naming,
%mp_cast, mp_atk, level, attack, defent, speed, rarity}
