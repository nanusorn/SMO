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
-module(check_card_type).
-compile (export_all).

get_type(Data) ->
	case Data of
		"Bandit" 		->	<<1:16>>;
		"Beast"  		->	<<2:16>>;
		"Dancer"  		-> 	<<3:16>>;
		"Divine"  		->   	<<4:16>>;
		"Dragon"  		-> 	<<5:16>>;
		"Dragon's Egg"  ->   	<<6:16>>;
		"Evil"  		->   	<<7:16>>;
		"Fish"  		->	<<8:16>>;
		"Gunner"  		->   	<<9:16>>;
		"Insect"  		->   	<<10:16>>;
		"Knight"  		->   	<<11:16>>;
		"Machine"  	->   	<<12:16>>;
		"Mage"		->   	<<13:16>>;
		"Merchant"  	->   	<<14:16>>;
		"Messenger"  	->   	<<15:16>>;
		"Monster"  	->   	<<16:16>>;
		"Musician"  	->   	<<17:16>>;
		"Ninja"  		->   	<<18:16>>;
		"Plant"  		->   	<<19:16>>;
		"Pugilist"  		->   	<<20:16>>;
		"Scientist"  	->   	<<21:16>>;
		"Tamer"  		->   	<<22:16>>;
		"Titan"  		->   	<<23:16>>;
		_ -> 0
	end.
	
get_naming(Data) ->
	case Data of
		"Alana"			->	<<1:16>>;
		"Andre"			->	<<2:16>>;
		"Angel"			->	<<3:16>>;
		"Angel of Music"	->	<<4:16>>;
		"Annedisonge"		->	<<5:16>>;
		"Archer"			->	<<6:16>>;
		"Baby Dragon"		->	<<7:16>>;
		"Benshee"			->	<<8:16>>;
		"Bishop"			->	<<9:16>>;
		"Bug Tamer"		->	<<10:16>>;
		"Centaur"			->	<<11:16>>;
		"Charles"			->	<<12:16>>;
		"Curse"			->	<<13:16>>;
		"Dark Pit Rat"		->	<<14:16>>;
		"Doll"			->	<<15:16>>;
		"Dragogriff"		->	<<16:16>>;
		"Dragoon"			->	<<17:16>>;
		"Fairy"			->	<<18:16>>;
		"Felasia"			->	<<19:16>>;
		"Francessca"		->	<<20:16>>;
		"Fudenun"			->	<<21:16>>;
		"Garry"			->	<<22:16>>;
		"Garuda"			->	<<23:16>>;
		"Gauntlet"			->	<<24:16>>;
		"Georgina"			->	<<25:16>>;
		"Grace the Valkyrie"	->	<<26:16>>;
		"Gregory"			->	<<27:16>>;
		"Griffin"			->	<<28:16>>;
		"Harison"			->	<<29:16>>;
		"Headman"		->	<<30:16>>;
		"Hunter"			->	<<31:16>>;
		"Jormungand"		->	<<32:16>>;
		"Kobold"			->	<<33:16>>;
		"Lazal"			->	<<34:16>>;
		"Magamouth"		->	<<35:16>>;
		"Martin"			->	<<36:16>>;
		"Marvin"			->	<<37:16>>;
		"Medusa"			->	<<38:16>>;
		"Mermage"		->	<<39:16>>;
		"Mermaid"			->	<<40:16>>;
		"Merman"			->	<<41:16>>;
		"Naga"			->	<<42:16>>;
		"Nerimor"			->	<<43:16>>;
		"Nightmare"		->	<<44:16>>;
		"Paladin"			->	<<45:16>>;
		"Pecca"			->	<<46:16>>;
		"Pegasus"			->	<<47:16>>;
		"Penguin"			->	<<48:16>>;
		"Phoenix"			->	<<49:16>>;
		"Poseidon"		->	<<50:16>>;
		"Prophet"			->	<<51:16>>;
		"Rat"				->	<<52:16>>;
		"Rebecca's Dragon"	->	<<53:16>>;
		"Regina"			->	<<54:16>>;
		"Salamandera"		->	<<55:16>>;
		"Shining Rat"		->	<<56:16>>;
		"Sigmund"			->	<<57:16>>;
		"Succubus"		->	<<58:16>>;
		"Thaliwilya"		->	<<59:16>>;
		"The Angel of Order"	->	<<60:16>>;
		"Therion"			->	<<61:16>>;
		"Trident"			->	<<62:16>>;
		"Unicorn"			->	<<63:16>>;
		"Vioria"			->	<<64:16>>;
		"Viper"			->	<<65:16>>;
		"Werewolf"			->	<<66:16>>;
		"Witch"			->	<<67:16>>;
		"Xerxes"			->	<<68:16>>;
		"Zadin"			->	<<69:16>>;
		"Zalom"			->	<<70:16>>;
		"Zechariah Mavin"	->	<<71:16>>;
		_ -> 0
	end.
	
get_name(Data) ->
	case Data of
		"Palanalcar, the Baby Dragon"	->	<<1:16>>;
		"Niltinco, the Baby Dragon"		->	<<2:16>>;
		"Dimminuial, the Baby Dragon"	->	<<3:16>>;
		"Pony Unicorn" ->	<<4:16>>;
		"Ratatosk" ->	<<5:16>>;
		"Firat" ->	<<6:16>>;
		"Sore Wing" ->	<<7:16>>;
		"Falkner, the Beast Tamer" ->	<<8:16>>;
		"Zalom Fortress Archer" ->	<<9:16>>;
		"Zeabird" ->	<<10:16>>;
		"Souless" ->	<<11:16>>;
		_ -> 0
	end.
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
