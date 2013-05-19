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
-module(attribute_check).
-import (mnesia_table, [do/1]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_ability.hrl").
-include_lib("s_skill.hrl").
-include_lib("mystic_ability.hrl").
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_turn_check(Turn, RequireTurn) -> 
	if 
		RequireTurn =:= Turn -> true;
		true -> out_of_require_turn
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_turn_check(OwnTurn, OppTurn, RequireTurn, RequirePlayer) -> 
	case RequirePlayer of
		all ->
			if 
				RequireTurn =:= OwnTurn; RequireTurn =:= OppTurn -> true;
				true -> out_of_require_turn
			end;
		opponent ->
			if 
				RequireTurn =:= OppTurn -> true;
				true -> out_of_require_turn
			end;
		owner -> 
			if 
				RequireTurn =:= OwnTurn -> true;
				true -> out_of_require_turn
			end;
		controller -> true
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_pharse_check(GamePhrase, RequirePhrase) ->
	case RequirePhrase of
		GamePhrase -> true;
		_ -> out_of_require_phase
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_mp_check(Mp, RequireMp) -> 
	if
		Mp =:= RequireMp -> true;
		Mp =/= RequireMp -> 
			case RequireMp of
				{more_than, X} ->
					  if
					  		Mp > X -> true;
							true -> player_mp_less_than_require_mp
					  end
			end;
		true -> 	false
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_mp_check(_, _, _, _) -> true.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_action_check([ActionHead|Tail], RequireAction) ->
	if 
		RequireAction =:= ActionHead -> true;
		true  -> player_action_check(Tail, RequireAction)
	end;
player_action_check([], _) -> player_status_mismatch.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
player_action_check(OwnAction, OppAction, RequireAction, RequirePlayer) ->
	case RequirePlayer of
		all ->
			POwnAction = player_action_check(OwnAction, RequireAction),
			if 
				POwnAction =:= true -> true;
				true -> player_action_check(OppAction, RequireAction)
			end;
		opponent ->
			player_action_check(OppAction, RequireAction)
	end.
%+++++++++++++++********************************************++++++++++++++++++++
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
present_check(PresentZone, [RequirePresentHead|Tail]) -> 
	if 
		RequirePresentHead =:= PresentZone -> true;
		true -> present_check(PresentZone, Tail)
	end;
present_check(_, []) -> card_zone_mismatch.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
pre_zone_check(PresentZone, [RequirePresentHead|Tail]) ->
	case stack_pool:get_last_stack(self(), check_case) of
		{ok, post_skill_interfere} -> true;
		_ ->
			if 
				RequirePresentHead =:= PresentZone -> true;
				true -> pre_zone_check(PresentZone, Tail)
			end
	end;
pre_zone_check(_, []) -> card_pre_zone_mismatch.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
post_zone_check(PresentZone, [RequirePresentHead|Tail]) ->
	case stack_pool:get_last_stack(self(), check_case) of
		{ok, post_skill_interfere} ->
			if 
				RequirePresentHead =:= PresentZone -> true;
				true -> post_zone_check(PresentZone, Tail)
			end;
		_ -> true
	end;
post_zone_check(_, []) -> card_post_zone_mismatch.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
active_zone_check(PresentZone, [RequirePresentHead|Tail]) ->
	case RequirePresentHead of
		arena_zone -> 
			if 
				PresentZone =:= arena_zone; PresentZone =:= support_cards -> true;
				true -> card_zone_mismatch
			end;
		_ ->
			if 
				RequirePresentHead =:= PresentZone -> true;
				true -> active_zone_check(PresentZone, Tail)
			end
	end;
active_zone_check(_, []) -> card_zone_mismatch.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
id_check(AbilityOwnCardData, Id) ->
	{_, {_, _, CardID}}= AbilityOwnCardData,
	if
		CardID =:= Id -> true;
		true -> false
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
card_type_check(CardID, CardType) ->
	Result = mnesia_odbc:is_seal_card(CardID),
	if 
		CardType =:= mystic, Result =:= is_not_seal -> true;
		CardType =:= seal, Result =:= is_seal -> true;
		CardType =:= all -> true;
		true -> card_type_mismatch
	end.	
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
name_check(CardName, RequireName) -> 
	case RequireName of
		{n, RequireRejectName} ->
			case [RequireRejectName]--CardName of
				[] -> CardName;
				_ -> true
			end;
		_ ->
			case [RequireName]--CardName of
				[] -> true;
				_ -> require_name_mismatch
			end		
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==	
naming_check(Name, RequireNaming) ->
	if
		Name =:= [] -> naming_mismatch;
		Name =/= [] ->
			  if 
				  RequireNaming =:= null; RequireNaming =:= [null] -> true;
				  RequireNaming =/= null -> 
					  Result = [RequireNaming]--Name ,
					  if
						  Result =:= [] -> true;
						  Result =/= [] -> naming_mismatch
					  end
			  end
		end.
%------------------------------------------------------------------
naming_or_check(CardNaming, RequireNaming) ->
	if 
		CardNaming =:= [] -> mismatch_both_type;
		CardNaming =/= [] ->
			case RequireNaming of
				  {n, RequireRejectNaming} ->
					  RejectNaming = RequireRejectNaming--CardNaming,
					  if	
						  RejectNaming =:= [] -> mismatch_both_reject_type;
						  RejectNaming =/= [] -> true
					  end;
				  _ ->
					  Naming = RequireNaming--CardNaming,
					  A = length(Naming),
					  B = length(RequireNaming),
					  if 
						  A < B -> true;
						  A >= B -> mismatch_both_type
					  end		
			 end
	end.
		
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
active_status_check(Active, RequireActive) ->	 
	if 
		RequireActive =:= Active -> true;
		true -> false
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
line_check(Line, RequireLine, CardCurse) ->	 
	if 
		RequireLine =:= null -> curse_check(CardCurse, {n, dimension_curse});
		RequireLine =:= Line -> curse_check(CardCurse, {n, dimension_curse});
		true -> line_mismatch
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
any_action_check(CardAction, RequireAction, PlayStatus) ->
	%smo_logger:fmsg("action_check CardActoin:~p, RequireAction:~p, PlayStatus:~p~n", [CardAction, RequireAction, PlayStatus]),
	case RequireAction--CardAction of
		Res -> 
			case length(Res) < length(RequireAction) of
				true -> true;
				_ ->
					case RequireAction of
						fight -> 
							case PlayStatus of
								attacker_fight ->
									case [attacker]--CardAction of
										[] -> true;
										_ -> is_not_attacker
									end;
								attacked_fight ->
									case [attacked]--CardAction of
										[] -> true;
										_ -> is_not_attacked
									end
							end;
						_ -> any_action_check(CardAction, RequireAction)
					end
			end
	end.

any_action_check(_, []) -> not_match_any_action;
any_action_check(CardAction, [RequireAction|Require]) ->
	case action_check(CardAction, RequireAction) of
		true -> true;
		_ -> any_action_check(CardAction, Require)
	end.
%-----------------------------------------------------------------
action_check(CardAction, RequireAction, PlayStatus) ->
	%smo_logger:fmsg("action_check CardActoin:~p, RequireAction:~p, PlayStatus:~p~n", [CardAction, RequireAction, PlayStatus]),
	case [RequireAction]--CardAction of
		[] -> true;
		_ ->
		 	case RequireAction of
				fight -> 
					case PlayStatus of
						attacker_fight ->
							case [attacker]--CardAction of
								[] -> true;
								_ -> is_not_attacker
							end;
						attacked_fight ->
							case [attacked]--CardAction of
								[] -> true;
								_ -> is_not_attacked
							end;
						_ -> true
					end;
				_ -> action_check(CardAction, RequireAction)
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
action_check(CardAction, RequireAction) ->
	%io:format('action_check CardActoin:~p, RequireAction:~p ~n', [CardAction, RequireAction]),
	CheckResult =
	case [RequireAction]--CardAction of
		[] -> true;
		_ ->
			case RequireAction of
				fight -> 
					 CardAttacker = [attacker]--CardAction,
					 CardAttacked = [attacked]--CardAction,
					 if
						 CardAttacker =:= []; CardAttacked =:= [] -> true;
						 CardAttacker =/= [], CardAttacked =/= [] -> false
					 end;
				end_of_fighting -> 
					CardAttacker = [attacker]--CardAction,
					 CardAttacked = [attacked]--CardAction,
					 if
						 CardAttacker =:= []; CardAttacked =:= [] -> true;
						 CardAttacker =/= [], CardAttacked =/= [] -> false
					 end;
				%into_shrine ->
					%DeckToShrine = [deck_to_shrine]--CardAction,
					%HandToShrine = [hand_to_shrine]--CardAction,
					%ArenaToShrine = [arena_to_shrine]--CardAction,
					%if
						%DeckToShrine =:= []; HandToShrine =:= []; ArenaToShrine =:= [] -> true;
						%DeckToShrine =/= [], HandToShrine =/= [], ArenaToShrine =/= [] -> false
					%end;
				into_shrine ->
					MoveToShrine = [moving_to_shrine]--CardAction,
					%HandToShrine = [hand_to_shrine]--CardAction,
					if
						%MoveToShrine =:= []; HandToShrine =:= [] -> true;
						MoveToShrine =:= [] -> true;
						true -> false
					end;				
				
				in_shrine ->
					MoveToShrine = [move_to_shrine]--CardAction,
					%HandToShrine = [hand_to_shrine]--CardAction,
					if
						%MoveToShrine =:= []; HandToShrine =:= [] -> true;
						MoveToShrine =:= [] -> true;
						true -> false
					end;
				
					%case [move_to_shrine]--CardAction of
					%	[] -> true;
					%	_ -> false
					%end;
				into_arena ->
					ShrineToArena = [shrine_to_arena]--CardAction,
					HandToArena = [hand_to_arena]--CardAction,
					DeckToArena = [deck_to_arena]--CardAction,
					RemoveToAre = [remove_to_arena]--CardAction,
					Casting = [card_casting]--CardAction,
					if
						ShrineToArena =:= []; HandToArena =:= []; DeckToArena =:= []; Casting =:= []; RemoveToAre =:= [] -> true;
						true -> false
					end;
				growth_into_arena ->
					ShrineToArena = [shrine_to_arena]--CardAction,
					HandToArena = [hand_to_arena]--CardAction,
					DeckToArena = [deck_to_arena]--CardAction,
					RemoveToAre = [remove_to_arena]--CardAction,
					Casting = [card_casting]--CardAction,
					if
						ShrineToArena =:= []; HandToArena =:= []; DeckToArena =:= []; Casting =:= []; RemoveToAre =:= [] -> true;
						true -> false
					end;
				on_arena ->
					OnAreSuccess = [card_on_arena_success]--CardAction,
					if
						OnAreSuccess =:= [] -> true;
						true -> false
					end;
				into_remove ->
					DeckToRemove = [remove_from_deck]--CardAction,
					HandToRemove = [remove_from_hand]--CardAction,
					ArenaToRemove = [remove_from_arena]--CardAction,
					ShrineToRemove = [remove_from_shrine]--CardAction,
					if
						DeckToRemove =:= []; HandToRemove =:= []; ArenaToRemove =:= []; ShrineToRemove =:= [] -> true;
						true -> out_of_case_into_remove_zone
					end;
				in_remove ->
					InRemove = [move_to_remove]--CardAction,
					DeckToRemove = [deck_to_remove]--CardAction,
					HandToRemove = [hand_to_remove]--CardAction,
					ArenaToRemove = [arena_to_remove]--CardAction,
					if
						InRemove =:= []; DeckToRemove =:= []; HandToRemove =:= []; ArenaToRemove =:= [] -> true;
						true -> out_of_case_in_remove_zone
					end;
				into_library ->
					SealToLib = [moving_to_seal_deck]--CardAction,
					MysticToLib = [moving_to_mysticl_deck]--CardAction,
					if
						SealToLib =:= []; MysticToLib =:= [] -> true;
						true -> out_of_case_into_library
					end;
				move_to_arena -> %this make to support other case beyond cast_success
					ShrineToArena = [shrine_to_arena]--CardAction,
					DeckToArena = [deck_to_arena]--CardAction,
					HandToArena = [hand_to_arena]--CardAction,
					RemoveToAre = [remove_to_arena]--CardAction,
					Casting = [card_casting]--CardAction,
					if
						HandToArena =:= [], Casting =/= [] -> true;
						ShrineToArena =:= [], Casting =/= [] -> true; 
						DeckToArena =:= [], Casting =/= [] -> true;
						RemoveToAre =:= [], Casting =/= [] -> true;
						true -> false
					end;
				remove_from_arena ->
					AreShrine = [arena_to_shrine]--CardAction,
					AreDeck = [arena_to_deck]--CardAction,
					AreHand = [arena_to_hand]--CardAction,
					AreRemove = [arena_to_remove]--CardAction,
					if
						AreShrine =:= [] -> true;
						AreDeck =:= [] -> true; 
						AreHand =:= [] -> true;
						AreRemove =:= [] -> true;
						true -> false
					end;
				use_skill_success ->
					UseSkillSuccess = [use_skill_success]--CardAction,
					if
						UseSkillSuccess =:= [] -> true;
						true -> false
					end;
				cast_success ->
					CastSuccess = [cast_success]--CardAction,
					if
						CastSuccess =:= [] -> true;
						true -> false
					end;
				card_casting ->
					CardCasting = [card_casting]--CardAction,
					if
						CardCasting =:= [] -> true;
						true -> false
					end;
				assign_attack ->
					AssignAttack = [assign_attack]--CardAction,
					if
						AssignAttack =:= [] -> true;
						true -> false
					end;
				moving_to_hand ->
					MovingToHand = [arena_to_hand]--CardAction,
					if
						MovingToHand =:= [] -> true;
						true -> false
					end;
				{n, card_casting} ->
					case [card_casting]--CardAction of
						[] -> 
							case [must_paste_to_s] -- CardAction of
								[] -> true;
								_ -> it_casting_with_no_force_paste
							end;
						_ -> true
					end;
				{n, RejectAction} ->
					case [RejectAction]--CardAction of
						[] -> false;
						_ -> true
					end;
				Action ->
					if
						Action	=/= [] -> false;
						true -> true
					end
			end
	end,
	%smo_logger:fmsg("check result is ~p~n", [CheckResult]),
	CheckResult.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
stack_check({_Zone, {CardOwner, CardOrder, CardID}}, OptionGet) ->
	case OptionGet of
		self ->
			case stack_pool:get_last_stack(self()) of
				{ok, {SOwner, SOrder, SId, _}} ->
					case {SOwner, SOrder, SId} of
						{CardOwner, CardOrder, CardID} -> true;
						_ -> not_in_self_stack
					end;
				_ -> cannot_found_stack_field
			end;
		StackOption ->
			case stack_pool:get_last_stack(self(), StackOption) of
				{ok, Option} ->
					case [{CardOwner, CardOrder, CardID}] -- Option of
						[] -> true;
						_ -> not_in_stack_field
					end;
				_ -> cannot_found_stack_field
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
element_check(CardElement, RequireElement) ->
	%io:format("chk card elem ~p, ~p~n", [CardElement, RequireElement]),
	if
		CardElement =:= [] -> element_mismatch;
		CardElement =/= [] ->
			  case RequireElement of
				  {n, RequireRejectElement} ->
					  case [RequireRejectElement]--CardElement of
					  	  [] -> reject_element_mismatch;
						  _ -> true
					  end;
				  _ ->
					  case [RequireElement]--CardElement of
					  	  [] -> true;
						  _ -> element_mismatch
					  end		
			 end
	 end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
type_check(CardType, RequireType) ->
	if 
		CardType =:= [] -> card_type_mismatch;
		CardType =/= [] ->
			case RequireType of
				  {n, RequireRejectType} ->
					  case [RequireRejectType]--CardType of
					  	  [] -> reject_type_mismatch;
						  _ -> true
					  end;
				  _ ->
				  	case [RequireType]--CardType of
						[] -> true;
						_ -> card_type_mismatch
					end		  	  	
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
type_or_check(CardType, RequireType) ->	
	%io:format("type or check CardType:~p , Type Require:~p~n", [CardType, RequireType]),
	if 
		CardType =:= [] -> mismatch_both_type;
		CardType =/= [] ->
			case RequireType of
				  {n, RequireRejectType} ->
					  RejectType = RequireRejectType--CardType,
					  if	
						  RejectType =:= RequireRejectType -> true;
						  RejectType =/= RequireRejectType -> mismatch_both_reject_type
					  end;
				  _ ->
					  Type = RequireType--CardType,
					  A = length(Type),
					  B = length(RequireType),
					  if 
						  A < B -> true;
						  A >= B -> mismatch_both_type
					  end		
			 end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
elem_or_type_check(CardElem, CardType, Require) ->
	if
		CardType =:= []; CardElem =:= [] -> elem_or_type_mismatch;
		CardType =/= [], CardElem =/= [] ->
			Type = Require--CardType,
			Elem = Require--CardElem,
			A = length(Type),
			B = length(Require),
			C = length(Elem),
			if 
				A < B; C < B -> true;
				true -> elem_or_type_mismatch
			end		
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
level_check(CardDataZone,  Level, RequireLevel) ->
	%smo_logger:fmsg("Other Card to Check Level is ~p~n", [Level]),
	if
		Level =:= [] -> false;
		Level =/= [] ->
			case is_number(RequireLevel) of 
				true -> 
					if 
						RequireLevel =:= Level -> true;
						true -> level_mismatch
					end;
				false ->
					case RequireLevel of  			
						equal_to_s ->
							AbilOwnLevel = game_info:card_level(CardDataZone),
							if 
								Level =:= AbilOwnLevel -> true;
								true -> mismatch_level
							end;
						less_than_s ->
							AbilOwnLevel = game_info:card_level(CardDataZone),
							if 
								Level < AbilOwnLevel -> true; % ls = less than S
								true -> have_more_level_than_s
							end;			 
						more_than_s ->
							AbilOwnLevel = game_info:card_level(CardDataZone),
							if
								Level > AbilOwnLevel -> true; % more_than_s = more than S
								true -> have_less_level_than_s
							end;
						"123" ->
							if 
								Level =:= 1; Level =:= 2; Level =:= 3 -> true;
								true -> is_not_123
							end;
						"12" ->
							if 
								Level =:= 1; Level =:= 2 -> true;
								true -> is_not_12
							end;
						equal_to_some_of_owner_arena ->
							{_, {OwnerPid, _, _}} = CardDataZone,
							CardList = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {OwnerPid, xxx}),
							check_equal_level(Level, CardList)
					end
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
check_equal_level(Level, [CardHead|Tail]) ->
	{_, {PlayerID, CardOrder, CardID}} = CardHead,
	{_, CardLevel} = arena_zone:get_card_power(PlayerID, CardOrder, CardID, level),
	if
		Level =:= CardLevel -> true;
		Level =/= CardLevel -> check_equal_level(Level, Tail)
	end;
check_equal_level(_, []) -> no_seal_have_same_level_as_s.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
mp_cast_check(CardDataZone, MpCast, RequireMpCast) ->	 %CardDataZone = {CardZone, {OwnerPid, CardOrder, CardID}}
	if
		MpCast =:= [] -> false;
		MpCast =/= [] ->
			  if 
				  RequireMpCast =:= null -> true;
				  RequireMpCast =/= null ->
					  case is_number(RequireMpCast) of                              
						  true -> 
							  if 
								  RequireMpCast =:= MpCast -> true;
								  true -> false
							  end;
						  false ->
							  AbilOwnMpCast = game_info:card_mpcast(CardDataZone),
							  case RequireMpCast of  			
								  equal_to_s ->
									  if 
										  MpCast =:= AbilOwnMpCast -> true;
										  MpCast =/= AbilOwnMpCast -> false
									  end;
								  less_than_s ->
									  if 
										  MpCast < AbilOwnMpCast -> true; % less_than_s = less than S
										  MpCast >= AbilOwnMpCast -> false
									  end;			 
								  more_than_s ->
									  if
										  MpCast > AbilOwnMpCast -> true; % more_than_s = more than S
										  MpCast =< AbilOwnMpCast -> false
									  end;
								{more_than, X} ->
									if 
										MpCast > X -> true;
										true -> false
									end;
								  "123" ->
									  if 
										  MpCast =:= 1; MpCast =:= 2; MpCast =:= 3 -> true;
										  MpCast =/= 1, MpCast =/= 2, MpCast =/= 3 -> false
									  end
							  end
					  end
			  end
	  end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
mpat_check(CardDataZone, MpAt, RequireMpAt) ->	 %CardDataZone = {CardZone, {OwnerPid, CardOrder, CardID}}
	if
		MpAt =:= [] -> false;
		MpAt =/= [] ->
			if 
				  RequireMpAt =:= null -> true;
				  RequireMpAt =/= null ->
					  case is_number(RequireMpAt) of
						  true -> 
							  if 
								  RequireMpAt =:= MpAt -> true;
								  RequireMpAt =/= MpAt -> false
							  end;
						  false ->
							  AbilOwnMpAt = game_info:card_mpat(CardDataZone),
							  case RequireMpAt of  			
								  equal_to_s ->
									  if 
										  MpAt =:= AbilOwnMpAt -> true;
										  MpAt =/= AbilOwnMpAt -> false
									  end;
								  less_than_s ->
									  if 
										  MpAt < AbilOwnMpAt -> true; % less_than_s = less than S
										  MpAt >= AbilOwnMpAt -> false
									  end;
								  more_than_s ->
									  if
										  MpAt > AbilOwnMpAt -> true; % more_than_s = mare than S
										  MpAt =< AbilOwnMpAt -> false
									  end;
								  "123" -> 
									  if 
										  MpAt =:= 1; MpAt =:= 2; MpAt =:= 3 -> true;
										  MpAt =/= 1, MpAt =/= 2, MpAt =/= 3 -> false
									  end
							  end
					  end
			  end
	  end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
at_check(CardDataZone, At, RequireAt) ->	%CardDataZone = {CardZone, {OwnerPid, CardOrder, CardID}}
	%smo_logger:fmsg("~nCard:~p, ~nAt:~p, ~nRequire~p~n", [CardDataZone, At, RequireAt]),
	% At คือ attack power ของการ์ดที่กำลังสนใจ
	% CardData คือ CardData ของ การ์ดเจ้าของ Ability
	if
		At =:= [] -> false;
		At =/= [] ->
			  if 
				  RequireAt =:= null -> true;
				  RequireAt =/= null ->
					  case is_number(RequireAt) of
						  true ->
							  if
								  RequireAt =:= At -> true;
								  RequireAt =/= At -> false
							  end;
						  false ->
							  AbilOwnAt = game_info:card_at(CardDataZone),
							  case RequireAt of  			
								  equal_to_s ->
									  if 
										  At =:= AbilOwnAt -> true;
										  At =/= AbilOwnAt -> false
									  end;
								  less_than_s ->
									  if 
										  At < AbilOwnAt -> true;
										  At >= AbilOwnAt -> false
									  end;
								  more_than_s ->
									  if
										  At > AbilOwnAt -> true;
										  At =< AbilOwnAt -> false
									  end;
								  "123" ->
									  if 
										  At =:= 1; At =:= 2; At =:= 3 -> true;
										  At =/= 1, At =/= 2, At =/= 3 -> false
									  end;
								  own_arena_highest ->
								  		{CardZone, {OwnerPid, CardOrder, CardID}} = CardDataZone,
									  OwnArSeal1 = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {OwnerPid, xxx}),
									  OwnArSeal = OwnArSeal1 -- [{CardZone, {OwnerPid, CardOrder, CardID}}],
									  %io:format("card on owner arena are ~p~n", [OwnArSeal]),
									  is_highest(at, At, OwnArSeal);
								  opp_arena_highest ->
								  		{_, {OwnerPid, _, _}} = CardDataZone,
									  PlayerOppID = mnesia_play:get_opponent_pid(OwnerPid),
									  OppArSeal = skill_card_list:player_zone_list({opponent, [arena_zone], seal}, {xxx, PlayerOppID}),
									  %io:format("card on owner arena are ~p~n", [OppArSeal]),
									  is_highest(at, At, OppArSeal)								  
							  end
					  end
			  end
	 end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
is_highest(_, _, []) -> true;
is_highest(PowerType, Power, [{_,{PlayerID, CardOrder, CardID}}|Tail]) ->
	ComparePower =
	case PowerType of
		at ->
			{_, CardAT} = arena_zone:get_card_power(PlayerID, CardOrder, CardID, attack),
			CardAT
	end,
	%io:format("card power is ~p and each card power is ~p~n", [Power, ComparePower]),
	if
		Power < ComparePower -> is_not_the_highest_at_on_arena;
		true -> is_highest(PowerType, Power, Tail)
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
df_check(CardDataZone, Df, RequireDf) ->
	if
		Df =:= [] -> false;
		Df =/= [] -> 
			if 
				  RequireDf =:= null -> true;
				  RequireDf =/= null ->
					  case is_number(RequireDf) of
						  true ->
							  if
								  RequireDf =:= Df -> true;
								  RequireDf =/= Df -> false
							  end;
						  false ->
							  AbilOwnDf = game_info:card_df(CardDataZone),
							  case RequireDf of  			
								  equal_to_s ->
									  if 
										  Df =:= AbilOwnDf -> true;
										  Df =/= AbilOwnDf -> false
									  end;
								  less_than_s ->
									  if 
										  Df < AbilOwnDf -> true; % ls = less than S
										  Df >= AbilOwnDf -> false
									  end;
								  more_than_s ->
								  		%smo_logger:fmsg("Ability Owner Data is ~p, with Df ~p and Other Card df is ~p~n", [CardDataZone, AbilOwnDf, Df]),
										if
										  Df > AbilOwnDf -> true; % more_than_s = mare than S
										  Df =< AbilOwnDf -> false
										end;
								  "123" ->
									  if 
										  Df =:= 1; Df =:= 2; Df =:= 3 -> true;
										  Df =/= 1, Df =/= 2, Df =/= 3 -> false
									  end
							  end
					  end
			  end
	 end.
%------------------------------------------------------------------
skill_taret_sp_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, Sp, RequireSp) ->
	AllFx = card_utility:get_all_card_effect(PlayerOwnID, AbilOwnCardOrder, CardID),
	AllInterest = [{skill, can_use_even_target_sp_mismatch}],
	case function_utility:is_contain(AllInterest, AllFx) of
		[] -> sp_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, Sp, RequireSp);
		_ -> true
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
sp_check(WhichCheck, {CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, Sp, RequireSp, PlatStatus) ->
	case PlatStatus of
		use_skill ->
			case WhichCheck of
				other_check ->
					AllFx = card_utility:get_all_card_effect(PlayerOwnID, AbilOwnCardOrder, CardID),
					AllInterest = [{skill, can_use_even_target_sp_mismatch}],
					LeftOver = AllFx--AllInterest,
					Interest = AllFx--LeftOver,
					case Interest of
						[{skill, can_use_even_target_sp_mismatch}] ->	true;
						_ -> sp_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, Sp, RequireSp)
					end;
				_ -> sp_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, Sp, RequireSp)
			end
	end.
%--------------------------------------------------------------------------------------------------------
sp_check(CardDataZone, Sp, RequireSp) ->	
	%io:format('Start Speed Check'),
	%io:format('SpRequire ~p~n', [RequireSp]),
	%io:format('Sp ~p~n', [Sp]),	
	if
		Sp =:= [] -> false;
		true ->
			if 
				  RequireSp =:= null -> true;
				  true ->
					  case is_number(RequireSp) of
						  true ->
							  if
								  RequireSp =:= Sp -> true;
								  true -> not_equal_to_require_speed
							  end;
						  false ->	
							  case RequireSp of  			
								  equal_to_s ->
									AbilOwnSp = game_info:card_sp(CardDataZone),
									  if 
										  Sp =:= AbilOwnSp -> true;
										  true -> not_equal_speed
									  end;
								  less_than_s ->
									AbilOwnSp = game_info:card_sp(CardDataZone),
									  if 
										  Sp < AbilOwnSp -> true; % ls = less than S
										  true -> speed_more_than_s
									  end;
								  more_than_s ->
									AbilOwnSp = game_info:card_sp(CardDataZone),
									  if
										  Sp > AbilOwnSp -> true; % more_than_s = mare than S
										  true -> speed_less_than_s
									  end;
								  "123" ->
									  if 
										  Sp =:= 1; Sp =:= 2; Sp =:= 3 -> true;
										  true -> false
									  end;
								  "1234" ->
									  if	
										  Sp =:= 1; Sp =:= 2; Sp =:= 3; Sp =:= 4 -> true;
										  true -> false
									  end;
								  "234" ->
									  if	
										  Sp =:= 2; Sp =:= 3; Sp =:= 4 -> true;
										  true -> false
									  end;
								  "34" ->
									  if	
										  Sp =:= 3; Sp =:= 4 -> true;
										  true -> false
									  end;
								  "345" ->
									  if	
										  Sp =:= 3; Sp =:= 4; Sp =:= 5  -> true;
										  true  -> false
									  end;
								  "12345" ->
									  if	
										  Sp =:= 1; Sp =:= 2; Sp =:= 3; Sp =:= 4; Sp =:= 5 -> true;
										  true -> false
									  end;
									"2345" ->
										if
											Sp =:= 2; Sp =:= 3; Sp =:= 4; Sp =:= 5 -> true;
											true -> false
										end;
									"0123" ->
										if
											Sp =:= 0; Sp =:= 1; Sp =:= 2; Sp =:= 3 -> true;
											true -> false
										end;
									"034" ->
										if
											Sp =:= 0; Sp =:= 3; Sp =:=4 -> true;
											true -> false
										end;
									"45" ->
										if
											Sp =:= 4; Sp =:=5 -> true;
											true -> false
										end
							  end
					  end
			  end
	 end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
curse_check(Curse, RequireCurse) ->
	%smo_logger:fmsg("seal receive curse -> ~p, Require Curse ->~p ~n", [Curse, RequireCurse]),
	if 
		RequireCurse =:= y, Curse =/= []  -> true;
		RequireCurse =:= y, Curse =:= [] -> no_curse_detect;
		RequireCurse =/= y ->
			case RequireCurse of
				{n, ReqRejectCurse} ->
		   			RejectCurse = [ReqRejectCurse]--Curse,
						case ReqRejectCurse of
							charm_curse -> reject_charm(Curse); 
							last_dance_curse -> reject_last_dance(Curse);
							_ ->
								if	
									RejectCurse =:= [] -> false;
									true -> true
								end
						end;
				_ ->
					CurseResult = [RequireCurse]--Curse,
					if
						CurseResult =:= [] -> true;
						true -> 
							case RequireCurse of
								charm_curse -> check_charm(Curse); 
								last_dance_curse -> check_last_dance(Curse);
								_ -> mismatch_curse
							end
					end
			end
	end.
	
reject_charm([]) -> true;
reject_charm([{charm_curse, _}|_]) -> already_charm;
reject_charm([_|Tail]) -> reject_charm(Tail).

reject_last_dance([]) -> true;
reject_last_dance([{last_dance_curse, _}|_]) -> already_last_dance;
reject_last_dance([_|Tail]) -> reject_last_dance(Tail).


check_charm([{charm_curse, _}|_]) -> true;
check_charm([_|Tail]) -> check_charm(Tail);
check_charm([]) -> no_charm.
check_last_dance([{last_dance_curse, _}|_])  -> true;
check_last_dance([_|Tail]) -> check_last_dance(Tail);
check_last_dance([]) -> no_last_dance.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
protect_curse_check(ProtectCurse, ReqProtectCurse) ->
%	io:format('---------------------------------------ProtectCurse ~p~n', [ProtectCurse]),
%	io:format('---------------------------------------ReqProtectCurse ~p~n', [ReqProtectCurse]),
	case ReqProtectCurse of
		{n, RejProtCurse} ->
		 	ResRej = [RejProtCurse]--ProtectCurse,
			if	
				ResRej =:= []; [ProtectCurse] =:= [all];  ProtectCurse =:= [[all]] -> false;
				ResRej =/= [] -> true
			end;
		_ ->
			Res = [ReqProtectCurse]--ProtectCurse,
			if
				Res =:= [] -> true;
				Res =/= [] -> not_protect_require_curse
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
combine_check(CardDataZone, RequireCombine, PlayStatus) ->
	case PlayStatus of
		use_skill ->
			{_, {PlayerPid, CardOrder, CardID}} = CardDataZone,
			GetAllFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),				
			case  function_utility:is_contain([{skill, without_combine}], GetAllFx) of
				[{skill, without_combine}|_] -> true;
				_ ->
					combine_check(CardDataZone, RequireCombine)
			end;
		_ ->
			combine_check(CardDataZone, RequireCombine) 
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
combine_check(CardDataZone, RequireCombine) ->
	{_, {CardOwner, CardOrder, CardID}} = CardDataZone,
	Support = arena_zone:get_support_seals(CardOwner, CardOrder, CardID),
	if 
		RequireCombine =:= n, Support =/= [] -> is_combine;
		RequireCombine =:= n, Support =:= [] -> true;
		RequireCombine =:= y,  Support =/= [] -> true;
		RequireCombine =:= y,  Support =:= [] -> is_not_combine;
		RequireCombine =:= d ->  
			case length(Support) of
				1 -> true;
				_-> is_not_double_combine
			end;
		RequireCombine =:= t ->  
			case length(Support) of
				2 -> true;
				_ -> is_not_triple_combine
			end;
		RequireCombine =:= have_choice ->
			case material_search:get_combination_option(CardOwner, CardOrder, CardID) of
				{have_choise, _} -> true;
				{no_choise, _} -> have_no_combination_choice
			end;
		true -> support_check(Support, CardDataZone, RequireCombine)
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
support_check([{SOwner, SOrder, SupportCardID}|Tail], {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine) ->
	case mnesia_odbc:is_seal_card(SupportCardID) of
		is_seal ->
			case RequireCombine of
				{naming, Naming } ->
					%[SupportNaming] = do(qlc:q([X#seal_card.card_naming|| X <- mnesia:table(seal_card), X#seal_card.card_id =:= SupportCardID])),
					{ok, SupportNaming} = seal_card:get_seal_base_power(SOwner, SOrder, SupportCardID, card_naming),
					%io:format('SupportNaming ~p~n', [SupportNaming]),
					case naming_check(SupportNaming, Naming) of 
						true -> true;
						_ -> support_check(Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
					end;
				{elem, Element} ->
					%[SupportElement] = do(qlc:q([X#seal_card.card_element|| X <- mnesia:table(seal_card), X#seal_card.card_id =:= SupportCardID])),
					{ok, SupportElement} = seal_card:get_seal_base_power(SOwner, SOrder, SupportCardID, card_element),
					case element_check(SupportElement, Element) of
						true -> true;
						_ -> support_check(Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
					end;
				{type, Type} ->
					%[SupportType] = do(qlc:q([X#seal_card.card_type|| X <- mnesia:table(seal_card), X#seal_card.card_id =:= SupportCardID])),
					%io:format('SupportType combine ~p~n', [SupportType]),
					{ok, SupportType} = seal_card:get_seal_base_power(SOwner, SOrder, SupportCardID, card_type),
					case type_check(SupportType, Type) of
						true -> true;
						_ -> support_check(Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
					end;
				{card_type, CardType} ->
					case CardType of
						is_seal -> true;
						_ -> support_check(Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
					end;
				{n, Check} ->
					case support_check([{SOwner, SOrder, SupportCardID}], {CardZone, {PlayerPid, CardOrder, CardID}}, Check) of
						true -> false;
						_ -> support_check(Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
					end;
				_ -> 
					io:format("unavailable case of RequireCombine ~p~n", [RequireCombine])
			end;
		_ -> 
			case card_utility:get_card_combine_part (PlayerPid, CardOrder, CardID, option_number) of
				{ok, OptionNo} ->
					{_, SupportList} = mnesia_odbc:get_combination_support(CardID, OptionNo),
					support_code_check(SupportList, Tail, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine);
				_ -> false
			end
	end;
support_check([], _, RequireCombine) -> 
	case RequireCombine of
		{n, _} -> true;
		_ -> supporter_mismatch
	end.
%-------------------------------------------------------------------
support_code_check(FxSupport, SupportList, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine) -> % RequireCombine = {What, Interest}
	%smo_logger:fmsg("Mystic effect combine is ~p and Combination Require is ~p~n", [FxSupport, RequireCombine]),
	case	[RequireCombine] -- FxSupport of
		[] -> true;
		_ -> support_check(SupportList, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
	end.
	% case RequireCombine of
		% Support -> true;
		% _ -> support_code_check(Tail, SupportList, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine)
	% end;
% support_code_check([], SupportList, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine) ->	support_check(SupportList, {CardZone, {PlayerPid, CardOrder, CardID}}, RequireCombine).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
support_to({_SZone, {SOwner, SOrder, SID}}, {SupZone, {SupPid, SupOrder, SupID}}, Value) ->
	case Value of
		s -> 
			Support = arena_zone:get_support_seals(SOwner, SOrder, SID),
			case [{SupPid, SupOrder, SupID}] -- Support of
				[] -> true;
				_ -> not_support_to_s
			end;
		_ -> undefined
	end.
%--------------------
skill_effect_check(SkillReceive, RequireSkill) ->	 
	if 
		RequireSkill =:= null -> true;
		RequireSkill =:= y ,  SkillReceive =/= [] -> true;
		RequireSkill =/= null, SkillReceive =:= [] -> no_skill_effect_receive
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
ability_effect_check(_, _) ->	true.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
mystic_paste_check(MysticPaste, RequireMystic, PlayerPid, OpponPid) ->	
	%[RequireMysticHead|Tail] = RequireMystic,
	if 
		RequireMystic =:= y, MysticPaste =:= [] -> no_mystic_paste;
		RequireMystic =:= y, MysticPaste =/= [] -> true;
		RequireMystic =/= y -> mystice_check(MysticPaste, RequireMystic,  PlayerPid, OpponPid)
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
mystice_check(MysticPaste, RequireMystic,  PlayerPid, OpponPid) -> 
	if
		MysticPaste =:= RequireMystic -> true;
		MysticPaste =/= RequireMystic -> 
			case RequireMystic of
				{type, Type} -> paste_type_check_loop(MysticPaste, Type);
				{owner, Owner} -> mystic_owner_check_loop(MysticPaste, Owner,  PlayerPid, OpponPid)
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
paste_type_check_loop([MysticCheck|Tail], Type) ->
	case paste_type_check(MysticCheck, Type) of
		true -> true;
		_ -> paste_type_check_loop(Tail, Type)
	end;
paste_type_check_loop([], _) -> false.
%------------------------------------------------------------------
paste_type_check(CardData, MysticType) ->
	{_, _, CardID} =  CardData,
	case mnesia_odbc:get_mystic_data(CardID, paste_type) of
		{ok, UsingType} -> 
			  if
				  UsingType =:= MysticType -> true;
				  true -> mystic_paste_type_mismatch
			  end;
		_ -> 
			mystic_paste_type_mismatch
	end.
%-----------------------------------------------------------------
mystic_type_check(CardData, MysticType) ->
	{_, _, CardID} =  CardData,
	case mnesia_odbc:get_mystic_data(CardID, card_type) of
		{ok, CardType} -> 
			  if
				  CardType =:= MysticType -> true;
				  true -> mystic_type_mismatch
			  end;
		_R ->
			mystic_type_mismatch
	end.		
%-----------------------------------------------------------------
mystic_subtype_check(CardData, ReqSubtype) ->
	{_, _, CardID} =  CardData,
	case mnesia_odbc:get_mystic_data(CardID, card_subtype) of
		{ok, Subtype} -> 
			  if
				  Subtype =:= ReqSubtype -> true;
				  true -> mystic_subtype_mismatch
			  end;
		_R -> 
			mystic_subtype_mismatch
	end.		
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
mystic_owner_check_loop([MysticCheck|Tail], OwnerRequire,  PlayerPid, OpponPid) ->
	case mystic_owner_check(MysticCheck, OwnerRequire,  PlayerPid, OpponPid) of
		true -> true;
		_ -> mystic_owner_check_loop(Tail, OwnerRequire,  PlayerPid, OpponPid) 
	end;
mystic_owner_check_loop([], _, _, _) -> [].
%------------------------------------------------------------------
mystic_owner_check(MysticCheck, OwnerRequire, PlayerPid, OpponPid) ->
	{OwnerPid, _, _} = MysticCheck,
	case OwnerRequire of
		owner -> 
			if 
				OwnerPid =:= PlayerPid -> true;
				true -> false
			end;
		opponent ->
			if
				OwnerPid =:= OpponPid -> true;
				true -> false
			end
	end.			
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
growth_check(CardGrowth, RequireGrowth) -> 
	if
%		RequireGrowth =:= y, CardGrowth =:= 1 -> true;
%		RequireGrowth =:= y, CardGrowth =/= 1 -> is_not_growth
		RequireGrowth =:= y, CardGrowth =:= 0 -> is_not_growth;
		RequireGrowth =:= y, CardGrowth =:= [] -> is_not_growth;
		true -> true
	end.	 
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
counter_check(Charging, RequireCharging) ->
	%smo_logger:fmsg("card counter is ~p and RequierChareging is ~p~n", [Charging, RequireCharging]),
	case RequireCharging of
		{less_than, Value} ->
			if
				Charging >= Value ->	false;
				Charging < Value -> true
			end;
		{more_than, Value} ->
			if
				Charging =< Value ->	false;
				Charging >Value -> true
			end;
		{more_or_equal, Value} ->
			if
				Value < Charging ->	false;
				Value >= Charging -> true
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
get_mystic_paste({CardZone, {CardOwner, CardOrder, CardID}}, {_OZone, {OtherPid, OtherOrder, OtherID}}, {ConditionNeed, Count}) ->
	MysticPaste = arena_zone:get_mystic_pasted(OtherPid, OtherOrder, OtherID),
	%smo_logger:fmsg("get_mystic_paste List of ~p,~p~n", [ConditionNeed, MysticPaste]),
	Mystic = function_utility:at_zone(arena_zone, MysticPaste),
	AList = check_all({CardZone, {CardOwner, CardOrder, CardID}}, ConditionNeed, Mystic),
	%smo_logger:fmsg("get_mystic_paste List of ~p,~p,~p~n", [ConditionNeed, MysticPaste, AList]),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
pasted_target({CardZone, {CardOwner, CardOrder, CardID}}, {ConditionNeed, Count}) ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to) of
		{ok, 0} -> false;
		{ok, PastedCard} ->
		%io:format("Pasted Target of {~p, ~p, ~p} is ~p~n", [CardOwner, CardOrder, CardID, PastedCard]),
		%smo_logger:fmsg("get_mystic_paste List of ~p,~p~n", [ConditionNeed, MysticPaste]),
			Pasted = function_utility:at_zone(arena_zone, PastedCard),
			AList = check_all({CardZone, {CardOwner, CardOrder, CardID}}, ConditionNeed, Pasted),
			%smo_logger:fmsg("get_mystic_paste List of ~p,~p,~p~n", [ConditionNeed, MysticPaste, AList]),
			check_count(AList, Count);
		_ -> false
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
check_card_condition({RequirePlayer, Zone, Card, Self}, {CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({OwnerPid, CardOrder, CardID}, PlayerOppID, RequirePlayer),
	OwnerArenaList = skill_card_list:player_zone_list({ReqPlayer, Zone, Card}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	CardList = 
	case Self of
		n -> OwnerArenaList -- [{OwnerPid, CardOrder, CardID}];
		_ -> OwnerArenaList
	end,
	%smo_logger:fmsg("s_owner_arena_include_check List of ~p,~p~n", [ConditionNeed, OwnerArenaList]),
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, CardList),
	%smo_logger:fmsg("s_owner_arena_include_check List of ~p,~p,~p~n", [ConditionNeed, OwnerArenaList, AList]),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
% s_owner_arena_include_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	% OwnerArenaList = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {OwnerPid, PlayerOppID}),
	% %smo_logger:fmsg("s_owner_arena_include_check List of ~p,~p~n", [ConditionNeed, OwnerArenaList]),
	% AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerArenaList),
	% %smo_logger:fmsg("s_owner_arena_include_check List of ~p,~p,~p~n", [ConditionNeed, OwnerArenaList, AList]),
	% check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
% s_owner_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	% OwnerArenaList1 = skill_card_list:player_zone_list({owner, [hand_cards], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	% OwnerArenaList = OwnerArenaList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	% AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerArenaList),
	% check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
% s_controller_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	% AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	% {ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),	
	% UnconHand = skill_card_list:player_zone_list({owner, [hand_cards], seal}, {ControllerPid, UncontrolPid}), % Return a list of cards of each player in require zone
	% AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, UnconHand),
	% check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
m_owner_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	OwnerHandList1 = skill_card_list:player_zone_list({owner, [hand_cards], mystic}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	OwnerHandList = OwnerHandList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerHandList),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
s_owner_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	OwnerArenaList1 = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	OwnerArenaList = OwnerArenaList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerArenaList),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_owner_shrine_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OwnerShrineList1 = skill_card_list:player_zone_list({owner, [shrine_cards], mystic}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%OwnerShrineList = OwnerShrineList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerShrineList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_owner_deck_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OwnerDeckList1 = skill_card_list:player_zone_list({owner, [mystic_deck], all}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%OwnerDeckList = OwnerDeckList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerDeckList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_controller_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	%case function_utility:is_contain([{curse, charm_curse}], AllFx) of
		%{ControllerPid, _, _} ->
			%PlayerPid = ControllerPid,
			%PlayerOppPid = mnesia_play:get_opponent_pid(PlayerPid);
		%_ -> 
			%PlayerPid = OwnerPid,
			%PlayerOppPid = PlayerOppID
	%end,
	%OwnerHandList1 = skill_card_list:player_zone_list({owner, [hand_cards], mystic}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	%OwnerHandList = OwnerHandList1--[{CardZone, {OwnerPid, CardOrder, CardID}}],
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OwnerHandList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%s_opp_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OppArList = skill_card_list:player_zone_list({opponent, [arena_zone], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OppArList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%s_opp_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OppHandList = skill_card_list:player_zone_list({opponent, [hand_cards], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OppHandList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_opp_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OppHandList = skill_card_list:player_zone_list({opponent, [hand_cards], mystic}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OppHandList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_opp_shrine_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OppShrineList = skill_card_list:player_zone_list({opponent, [shrine_card], mystic}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OppShrineList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
%m_opp_deck_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	%OppDeckList = skill_card_list:player_zone_list({opponent, [mystic_deck], all}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	%AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, OppDeckList),
	%check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, [Head|Tail])	->
	check_other:check_all_other_require_attribute({CardZone, {OwnerPid, CardOrder, CardID}}, Head, ConditionNeed)++check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, Tail);
check_all(_, _, []) -> [].
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
s_uncon_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	{ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),
	UnconAre = skill_card_list:player_zone_list({opponent, [arena_zone], seal}, {ControllerPid, UncontrolPid}), % Return a list of cards of each player in require zone
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, UnconAre),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
s_uncon_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	{ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),
	UnconHand = skill_card_list:player_zone_list({opponent, [hand_cards], seal}, {ControllerPid, UncontrolPid}), % Return a list of cards of each player in require zone
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, UnconHand),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
c_uncon_hand_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	{ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),
	UnconHand = skill_card_list:player_zone_list({opponent, [hand_cards], all}, {ControllerPid, UncontrolPid}), % Return a list of cards of each player in require zone
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, UnconHand),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
s_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	SealArList = skill_card_list:player_zone_list({null, [arena_zone], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, SealArList),
	%io:format('------------------------------------------------Card on Uncontroller hand ~p~n', [AList]),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
m_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	SealArList = skill_card_list:player_zone_list({null, [arena_zone], mystic}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, SealArList),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
other_s_arena_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, {ConditionNeed, Count}) ->
	SealArList = skill_card_list:player_zone_list({null, [arena_zone], seal}, {OwnerPid, PlayerOppID}), % Return a list of cards of each player in require zone
	ResultList = SealArList -- [{CardZone, {OwnerPid, CardOrder, CardID}}],
	AList = check_all({CardZone, {OwnerPid, CardOrder, CardID}}, ConditionNeed, ResultList),
	check_count(AList, Count).
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
check_count(AList, Count) ->
	case is_number(Count) of
		true ->
			if 
				length(AList) >= Count -> true; % not yet include delete [] function
				length(AList) < Count -> not_enough
			end;
		false ->
			case Count of
				{equal_to, X} ->
					if
						length(AList) =:= X -> true;
						true -> not_equal_to_X
					end;
				{less_than, X} ->
					if
						length(AList) >= X -> not_less_than_X;
						length(AList) < X -> true
					end;
				null -> true
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
have_ability({CardZone, {PlayerPid, CardOrder, CardID}}, Require) ->
	case Require of
		y ->
			CardIDList = do(qlc:q([X#card_ability.card_id ||X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
			case CardIDList of
				[] -> false;
				_ -> true
			end;
		n ->
			CardIDList = do(qlc:q([X#card_ability.card_id ||X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
			case CardIDList of
				[] -> false;
				_ -> true
			end;
		{cancel_curse, Need} ->
			AbilityId = do(qlc:q([X#card_ability.ability_id ||X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
			AbilityList = effect_retreive(AbilityId),
			case Need of
				[all] -> check_complete_have_ability({CardZone, {PlayerPid, CardOrder, CardID}}, AbilityList);
				_ ->
					DelCancelCurse = [{cancel_curse, Need}]--AbilityList,
					if
						DelCancelCurse =:= [] -> true;
						true -> false
					end
			end
	end.
%------------------------------------------------------------------	
effect_retreive([AbilityId|Tail]) -> 
	OwnerFx1 = do(qlc:q([X#card_ability.owner_effect ||X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])), 
	case OwnerFx1 of
		[] -> OwnerFx = [];
		_ -> 
			[OwnerFx] = OwnerFx1
	end,
	OtherFx1 = do(qlc:q([X#card_ability.other_effect ||X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	case OtherFx1 of
		[] -> OtherFx = [];
		_ -> 
			[OtherFx] = OtherFx1
	end,
	[{AbilityId, OwnerFx ++ OtherFx}] ++ effect_retreive(Tail);
effect_retreive([]) -> [].
%------------------------------------------------------------------
check_complete_have_ability(AbilityOwnCardData, [{AbilityId, Ability}|Tail]) -> 
	case have_cancel_curse(Ability) of
		true -> 
			[OwnerCheck] = do(qlc:q([X#card_ability.owner_check ||X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
			case [{growth, y}]--OwnerCheck of
				[] ->	growth_check(game_info:card_growth(AbilityOwnCardData), y);
				_ ->	true
			end;
		_ -> check_complete_have_ability(AbilityOwnCardData, Tail)
	end;
check_complete_have_ability(_, []) -> false.
%------------------------------------------------------------------
have_cancel_curse([{cancel_curse, _}|_]) -> true;
have_cancel_curse([_|Tail]) -> have_cancel_curse(Tail);
have_cancel_curse([]) -> false.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
have_skill(CardID, Require) -> 
	CardIDList = do(qlc:q([X#card_skill.card_id ||X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID])),
	case Require of
		y ->
			if
				CardIDList =:= [] -> false; %ต้องตรวจสอบต่อว่า ได้รับ skill จาก การ์ดใบอื่นหรือเปล่าด้วย
				true -> true
			end;
		n ->
			if
				CardIDList =/= []-> false; %ต้องตรวจสอบต่อว่า ได้รับ skill จาก การ์ดใบอื่นหรือเปล่าด้วย
				true -> true
			end
	end.
%=:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==:==
have_skill_ability(CardID, Require) -> 
	SkillIdList = do(qlc:q([X#card_skill.card_id ||X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID])),
	AbilityIdList = do(qlc:q([X#card_ability.card_id ||X <- mnesia:table(card_ability), X#card_ability.card_id =:= CardID])),
	case Require of
		y ->
			if
				SkillIdList =:= [],  AbilityIdList =:= []-> false;
				true -> true
			end;
		n ->
			if
				SkillIdList =:= [],  AbilityIdList =:= []-> true;
				true -> true
			end
	end.
%===============================================================================
check_protect_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, {TZone, {TargetPid, TargetOrder, TargetID}}, Value) ->
	AllFx = card_utility:get_all_card_effect(TargetPid, TargetOrder, TargetID, TZone),
	ProtectSkillContain = protect_skill_contain(AllFx),
	protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, ProtectSkillContain, Value).
%------------------------------------------------------------------
check_protect_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, {TZone, {TargetPid, TargetOrder, TargetID}}, Value) ->
	AllFx = card_utility:get_all_card_effect(TargetPid, TargetOrder, TargetID, TZone),
	ProtectAttackContain = protect_attack_contain(AllFx),
	protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, ProtectAttackContain, Value).

protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, [ProtectAttack|Tail], Value) ->
	case Value of
		y ->
			case ProtectAttack of
				{protect_attack, [all]} -> true;
				{protect_attack, [{elem, AnyElem}]} -> 
					case game_info:card_element({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyElem -> true;
						_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				{protect_attack, [{type, AnyType}]} ->
					case game_info:card_type({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyType -> true;
						_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end;
		n ->
			case ProtectAttack of
				{protect_attack, [all]} -> is_protect_attack;
				{protect_attack, [{elem, AnyElem}]} -> 
					case game_info:card_element({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyElem -> is_protect_attack_fo_this_element;
						_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				{protect_attack, [{type, AnyType}]} ->
					case game_info:card_type({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyType -> is_protect_attack_of_this_type;
						_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end;
		{n, Reject} ->
		 	case ProtectAttack of
				{protect_attack, Reject} -> false;
				_ -> protect_any_attack({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end			
	end;
protect_any_attack(_, [], Value) -> 
	case Value of
		y -> false;
		n -> true;
		{n, _} -> true
	end.
%------------------------------------------------------------------
protect_attack_contain([{_, CardFx, _}|Tail]) ->
	protect_attack(CardFx)++protect_attack_contain(Tail);
protect_attack_contain([]) -> [].

protect_attack([]) -> [];
protect_attack([{protect_skill, Any}|Tail]) -> [{protect_skill, Any}]++protect_attack(Tail);
protect_attack([_|Tail]) -> protect_attack(Tail).
%------------------------------------------------------------------
protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, [ProtectSkill|Tail], Value) ->
	case Value of
		y ->
			case ProtectSkill of
				{protect_skill, [all]} -> true;
				{protect_skill, [{elem, AnyElem}]} -> 
					case game_info:card_element({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyElem -> true;
						_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				{protect_skill, [{type, AnyType}]} ->
					case game_info:card_type({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyType -> true;
						_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end;
		n ->
			case ProtectSkill of
				{protect_skill, [all]} -> is_protect_skill;
				{protect_skill, [{elem, AnyElem}]} -> 
					case game_info:card_element({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyElem -> is_protect_skill_fo_this_element;
						_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				{protect_skill, [{type, AnyType}]} ->
					case game_info:card_type({OwnerZone, {PlayerPid, CardOrder, CardID}}) of
						AnyType -> is_protect_skill_of_this_type;
						_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
					end;
				_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end;
		{n, Reject} ->
		 	case ProtectSkill of
				{protect_skill, Reject} -> false;
				_ -> protect_any_skill({OwnerZone, {PlayerPid, CardOrder, CardID}}, Tail, Value)
			end			
	end;
protect_any_skill(_, [], Value) -> 
	case Value of
		y -> false;
		n -> true;
		{n, _} -> true
	end.
%------------------------------------------------------------------
protect_skill_contain([{_, CardFx, _}|Tail]) ->
	protect_skill(CardFx)++protect_skill_contain(Tail);
protect_skill_contain([]) -> [].

protect_skill([]) -> [];
protect_skill([{protect_skill, Any}|Tail]) -> [{protect_skill, Any}]++protect_skill(Tail);
protect_skill([_|Tail]) -> protect_skill(Tail).	
%------------------------------------------------------------------
use_skill_duration_contain([{CardGiveFx, Fx, {use_skill, CountRequire}}|_]) -> {CardGiveFx, Fx, {use_skill, CountRequire}};
use_skill_duration_contain([_|Tail]) -> use_skill_duration_contain(Tail);
use_skill_duration_contain([]) -> no_use_skill_duration.	
%------------------------------------------------------------------
check_controller({OwnerPid, CardOrder, CardID}, PlayerOppID, RequirePlayer) ->
	AllFx = card_utility:get_all_card_effect (OwnerPid, CardOrder, CardID),
	case RequirePlayer of
		controller ->
			{ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),
			ReqPlayer = owner;
		uncontrol ->
			{ControllerPid, UncontrolPid} = controller(OwnerPid, PlayerOppID, AllFx),
			ReqPlayer = opponent;
		_ -> 
			ControllerPid = OwnerPid,
			UncontrolPid = PlayerOppID,
			ReqPlayer = RequirePlayer
	end,
	{ControllerPid, UncontrolPid, ReqPlayer}.
%------------------------------------------------------------------
controller(OwnerPid, PlayerOppID, AllFx) ->
	case function_utility:is_contain([{curse, {charm_curse, OwnerPid}}, {curse, {charm_curse, PlayerOppID}}], AllFx) of
		[{curse, {charm_curse, Controller}}] ->	
			ControllerPid = Controller,
			UncontrolPid = mnesia_play:get_opponent_pid(Controller);
		_ -> 
			ControllerPid = OwnerPid,
			UncontrolPid = PlayerOppID
	end,
	{ControllerPid, UncontrolPid}.
%------------------------------------------------------------------
check_activate_ability({_, {PlayerPid, CardOrder, CardID}}, AbilityId, {XTime, Value}) ->
	AbilityNo = query_ability:no_of_ability(AbilityId),
	AllGFx = loop_check_activate_ability(Value),
	case length(check_loop({PlayerPid, CardOrder, CardID, AbilityNo}, AllGFx)) < XTime of
		true -> true;
		_ -> false
	end.

check_loop(_, []) -> [];
check_loop({PlayerPid, CardOrder, CardID, AbilityNo}, [{PlayerPid, CardOrder, CardID, AbilityNo}|AllGFx]) ->
	[{PlayerPid, CardOrder, CardID, AbilityNo}] ++ check_loop({PlayerPid, CardOrder, CardID, AbilityNo}, AllGFx);
check_loop({PlayerPid, CardOrder, CardID, AbilityNo}, [_|AllGFx]) -> check_loop({PlayerPid, CardOrder, CardID, AbilityNo}, AllGFx).
	
% ถ้่า 1 Subturn Value = 0, 1 Turn Value = 1, 3 Subturn Value = 2
loop_check_activate_ability(0) ->
	Subturn = get(subturn),
	case 	get({card_give_fx, Subturn}) of
		undefined -> [];
		AllGFx -> AllGFx
	end;
loop_check_activate_ability(Value) ->
	Subturn = get(subturn),
	GFx =
	case 	get({card_give_fx, Subturn - Value}) of
		undefined -> [];
		AllGFx -> AllGFx
	end,
	GFx ++ loop_check_activate_ability(Value - 1).
% -----------------------------------------------------------------	
move_to_line_check(_AbilityOwnCardData, OtherCardDataZone, Value) ->
	{_, {OOwner, OOrder, OId}} = OtherCardDataZone,
	CFx = card_utility:get_all_card_effect(OOwner, OOrder, OId),
	case function_utility:is_contain([{change_line, disallow}], CFx) of
		[] ->
			case Value of
				1 -> curse_check(game_info:card_curse(OtherCardDataZone), {n, freeze_curse});
				0 -> true;
				any -> curse_check(game_info:card_curse(OtherCardDataZone), {n, freeze_curse})
			end;
		_ -> cannot_change_line
	end.
%------------------------------------------------------------------
arena_count([], _Value) -> true;
arena_count([{CardOwner, CardOrder, CardID}|Card], Value) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	OwnerSeals = length(card_utility:get_all_card(CardOwner, seal_card, arena_zone)),
	OppSeals = length(card_utility:get_all_card(OpponentPid, seal_card, arena_zone)),
	case Value of
		{owner, seal, less} -> 
			if 
				OwnerSeals < OppSeals -> arena_count(Card, Value);
				true -> false
			end;
		{opponent, seal, less} ->
			if 
				OppSeals < OwnerSeals -> arena_count(Card, Value);
				true -> false
			end;
		{controller, seal, less} ->
			{ControllerPid, _UncontPid, _} = check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
			case ControllerPid of
				CardOwner  -> 
					if
						OwnerSeals < OppSeals -> arena_count(Card, Value);
						true -> false
					end;
				OpponentPid ->
					if 
						OppSeals < OwnerSeals -> arena_count(Card, Value);
						true -> false
					end
			end
	end.
%-----------------------------------------------------------------
arena_count({_, {CardOwner, CardOrder, CardID}}, OpponentPid, Value) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	OwnerSeals = length(card_utility:get_all_card(CardOwner, seal_card, arena_zone)),
	OppSeals = length(card_utility:get_all_card(OpponentPid, seal_card, arena_zone)),
	case Value of
		{owner, seal, less} -> 
			if 
				OwnerSeals < OppSeals -> true;
				true -> false
			end;
		{opponent, seal, less} ->
			if 
				OppSeals < OwnerSeals -> true;
				true -> false
			end;
		{controller, seal, less} ->
			{ControllerPid, _UncontPid, _} = check_controller({CardOwner, CardOrder, CardID}, OpponentPid, controller),
			case ControllerPid of
				CardOwner  -> 
					if
						OwnerSeals < OppSeals -> true;
						true -> false
					end;
				OpponentPid ->
					if 
						OppSeals < OwnerSeals -> true;
						true -> false
					end
			end
	end.
%------------------------------------------------------------------
hand_count({_, {PlayerPid, _CardOrder, _CardID}}, OpponentPid, Value) ->
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	OwnerSeals = length(card_utility:get_all_card (PlayerPid, seal_card, hand_cards)),
	OwnerMystics = length(card_utility:get_all_card (PlayerPid, mystic_card, hand_cards)),
	OppSeals = length(card_utility:get_all_card (OpponentPid, seal_card, hand_cards)),
	OppMystics = length(card_utility:get_all_card (OpponentPid, mystic_card, hand_cards)),
	case Value of
		{owner, all, less} -> 
			if 
				OwnerSeals + OwnerMystics < OppSeals + OppMystics -> true;
				true -> false
			end;
		{opponent, seal, less} ->
			if 
				OppSeals < OwnerSeals -> true;
				true -> false
			end
	end.
%----------------------------------------------------------------
paste_to_seal_check({OwnerZone, {PlayerPid, CardOrder, CardID}}, {CardZone, {TargetPid, TargetOrder, TargetID}}, _Value) ->
	% เช็คว่า Mystic นั้นมี กี่ ability 
	M_Ability = do(qlc:q( [X#mystic_ability.m_ability_number|| X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= TargetID])),
	%smo_logger:fmsg("start check paste to other ~p~n", [M_Ability]),
	start_check_target({CardZone, {TargetPid, TargetOrder, TargetID}}, M_Ability, _Value, {OwnerZone, {PlayerPid, CardOrder, CardID}}).
%-----------------------------------------------------------------
% เช็คว่า แต่ละ ability มีเป้าหมายหรือไม่
start_check_target(_, [], _, _) -> false;  % เช็คจนหมดทุก ability Number แล้ว ไม่พบเป้าหมายใหม่ แสดงว่า ....
start_check_target({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, [M_AbilityNo|M_Ability], PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}}) -> % check all possible conditions 
	AllAbilityID = do(qlc:q([X#mystic_ability.m_ability_id || X <- mnesia:table(mystic_ability), X#mystic_ability.card_id =:= MCardID, X#mystic_ability.m_ability_number =:= M_AbilityNo])),
	RemainAbilityID = new_mystic_check:check_to_check_condition_id(function_utility:qsort(AllAbilityID)),
	%io:format("card {~p, ~p, ~p} have remain id of ~p~n", [MPlayerOwnID, MCardOrder, MCardID, RemainAbilityID]),
	case	RemainAbilityID of
		[] -> false; % if that CardID have no ability 
		_ -> % if that CardID have ability no matter how many it have just start ability
			case m_ability_target_check({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, RemainAbilityID, PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}}) of
				% ถ้ามี Ability ใดที่ Return true ให้ หลุดออกไปเลย
				true -> true;
				_ -> start_check_target({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, M_Ability, PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}})
			end
	end.
%------------------------------------------------------------------
m_ability_target_check(_, [], _, _) -> false;
m_ability_target_check({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, [AbilityId|Ability], PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}}) ->	
	OtherTargetHaveEffect = do(qlc:q( [ X#mystic_ability.have_fx_to_target|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= AbilityId ])),
	case OtherTargetHaveEffect of
		[n] -> false;
		_ ->
			PlayerOppID = mnesia_play:get_opponent_pid(MPlayerOwnID),
			case target_got_effect({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, AbilityId, PlayerOppID) of
				% AbilityID นี้ไม่มีเป้่าให้ Mystic แปะได้
				[] -> m_ability_target_check({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, Ability, PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}});
				% มีเป้าให้แปะได้ ต้อง Check ต่อว่า
				Checked -> 
					%io:format("mystic {~p, ~p, ~p} can paste to ~p~n", [MPlayerOwnID, MCardOrder, MCardID, Checked]),
					case PasteTo of
						% ถ้าความสามารถบอกให้แปะที่เจ้าของ Ability 
						self ->
							case [{PlayerPid, CardOrder, CardID}] -- Checked of
								% แต่เป้่าที่เช็คออกมาไม่มีเจ้าของ Ability อยู่ให้ เช็ค AbilityId อื่นต่อ
								[] -> true;
								_ -> m_ability_target_check({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, Ability, PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}})
							end;
						other ->
							case card_utility:get_card_option_field(MPlayerOwnID, MCardOrder, MCardID, paste_to, arena_zone) of
								{ok, [{SPid, SOrder, Sid}]} -> 
									case Checked -- [{SPid, SOrder, Sid}] of
										[] -> m_ability_target_check({MCardZone, {MPlayerOwnID, MCardOrder, MCardID}}, Ability, PasteTo, {OwnerZone, {PlayerPid, CardOrder, CardID}});
										_ -> true
									end;
								_ -> true
							end
					end
			end
	end.
	
target_got_effect({CardZone, {CardOwner, CardOrder, CardID}}, MAbilityID, OpponentPid) ->
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_ability.target_card_owner,
																																						  X#mystic_ability.target_card_zone_check, 
																																						  X#mystic_ability.target_card_type
																																						 } || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	AllCard = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {CardOwner, OpponentPid}), % Return a list of cards of each player in require zone
	%smo_logger:fmsg("card on <<~p>> of ##~p## are @~p@}", [RequirePresentZone, RequirePlayer, PlayerZoneList1]),
	[ThisInclude] = do(qlc:q([X#mystic_ability.target_include_this || X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	CardCheck =
	case ThisInclude of
		n -> AllCard -- [{CardZone, {CardOwner, CardOrder, CardID}}];
		_ -> AllCard
 	end,
	[Condition] = do(qlc:q([X#mystic_ability.target_condition|| X <- mnesia:table(mystic_ability), X#mystic_ability.m_ability_id =:= MAbilityID])),
	function_utility:card_match_condition({CardZone, {CardOwner, CardOrder, CardID}}, CardCheck, Condition -- [{action, {n, card_casting}}]).
	
check_flag(CardAction, Flag) ->
	case Flag of
		{n, FlagRequire} ->
			case [FlagRequire]--CardAction of
				[] -> false;
				_ -> true
			end;
		_ ->
			case [Flag]--CardAction of
				[] -> true;
				_ -> false
			end
	end.
