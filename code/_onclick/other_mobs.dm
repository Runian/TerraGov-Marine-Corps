/*
	Animals & All Unspecified
*/
/mob/living/UnarmedAttack(atom/A, has_proximity, modifiers)
	if(HAS_TRAIT(src, TRAIT_HANDS_BLOCKED))
		return
	A.attack_animal(src)


/atom/proc/attack_animal(mob/user as mob)
	return


/**
 * Called when a human attacks us with an empty hand via LEFT CLICK.
 *
 * Arguments:
 * * mob/living/carbon/human/user - The human that touched us.
 *
 * Returns TRUE if the attack has fully resolved.
 */
/atom/proc/attack_hand(mob/living/user)
	. = FALSE
	if(QDELETED(src))
		stack_trace("attack_hand on a qdeleted atom")
		return TRUE
	add_fingerprint(user, "attack_hand")
	if(SEND_SIGNAL(src, COMSIG_ATOM_ATTACK_HAND, user) & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE

/atom/movable/attack_hand(mob/living/user)
	. = ..()
	if(.)
		return

	if(status_flags & INCORPOREAL || user.status_flags & INCORPOREAL) //We can't physically attack or be attacked by the incorporeal
		return FALSE

	if(buckle_flags & CAN_BUCKLE)
		switch(LAZYLEN(buckled_mobs))
			if(0)
				return
			if(1)
				if(user_unbuckle_mob(buckled_mobs[1], user))
					return TRUE
			else
				var/unbuckled = tgui_input_list(user, "Who do you wish to unbuckle?", "Unbuckle Who?", sortNames(buckled_mobs))
				if(!unbuckled)
					return
				if(user_unbuckle_mob(unbuckled, user))
					return TRUE

/obj/structure/bed/attack_hand(mob/living/user)
	. = ..()
	if(.)
		return
	if(buckled_bodybag)
		unbuckle_bodybag()
		return TRUE

/**
 * Called when a human attacks us with an empty hand via RIGHT CLICK.
 *
 * Arguments:
 * * mob/living/carbon/human/user - The human that touched us.
 *
 * Returns TRUE if the attack has fully resolved.
 */

/atom/proc/attack_hand_secondary(mob/living/carbon/human/user)
	. = FALSE
	if(QDELETED(src))
		stack_trace("attack_hand_secondary on a qdeleted atom")
		return TRUE
	add_fingerprint(user, "attack_hand_secondary")
	if(SEND_SIGNAL(src, COMSIG_ATOM_ATTACK_HAND_SECONDARY, user) & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE


/*
	New Players:
	Have no reason to click on anything at all.
*/
/mob/new_player/Click()
	return TRUE
