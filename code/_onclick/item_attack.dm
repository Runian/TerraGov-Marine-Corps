/**
 * ### Implementation Details
 *
 * [obj/item/proc/base_item_interaction] handles all non-combat interactions of an item with an atom.
 * It calls [atom/proc/tool_act], among others.
 *
 * [obj/item/proc/pre_attack] is called before an attack is made, before we even check for block.
 * Use pre_attack to cancel attacks early based on conditions ("You can't attack with this unless you're a pacifist!")
 *
 * After pre-attack, we go into [atom/proc/attackby] which is where individual atoms handle what happens when being hit.
 * Mobs (/mob/living) -> [obj/item/proc/attack].
 * Turfs (/turf) -> [obj/item/proc/attack_turf].
 * Objects (/obj) -> [obj/item/proc/attack_obj].
 *
 * You GENERALLY should never override [obj/item/proc/attack]; use NOBLUDGEON to ensure parent can be called with no issue if overridden.
 * All on-attack effects should dealt with in [obj/item/proc/afterattack] which should handle any on-attack effects or special item.
 * e.g. stuff like "my stick stuns when it hits people".
 *
 * Regardless, we go BACK to the atom to call [atom/proc/attacked_by], which is where the damage is actually applied.
 * Finally we call [obj/item/proc/afterattack] after damage is dealt, to handle special item interacts after an attack occurs.
 * Use afterattack for stuff like "My stick stuns when it hits people".
 *
 * Arguments
 * * mob/living/user - The mob using src on something
 * * atom/target - The atom being clicked on with src
 * * list/modifiers - click params such as alt/shift and x/y, etc
 */

/obj/item/proc/melee_attack_chain(mob/user, atom/target, list/modifiers)
	var/item_interact_result = target.base_item_interaction(user, src, modifiers)
	if(item_interact_result & ITEM_INTERACT_SUCCESS)
		return TRUE
	if(item_interact_result & ITEM_INTERACT_BLOCKING)
		return FALSE

	var/is_right_clicking = LAZYACCESS(modifiers, RIGHT_CLICK)
	var/is_left_clicking = !is_right_clicking

	// At this point, it means we're not doing a non-combat interaction so let's just try to bash it.
	var/pre_attack_result
	if(is_left_clicking)
		pre_attack_result = pre_attack(target, user, modifiers)
	else
		switch(pre_attack_secondary(target, user, modifiers))
			if(SECONDARY_ATTACK_CALL_NORMAL)
				pre_attack_result = pre_attack(target, user, modifiers)
			if(SECONDARY_ATTACK_CANCEL_ATTACK_CHAIN)
				return TRUE
			if(SECONDARY_ATTACK_CONTINUE_CHAIN)
				EMPTY_BLOCK_GUARD // Normal behavior
			else
				CRASH("pre_attack_secondary must return an SECONDARY_ATTACK_* define, please consult code/__DEFINES/combat.dm")
	if(pre_attack_result)
		return TRUE

	// At this point, the attack is really about to happen.
	if(user.lying_angle) // TODO: Is this really needed?
		user.balloon_alert(user, "can't while prone!")
		return

	var/attackby_result
	if(is_left_clicking)
		attackby_result = target.attackby(src, user, modifiers)
	else
		switch(target.attackby_secondary(src, user, modifiers))
			if(SECONDARY_ATTACK_CALL_NORMAL)
				attackby_result = target.attackby(src, user, modifiers)
			if(SECONDARY_ATTACK_CANCEL_ATTACK_CHAIN)
				return TRUE
			if(SECONDARY_ATTACK_CONTINUE_CHAIN)
				EMPTY_BLOCK_GUARD // Normal behavior
			else
				CRASH("attackby_secondary must return an SECONDARY_ATTACK_* define, please consult code/__DEFINES/combat.dm")
	if(attackby_result)
		return TRUE
	if(QDELETED(target) || QDELETED(src))
		return TRUE

	SEND_SIGNAL(src, COMSIG_ITEM_AFTERATTACK, target, user, modifiers)
	SEND_SIGNAL(target, COMSIG_ATOM_AFTER_ATTACKEDBY, src, user, modifiers)
	afterattack(target, user, modifiers)

	return TRUE

/// Called when the item is in the active hand is LEFT CLICKED.
/obj/item/proc/attack_self(mob/user)
	SHOULD_CALL_PARENT(FALSE)// When possible, parent should be called - but some may need to completely override us.
	add_fingerprint(user, "attack_self")
	if(SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_SELF, user) & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE

	// TODO: This likely need to be moved elsewhere?
	if(item_flags & ITEM_ACTIVATABLE)
		toggle_active(!active, user)
		return
	if(!can_interact(user))
		return
	interact(user)

/// Called when the item is in the active hand is RIGHT CLICKED.
/obj/item/proc/attack_self_secondary(mob/user)
	SHOULD_CALL_PARENT(TRUE)
	add_fingerprint(user, "attack_self_secondary")
	if(SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_SELF_SECONDARY, user) & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	return FALSE

/**
 * Called on the item before it hits something, via LEFT CLICK.
 *
 * Arguments:
 * * atom/target - The atom about to be hit.
 * * mob/living/user - The mob doing the htting.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack chain should end here.
 *
 * See: [/obj/item/proc/melee_attack_chain]
 */
/obj/item/proc/pre_attack(atom/target, mob/living/user, list/modifiers)
	var/signal_return = SEND_SIGNAL(src, COMSIG_ITEM_PRE_ATTACK, target, user, modifiers) | SEND_SIGNAL(user, COMSIG_USER_PRE_ITEM_ATTACK, src, target, modifiers)
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	return FALSE

/**
 * Called on the item before it hits something via RIGHT CLICK.
 *
 * Arguments:
 * * atom/target - The atom about to be hit.
 * * mob/living/user - The mob doing the htting.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack chain should end here.
 *
 * See: [/obj/item/proc/melee_attack_chain]
 */
/obj/item/proc/pre_attack_secondary(atom/target, mob/living/user, list/modifiers)
	var/signal_result = SEND_SIGNAL(src, COMSIG_ITEM_PRE_ATTACK_SECONDARY, target, user, modifiers) | SEND_SIGNAL(user, COMSIG_USER_PRE_ITEM_ATTACK_SECONDARY, src, target, modifiers)
	if(signal_result & COMPONENT_SECONDARY_CANCEL_ATTACK_CHAIN)
		return SECONDARY_ATTACK_CANCEL_ATTACK_CHAIN
	if(signal_result & COMPONENT_SECONDARY_CONTINUE_ATTACK_CHAIN)
		return SECONDARY_ATTACK_CONTINUE_CHAIN
	return SECONDARY_ATTACK_CALL_NORMAL

/**
 * Called on an atom is getting attacked by an item via LEFT CLICK.
 *
 * Arguments:
 * * obj/item/attacking_item - The item hitting this atom.
 * * mob/user - The mob doing the hitting.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack chain should end here.
 *
 * See: [/obj/item/proc/melee_attack_chain]
 */

/atom/proc/attackby(obj/item/attacking_item, mob/user, list/modifiers)
	SIGNAL_HANDLER_DOES_SLEEP // TODO: Someone should really get rid of this.
	add_fingerprint(user, "attackby", attacking_item)
	var/signal_return = SEND_SIGNAL(src, COMSIG_ATOM_ATTACKBY, attacking_item, user, modifiers)
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	if(signal_return & COMPONENT_CONTINUE_ATTACK_CHAIN)
		return FALSE
	return FALSE

/mob/living/attackby(obj/item/attacking_item, mob/user, list/modifiers)
	. = ..()
	if(.)
		return TRUE
	user.changeNext_move(attacking_item.attack_speed)
	return attacking_item.attack(src, user, modifiers)

/turf/attackby(obj/item/attacking_item, mob/user, list/modifiers)
	. = ..()
	if(.)
		return TRUE
	return attacking_item.attack_turf(src, user, modifiers)

/obj/attackby(obj/item/attacking_item, mob/user, list/modifiers)
	. = ..()
	if(.)
		return TRUE
	if(!(obj_flags & CAN_BE_HIT))
		return FALSE
	return attacking_item.attack_obj(src, user, modifiers)

/obj/item/grab/attackby(obj/item/attacking_item, mob/user, list/modifiers)
	. = ..()
	if(.)
		return TRUE
	if(grab_interact(attacking_item, user))
		user.changeNext_move(GRAB_SLAM_DELAY)
		return TRUE

/**
 * Called on an atom is getting attacked by an item via RIGHT CLICK.
 *
 * Arguments:
 * * obj/item/attacking_item - The item hitting this atom.
 * * mob/user - The mob doing the hitting.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack chain should end here.
 *
 * See: [/obj/item/proc/melee_attack_chain]
 */
/atom/proc/attackby_secondary(obj/item/attacking_item, mob/user, list/modifiers)
	add_fingerprint(user, "attackby_secondary", attacking_item)
	if(SEND_SIGNAL(src, COMSIG_ATOM_ATTACKBY_SECONDARY, attacking_item, user, modifiers) & COMPONENT_NO_AFTERATTACK)
		return TRUE
	return FALSE

/mob/living/attackby_secondary(obj/item/attacking_item, mob/living/user, params)
	. = ..()
	if(.)
		return TRUE
	user.changeNext_move(attacking_item.attack_speed_alternate)
	return attacking_item.attack_secondary(src, user)

/**
 * Called on an item that is being used to attack a living mob via LEFT CLICK.
 *
 * Arguments:
 * * mob/living/target_mob - The living mob getting attacked.
 * * mob/living/user - The living mob that is attacking.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack has fully resolved / if the attack chain should end here.
 */
/obj/item/proc/attack(mob/living/target_mob, mob/user, list/modifiers)
	var/signal_return = SEND_SIGNAL(src, COMSIG_ITEM_ATTACK, target_mob, user, modifiers) || SEND_SIGNAL(user, COMSIG_MOB_ITEM_ATTACK, target_mob, src, modifiers)
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	if(signal_return & COMPONENT_CONTINUE_ATTACK_CHAIN)
		return FALSE
	if(item_flags & NOBLUDGEON)
		return FALSE
	if(!force)
		return FALSE
	if(target_mob.status_flags & INCORPOREAL || user.status_flags & INCORPOREAL)
		return FALSE
	if(target_mob.can_be_operated_on() && do_surgery(target_mob, user, src)) // Checks if mob is lying down on table for surgery.
		return TRUE
	if(!target_mob.attacked_by(src, user, modifiers))
		return TRUE
	if(hitsound)
		playsound(loc, hitsound, 25, TRUE, MEDIUM_SOUND_RANGE)

/**
 * Called on an item that is being used to attack a living mob via RIGHT CLICK.
 *
 * Arguments:
 * * mob/living/target_mob - The living mob getting attacked.
 * * mob/living/user - The living mob that is attacking.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack has been fully handled / failed to connect.
 */
/obj/item/proc/attack_secondary(mob/living/target_mob, mob/living/user, list/modifiers)
	var/signal_result = SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_SECONDARY, target_mob, user, modifiers)
	if(signal_result & COMPONENT_SECONDARY_CANCEL_ATTACK_CHAIN)
		return SECONDARY_ATTACK_CANCEL_ATTACK_CHAIN
	if(signal_result & COMPONENT_SECONDARY_CONTINUE_ATTACK_CHAIN)
		return SECONDARY_ATTACK_CONTINUE_CHAIN
	return SECONDARY_ATTACK_CALL_NORMAL

/**
 * The equivalent of the standard version of [/obj/item/proc/attack] but for non-mob targets.
 * Called on an item that is being used to attack an atom via RIGHT CLICK.
 *
 * Arguments:
 * * atom/target_atom - The atom getting attacked.
 * * mob/living/user - The living mob that is attacking.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack has been fully handled / failed to connect.
 */

/obj/item/proc/attack_atom(atom/target_atom, mob/living/user, list/modifiers)
	var/signal_return = SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_ATOM, target_atom, user, modifiers) | SEND_SIGNAL(user, COMSIG_LIVING_ATTACK_ATOM, target_atom, src, modifiers)
	add_fingerprint(user, "attack_atom")
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	if(signal_return & COMPONENT_CONTINUE_ATTACK_CHAIN)
		return FALSE
	if(item_flags & NOBLUDGEON)
		return TRUE
	user.changeNext_move(attack_speed)
	if(!target_atom.attacked_by(src, user, modifiers))
		return TRUE
	return FALSE

/**
 * The equivalent of the standard version of [/obj/item/proc/attack] but for object targets.
 * Called on an item that is being used to attack an obj.
 *
 * Arguments:
 * * obj/target_object - The object getting attacked.
 * * mob/living/user - The living mob that is attacking.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack has been fully handled, failed to connect, or to prevent any afterattack.
 */
/obj/item/proc/attack_obj(obj/target_object, mob/living/user, modifiers)
	var/signal_return =  SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_OBJ, target_object, user, modifiers)
	add_fingerprint(user, "attack_obj")
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	if(signal_return & COMPONENT_CONTINUE_ATTACK_CHAIN)
		return FALSE
	if(item_flags & NOBLUDGEON)
		return TRUE
	user.changeNext_move(attack_speed)
	if(!target_object.attacked_by(src, user, modifiers))
		return TRUE
	return FALSE

/**
 * The equivalent of the standard version of [/obj/item/proc/attack] but for turf targets.
 * Called on an item that is being used to attack a turf.
 *
 * Arguments:
 * * turf/target_turf - The turf getting attacked.
 * * mob/living/user - The living mob that is attacking.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack has been fully handled, failed to connect, or to prevent any afterattack.
 */
/obj/item/proc/attack_turf(turf/target_turf, mob/living/user, modifiers)
	var/signal_return = NONE
	//var/signal_return = SEND_SIGNAL(src, COMSIG_ITEM_ATTACK_TURF, target_turf, user, modifiers)
	add_fingerprint(user, "attack_turf")
	if(signal_return & COMPONENT_CANCEL_ATTACK_CHAIN)
		return TRUE
	if(signal_return & COMPONENT_CONTINUE_ATTACK_CHAIN)
		return FALSE
	return FALSE

/**
 * Deals with the default/regular attack effects by an item.
 *
 * Arguments:
 * * obj/item/attacking_item - The item hitting this atom.
 * * mob/user - The mob doing the hitting.
 * * list/modifiers - The click modifiers, e.g. alt, shift.
 *
 * Returns TRUE if the attack successfully connected / has been processed.
 *
 * See: [/obj/item/proc/attack], [/obj/item/proc/attack_obj], etc.
 */
/atom/proc/attacked_by(obj/item/attacking_item, mob/living/user, list/modifiers)
	return FALSE

/mob/living/attacked_by(obj/item/attacking_item, mob/living/user, def_zone)
	. = ..()
	if(.)
		return TRUE

	var/message_verb = "attacks"
	if(LAZYLEN(attacking_item.attack_verb))
		message_verb = pick(attacking_item.attack_verb)
	var/message_hit_area
	if(def_zone)
		message_hit_area = " in the [def_zone]"
	var/attack_message = "[src] is [message_verb][message_hit_area] with [attacking_item]!"
	var/attack_message_local = "You're [message_verb][message_hit_area] with [attacking_item]!"
	if(user in viewers(src, null))
		attack_message = "[user] [message_verb] [src][message_hit_area] with [attacking_item]!"
		attack_message_local = "[user] [message_verb] you[message_hit_area] with [attacking_item]!"

	user.changeNext_move(attacking_item.attack_speed)
	user.do_attack_animation(src, used_item = attacking_item)

	var/power = attacking_item.force + round(attacking_item.force * MELEE_SKILL_DAM_BUFF * user.skills.getRating(SKILL_MELEE_WEAPONS))

	switch(attacking_item.damtype)
		if(BRUTE)
			apply_damage(power, BRUTE, user.zone_selected, MELEE, attacking_item.sharp, attacking_item.edge, FALSE, attacking_item.penetration, user)
			if(is_sharp(attacking_item))
				new /obj/effect/temp_visual/dir_setting/bloodsplatter(loc, Get_Angle(user, src), get_blood_color())
		if(BURN)
			if(apply_damage(power, BURN, user.zone_selected, FIRE, attacking_item.sharp, attacking_item.edge, FALSE, attacking_item.penetration, user))
				attack_message_local = "[attack_message_local] It burns!"
		if(STAMINA)
			apply_damage(power, STAMINA, user.zone_selected, MELEE, attacking_item.sharp, attacking_item.edge, FALSE, attacking_item.penetration, user)

	visible_message(span_danger("[attack_message]"),
		span_userdanger("[attack_message_local]"), null, COMBAT_MESSAGE_RANGE)

	UPDATEHEALTH(src)

	record_melee_damage(user, power)
	log_combat(user, src, "attacked", attacking_item, "(INTENT: [uppertext(user.a_intent)]) (DAMTYE: [uppertext(attacking_item.damtype)]) (RAW DMG: [power])")
	if(power && !user.mind?.bypass_ff && !mind?.bypass_ff && user.faction == faction)
		var/turf/T = get_turf(src)
		user.ff_check(power, src)
		log_ffattack("[key_name(user)] attacked [key_name(src)] with \the [attacking_item] in [AREACOORD(T)] (RAW DMG: [power]).")
		msg_admin_ff("[ADMIN_TPMONTY(user)] attacked [ADMIN_TPMONTY(src)] with \the [attacking_item] in [ADMIN_VERBOSEJMP(T)] (RAW DMG: [power]).")

	return TRUE

/obj/attacked_by(obj/item/attacking_item, mob/living/user, list/modifiers)
	. = ..()
	if(.)
		return TRUE

	user.changeNext_move(attacking_item.attack_speed)
	user.do_attack_animation(src, used_item = attacking_item)

	user.visible_message(span_warning("[user] hits [src] with [attacking_item]!"),
		span_warning("You hit [src] with [attacking_item]!"), visible_message_flags = COMBAT_MESSAGE)
	log_combat(user, src, "attacked", attacking_item)
	var/power = attacking_item.force + round(attacking_item.force * MELEE_SKILL_DAM_BUFF * user.skills.getRating(SKILL_MELEE_WEAPONS))
	take_damage(power, attacking_item.damtype, MELEE, blame_mob = user)
	return TRUE

// has_proximity is TRUE if this afterattack was called on something adjacent, in your square, or on your person.
// Click parameters is the params string from byond Click() code, see that documentation.
/obj/item/proc/afterattack(atom/target, mob/user, has_proximity, click_parameters)
	SEND_SIGNAL(src, COMSIG_ITEM_AFTERATTACK, target, user, has_proximity, click_parameters)
	SEND_SIGNAL(user, COMSIG_MOB_ITEM_AFTERATTACK, target, user, has_proximity, click_parameters)
	return

/**
 * This proc is called when an atom gets attacked (clicked on) by a powerloader clamp
 *
 * Arguments:
 * * user: The mob doing the attacking
 * * attached_clamp: The clamp used by the mob for attacking
 */

/turf/powerloader_act(mob/living/user, obj/item/powerloader_clamp/attached_clamp)
	. = ..()
	if(.)
		return TRUE
	if(!attached_clamp.loaded || density) // No picking up turfs.
		return FALSE
	. = TRUE

	for(var/i in contents)
		var/atom/movable/blocky_stuff = i
		if(!blocky_stuff.density)
			continue
		to_chat(user, span_warning("You can't drop [attached_clamp.loaded] here, [blocky_stuff] blocks the way."))
		return
	if(attached_clamp.loaded.bound_height > 32)
		var/turf/next_turf = get_step(src, NORTH)
		if(next_turf.density)
			to_chat(user, span_warning("You can't drop [attached_clamp.loaded] here, something blocks the way."))
			return
		for(var/i in next_turf.contents)
			var/atom/movable/blocky_stuff = i
			if(!blocky_stuff.density)
				continue
			to_chat(user, span_warning("You can't drop [attached_clamp.loaded] here, [blocky_stuff] blocks the way."))
			return

	if(attached_clamp.loaded.bound_width > 32)
		var/turf/next_turf = get_step(src, EAST)
		if(next_turf.density)
			to_chat(user, span_warning("You can't drop [attached_clamp.loaded] here, something blocks the way."))
			return
		for(var/i in next_turf.contents)
			var/atom/movable/blocky_stuff = i
			if(!blocky_stuff.density)
				continue
			to_chat(user, span_warning("You can't drop [attached_clamp.loaded] here, [blocky_stuff] blocks the way."))
			return

	user.visible_message(span_notice("[user] drops [attached_clamp.loaded] onto [src]."),
	span_notice("You drop [attached_clamp.loaded] onto [src]."))
	attached_clamp.loaded.forceMove(src)
	attached_clamp.loaded = null
	playsound(src, 'sound/machines/hydraulics_1.ogg', 40, 1)
	attached_clamp.update_icon()

/obj/powerloader_act(mob/living/user, obj/item/powerloader_clamp/attached_clamp)
	. = ..()
	if(.)
		return TRUE
	. = TRUE
	if(attached_clamp.loaded)
		return
	if(!CHECK_BITFIELD(interaction_flags, INTERACT_POWERLOADER_PICKUP_ALLOWED) && !CHECK_BITFIELD(interaction_flags, INTERACT_POWERLOADER_PICKUP_ALLOWED_BYPASS_ANCHOR))
		to_chat(user, span_notice("[attached_clamp.linked_powerloader] cannot pick up [src]!"))
		return
	if(anchored && !CHECK_BITFIELD(interaction_flags, INTERACT_POWERLOADER_PICKUP_ALLOWED_BYPASS_ANCHOR))
		to_chat(user, span_notice("[src] is bolted to the ground."))
		return
	attached_clamp.do_load(src)
