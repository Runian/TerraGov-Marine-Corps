
/*
* effect/alien
*/
/obj/alien
	name = "alien thing"
	desc = "theres something alien about this"
	icon = 'icons/Xeno/Effects.dmi'
	hit_sound = SFX_ALIEN_RESIN_BREAK
	anchored = TRUE
	max_integrity = 1
	resistance_flags = UNACIDABLE
	obj_flags = CAN_BE_HIT
	var/on_fire = FALSE
	/// Should this object be automatically destroyed if any weeds below it are destroyed?
	var/delete_on_weed_destruction = TRUE

/obj/alien/Initialize(mapload)
	. = ..()
	if(!ignore_weed_destruction)
		RegisterSignal(loc, COMSIG_TURF_WEED_REMOVED, PROC_REF(weed_removed))

/obj/alien/attackby(obj/item/attacking_item, mob/user, params)
	. = ..()
	if(.)
		return

	if(user.a_intent == INTENT_HARM) // Harm intent already handled by parent.
		return

	if(!(obj_flags & CAN_BE_HIT))
		return

	return attacking_item.attack_obj(src, user)

/obj/alien/fire_act(burn_level)
	take_damage(burn_level * 2, BURN, FIRE)

/obj/alien/ex_act(severity)
	switch(severity)
		if(EXPLODE_DEVASTATE)
			take_damage(500, BRUTE, BOMB)
		if(EXPLODE_HEAVY)
			take_damage((rand(140, 300)), BRUTE, BOMB)
		if(EXPLODE_LIGHT)
			take_damage((rand(50, 100)), BRUTE, BOMB)
		if(EXPLODE_WEAK)
			take_damage(rand(25, 50), BRUTE, BOMB)

/obj/alien/effect_smoke(obj/effect/particle_effect/smoke/S)
	. = ..()
	if(!.)
		return
	if(CHECK_BITFIELD(S.smoke_traits, SMOKE_BLISTERING))
		take_damage(rand(2, 20) * 0.1, BURN, ACID)

/// Destroys us when the weed we're were ontop of has been deleted.
/obj/alien/proc/weed_removed()
	SIGNAL_HANDLER
	// This is using [obj_destruction()] instead of [deconstruct()] since it is overridden by certain objects to carry out their destruction effects (acid wells & hugger/carrier traps).
	obj_destruction(damage_flag = "melee")

/// Special behavior for COMSIG_ATOM_ENTERED, if registered, to handle when an atom enters our loc.
/obj/alien/proc/on_loc_entered(datum/source, atom/movable/arrived, atom/old_loc, list/atom/old_locs)
	SIGNAL_HANDLER
	return

/// If possible, will slows down an non-xeno's next move by a certain amount.
/obj/alien/proc/slow_down_crosser(atom/movable/crosser, slow_amount = 1)
	if(isxeno(crosser))
		return
	if(crosser.throwing || crosser.buckled)
		return
	if(crosser.pass_flags & PASS_LOW_STRUCTURE)
		return
	// We don't want to slowdown the crosser if there is something on top of the resin (e.g. a table).
	for(var/obj/object in orange(0, crosser))
		if(!object.density)
			continue
		if(object == crosser)
			continue
		return

	if(ismecha(crosser))
		var/obj/vehicle/sealed/vehicle = crosser
		COOLDOWN_INCREMENT(vehicle, cooldown_vehicle_move, slow_amount)
		return
	if(!ishuman(crosser))
		return
	var/mob/living/carbon/human/victim = crosser
	if(victim.lying_angle)
		return
	victim.next_move_slowdown += slow_amount
