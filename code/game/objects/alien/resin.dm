/obj/alien/resin
	name = "resin"
	desc = "Looks like some kind of slimy growth."
	icon_state = "Resin1"
	max_integrity = 200
	resistance_flags = XENO_DAMAGEABLE|UNACIDABLE

/obj/alien/resin/attack_hand(mob/living/user)
	balloon_alert(user, "need a weapon!")
	return TRUE

/obj/alien/resin/sticky
	name = STICKY_RESIN
	desc = "A layer of disgusting sticky slime."
	icon_state = "sticky"
	density = FALSE
	opacity = FALSE
	max_integrity = 36
	plane = FLOOR_PLANE
	layer = ABOVE_WEEDS_LAYER
	hit_sound = SFX_ALIEN_RESIN_MOVE
	ignore_weed_destruction = TRUE
	/// Slows non-xenos by this much when they cross us.
	var/slow_amount = 8
	/// Does this refund build points when destroyed?
	var/refundable = TRUE

/obj/alien/resin/sticky/Initialize(mapload)
	. = ..()
	var/static/list/connections = list(
		COMSIG_ATOM_ENTERED = PROC_REF(on_loc_entered)
	)
	AddElement(/datum/element/connect_loc, connections)

/obj/alien/resin/sticky/on_loc_entered(datum/source, atom/movable/crosser)
	slow_down_crosser(crosser, slow_amount)

/obj/alien/resin/sticky/attack_alien(mob/living/carbon/xenomorph/xeno_attacker, damage_amount = xeno_attacker.xeno_caste.melee_damage, damage_type = BRUTE, armor_type = MELEE, effects = TRUE, armor_penetration = xeno_attacker.xeno_caste.melee_ap, isrightclick = FALSE)
	if(xeno_attacker.status_flags & INCORPOREAL)
		return FALSE

	if(xeno_attacker.a_intent == INTENT_HARM) // Clear it out on hit; no need to double tap.
		if(CHECK_BITFIELD(SSticker.mode?.round_type_flags, MODE_ALLOW_XENO_QUICKBUILD) && SSresinshaping.active && refundable)
			SSresinshaping.quickbuild_points_by_hive[xeno_attacker.hivenumber]++
		xeno_attacker.do_attack_animation(src, ATTACK_EFFECT_CLAW) //SFX
		playsound(src, SFX_ALIEN_RESIN_BREAK, 25, FALSE, MEDIUM_SOUND_RANGE) //SFX
		deconstruct(TRUE)
		return TRUE

	return ..()

/obj/alien/resin/sticky/can_z_move(direction, turf/start, turf/destination, z_move_flags, mob/living/rider)
	z_move_flags |= ZMOVE_ALLOW_ANCHORED
	return ..()

/obj/alien/resin/sticky/onZImpact(turf/impacted_turf, levels, impact_flags = NONE)
	impact_flags |= ZIMPACT_NO_SPIN
	return ..()

// Hivelord's Sticky Resin spit uses this.
/obj/alien/resin/sticky/thin
	name = "thin sticky resin"
	desc = "A thin layer of disgusting sticky slime."
	max_integrity = 6
	ignore_weed_destruction = FALSE
	slow_amount = 4
	refundable = FALSE
