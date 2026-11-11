#define BASE_HEAL_RATE -0.0125

//Largely negative status effects go here, even if they have small benificial effects
//STUN EFFECTS
/datum/status_effect/incapacitating
	tick_interval = STATUS_EFFECT_NO_TICK
	status_type = STATUS_EFFECT_REPLACE
	alert_type = null

/datum/status_effect/incapacitating/on_creation(mob/living/new_owner, set_duration)
	if(new_owner.status_flags & GODMODE)
		qdel(src)
		return
	if(isnum(set_duration))
		duration = set_duration
	return ..()

//STAGGERED
/datum/status_effect/incapacitating/stagger
	id = "stagger"

/datum/status_effect/incapacitating/stagger/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_STAGGERED, TRAIT_STATUS_EFFECT(id))
	owner.adjust_mob_scatter(5)

/datum/status_effect/incapacitating/stagger/on_remove()
	REMOVE_TRAIT(owner, TRAIT_STAGGERED, TRAIT_STATUS_EFFECT(id))
	owner.adjust_mob_scatter(-5)

//STUN
/datum/status_effect/incapacitating/stun
	id = "stun"

/datum/status_effect/incapacitating/stun/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	ADD_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/stun/on_remove()
	REMOVE_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))
	return ..()

//KNOCKDOWN
/datum/status_effect/incapacitating/knockdown
	id = "knockdown"

/datum/status_effect/incapacitating/knockdown/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	ADD_TRAIT(owner, TRAIT_FLOORED, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/knockdown/on_remove()
	REMOVE_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	REMOVE_TRAIT(owner, TRAIT_FLOORED, TRAIT_STATUS_EFFECT(id))
	return ..()

//IMMOBILIZED
/datum/status_effect/incapacitating/immobilized
	id = "immobilized"

/datum/status_effect/incapacitating/immobilized/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/immobilized/on_remove()
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))
	return ..()

//PARALYZED
/datum/status_effect/incapacitating/paralyzed
	id = "paralyzed"

/datum/status_effect/incapacitating/paralyzed/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	ADD_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))
	ADD_TRAIT(owner, TRAIT_FLOORED, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/paralyzed/on_remove()
	REMOVE_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))
	REMOVE_TRAIT(owner, TRAIT_FLOORED, TRAIT_STATUS_EFFECT(id))
	return ..()

//UNCONSCIOUS
/datum/status_effect/incapacitating/unconscious
	id = "unconscious"

/datum/status_effect/incapacitating/unconscious/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/unconscious/on_remove()
	REMOVE_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))
	return ..()

/datum/status_effect/incapacitating/unconscious/tick(seconds_between_ticks)
	if(owner.getStaminaLoss())
		owner.adjustStaminaLoss(-0.3) //reduce stamina loss by 0.3 per tick, 6 per 2 seconds

//SLEEPING
/datum/status_effect/incapacitating/sleeping
	id = "sleeping"
	alert_type = /atom/movable/screen/alert/status_effect/asleep
	var/mob/living/carbon/carbon_owner
	var/mob/living/carbon/human/human_owner

/datum/status_effect/incapacitating/sleeping/on_creation(mob/living/new_owner)
	. = ..()
	if(.)
		if(iscarbon(owner)) //to avoid repeated istypes
			carbon_owner = owner
		if(ishuman(owner))
			human_owner = owner

/datum/status_effect/incapacitating/sleeping/Destroy()
	carbon_owner = null
	human_owner = null
	return ..()

/datum/status_effect/incapacitating/sleeping/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/sleeping/on_remove()
	REMOVE_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))
	return ..()

/datum/status_effect/incapacitating/sleeping/tick(seconds_between_ticks)
	if(!owner.maxHealth)
		return
	var/health_ratio = owner.health / owner.maxHealth
	var/healing = BASE_HEAL_RATE //set for a base of 0.25 healed per 2-second interval asleep in a bed with covers.
	if((locate(/obj/structure/bed) in owner.loc))
		healing += (2 * BASE_HEAL_RATE)
	else if((locate(/obj/structure/table) in owner.loc))
		healing += BASE_HEAL_RATE
	if(locate(/obj/item/bedsheet) in owner.loc)
		healing += BASE_HEAL_RATE
		if((locate(/obj/item/toy/plush) in owner.loc)) // plushie bonus in bed with a blanket
			healing += 0.75 * BASE_HEAL_RATE // plushie bonus in bed with a blanket
	if(health_ratio > -0.5)
		owner.adjustBruteLoss(healing)
		owner.adjustFireLoss(healing)
		owner.adjustToxLoss(healing * 0.5, TRUE, TRUE)
		owner.adjustStaminaLoss(healing * 100)
		owner.adjustCloneLoss(healing * health_ratio * 0.8)
	if(human_owner?.is_drunk())
		human_owner.adjust_drunkenness(-human_owner.amount_drunkenness() * 0.03) //reduce drunkenness by 0.3% per tick, 6% per 2 seconds
	if(prob(20))
		if(carbon_owner)
			carbon_owner.handle_dreams()
		if(prob(10) && owner.health > owner.get_crit_threshold())
			owner.emote("snore")

///Basically a temporary self-inflicted shutdown for maintenance
/datum/status_effect/incapacitating/repair_mode
	id = "repairing"
	tick_interval = 1 SECONDS
	///How much brute or burn per second
	var/healing_per_tick = 4
	///Whether the last tick made a sound effect or not
	var/last_sound

/datum/status_effect/incapacitating/repair_mode/on_apply()
	. = ..()
	if(!.)
		return
	//Robots and synths are generally resistant to blinding, so we apply an overlay directly instead
	owner.overlay_fullscreen("repair-mode", /atom/movable/screen/fullscreen/blind)
	ADD_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	ADD_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/repair_mode/on_remove()
	owner.clear_fullscreen("repair-mode")
	REMOVE_TRAIT(owner, TRAIT_INCAPACITATED, TRAIT_STATUS_EFFECT(id))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, TRAIT_STATUS_EFFECT(id))
	return ..()

/datum/status_effect/incapacitating/repair_mode/tick(seconds_between_ticks)
	var/sound_to_play
	if(owner.getBruteLoss())
		owner.heal_limb_damage(healing_per_tick, 0, TRUE, TRUE)
		sound_to_play = 'sound/effects/robotrepair.ogg'
	else if(owner.getFireLoss())
		owner.heal_limb_damage(0, healing_per_tick, TRUE, TRUE)
		sound_to_play = 'sound/effects/robotrepair2.ogg'
	if(!sound_to_play || last_sound)
		last_sound = FALSE
		return
	last_sound = TRUE
	playsound(owner, sound_to_play, 50)



/**
 * Adjusts a timed status effect on the mob,taking into account any existing timed status effects.
 * This can be any status effect that takes into account "duration" with their initialize arguments.
 *
 * Positive durations will add deciseconds to the duration of existing status effects
 * or apply a new status effect of that duration to the mob.
 *
 * Negative durations will remove deciseconds from the duration of an existing version of the status effect,
 * removing the status effect entirely if the duration becomes less than zero (less than the current world time).
 *
 * duration - the duration, in deciseconds, to add or remove from the effect
 * effect - the type of status effect being adjusted on the mob
 * max_duration - optional - if set, positive durations will only be added UP TO the passed max duration
 */
/mob/living/proc/adjust_timed_status_effect(duration, effect, max_duration)
	if(!isnum(duration))
		CRASH("adjust_timed_status_effect: called with an invalid duration. (Got: [duration])")

	if(!ispath(effect, /datum/status_effect))
		CRASH("adjust_timed_status_effect: called with an invalid effect type. (Got: [effect])")

	// If we have a max duration set, we need to check our duration does not exceed it
	if(isnum(max_duration))
		if(max_duration <= 0)
			CRASH("adjust_timed_status_effect: Called with an invalid max_duration. (Got: [max_duration])")

		if(duration >= max_duration)
			duration = max_duration

	var/datum/status_effect/existing = has_status_effect(effect)
	if(existing)
		if(isnum(max_duration) && duration > 0)
			// Check the duration remaining on the existing status effect
			// If it's greater than / equal to our passed max duration, we don't need to do anything
			var/remaining_duration = existing.duration - world.time
			if(remaining_duration >= max_duration)
				return

			// Otherwise, add duration up to the max (max_duration - remaining_duration),
			// or just add duration if it doesn't exceed our max at all
			existing.duration += min(max_duration - remaining_duration, duration)

		else
			existing.duration += duration

		// If the duration was decreased and is now less 0 seconds,
		// qdel it / clean up the status effect immediately
		// (rather than waiting for the process tick to handle it)
		if(existing.duration <= world.time)
			qdel(existing)

	else if(duration > 0)
		apply_status_effect(effect, duration)

/**
 * Sets a timed status effect of some kind on a mob to a specific value.
 * If only_if_higher is TRUE, it will only set the value up to the passed duration,
 * so any pre-existing status effects of the same type won't be reduced down
 *
 * duration - the duration, in deciseconds, of the effect. 0 or lower will either remove the current effect or do nothing if none are present
 * effect - the type of status effect given to the mob
 * only_if_higher - if TRUE, we will only set the effect to the new duration if the new duration is longer than any existing duration
 */
/mob/living/proc/set_timed_status_effect(duration, effect, only_if_higher = FALSE)
	if(!isnum(duration))
		CRASH("set_timed_status_effect: called with an invalid duration. (Got: [duration])")

	if(!ispath(effect, /datum/status_effect))
		CRASH("set_timed_status_effect: called with an invalid effect type. (Got: [effect])")

	var/datum/status_effect/existing = has_status_effect(effect)
	if(existing)
		// set_timed_status_effect to 0 technically acts as a way to clear effects,
		// though remove_status_effect would achieve the same goal more explicitly.
		if(duration <= 0)
			qdel(existing)
			return

		if(only_if_higher)
			// If the existing status effect has a higher remaining duration
			// than what we aim to set it to, don't downgrade it - do nothing (return)
			var/remaining_duration = existing.duration - world.time
			if(remaining_duration >= duration)
				return

		// Set the duration accordingly
		existing.duration = world.time + duration

	else if(duration > 0)
		apply_status_effect(effect, duration)

/atom/movable/screen/alert/status_effect/asleep
	name = "Asleep"
	desc = "You've fallen asleep. Wait a bit and you should wake up. Unless you don't, considering how helpless you are."
	icon_state = "asleep"

//ADMIN SLEEP
/datum/status_effect/incapacitating/adminsleep
	id = "adminsleep"
	alert_type = /atom/movable/screen/alert/status_effect/adminsleep
	duration = -1

/datum/status_effect/incapacitating/adminsleep/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/incapacitating/adminsleep/on_remove()
	REMOVE_TRAIT(owner, TRAIT_KNOCKEDOUT, TRAIT_STATUS_EFFECT(id))
	return ..()

/atom/movable/screen/alert/status_effect/adminsleep
	name = "Admin Slept"
	desc = "You've been slept by an Admin."
	icon_state = "asleep"

//CONFUSED
/datum/status_effect/incapacitating/confused
	id = "confused"
	alert_type = /atom/movable/screen/alert/status_effect/confused

/atom/movable/screen/alert/status_effect/confused
	name = "Confused"
	desc = "You're dazed and confused."
	icon_state = "asleep"

/datum/status_effect/incapacitating/harvester_slowdown
	id = "harvest_slow"
	tick_interval = 1 SECONDS
	status_type = STATUS_EFFECT_REPLACE
	var/debuff_slowdown = 2

/datum/status_effect/incapacitating/harvester_slowdown/on_apply()
	. = ..()
	if(!.)
		return
	if(HAS_TRAIT(owner, TRAIT_SLOWDOWNIMMUNE))
		return
	owner.add_movespeed_modifier(MOVESPEED_ID_HARVEST_TRAM_SLOWDOWN, TRUE, 0, NONE, TRUE, debuff_slowdown)

/datum/status_effect/incapacitating/harvester_slowdown/on_remove()
	owner.remove_movespeed_modifier(MOVESPEED_ID_HARVEST_TRAM_SLOWDOWN)
	return ..()

///irradiated mob
/datum/status_effect/incapacitating/irradiated
	id = "irradiated"
	status_type = STATUS_EFFECT_REFRESH
	tick_interval = 2 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/irradiated
	///Some effects only apply to carbons
	var/mob/living/carbon/carbon_owner

/datum/status_effect/incapacitating/irradiated/on_creation(mob/living/new_owner, set_duration)
	if(new_owner.status_flags & GODMODE || new_owner.stat == DEAD)
		qdel(src)
		return
	. = ..()
	if(.)
		if(iscarbon(owner))
			carbon_owner = owner

/datum/status_effect/incapacitating/irradiated/Destroy()
	carbon_owner = null
	return ..()

/datum/status_effect/incapacitating/irradiated/tick(seconds_between_ticks)
	var/mob/living/living_owner = owner
	//Roulette of bad things
	if(prob(15))
		living_owner.adjustCloneLoss(2)
		to_chat(living_owner, span_warning("You feel like you're burning from the inside!"))
	else
		living_owner.adjustToxLoss(3)
	if(prob(15))
		living_owner.adjust_Losebreath(5)
	if(prob(15))
		living_owner.vomit()
	if(carbon_owner && prob(15))
		var/datum/internal_organ/organ = pick(carbon_owner.internal_organs)
		if(organ)
			organ.take_damage(5)

/atom/movable/screen/alert/status_effect/irradiated
	name = "Irradiated"
	desc = "You've been irradiated! The effects of the radiation will continue to harm you until purged from your system."
	icon_state = "radiation"

// ***************************************
// *********** Dancer Tagged
// ***************************************
/datum/status_effect/incapacitating/dancer_tagged
	id = "dancer_tagged"
	duration = 15 SECONDS

// ***************************************
// *********** Lifedrain
// ***************************************
/datum/status_effect/incapacitating/lifedrain
	id = "life_drain"
	duration = 10 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/lifedrain

/atom/movable/screen/alert/status_effect/lifedrain
	name = "Lifedrain"
	desc = "Your life force transfers to xenos when they slash you!"
	icon_state = "skullemoji"

#undef BASE_HEAL_RATE
