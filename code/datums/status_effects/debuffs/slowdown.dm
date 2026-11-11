// Slowdowns the owner by an amount. The amount determines both the duration (via stacks) and potency of the slowdown.
/datum/status_effect/stacking/slowdown
	id = "slowdown"
	tick_interval = 2 SECONDS
	remove_on_fullheal = TRUE
	consumed_on_threshold = FALSE

/datum/status_effect/stacking/slowdown/on_apply()
	. = ..()
	if(!.)
		return
	stack_decay = owner.get_slowdown_regen()

/datum/status_effect/stacking/slowdown/on_remove()
	owner.remove_movespeed_modifier(MOVESPEED_ID_STAGGERSTUN)
	return ..()

/datum/status_effect/stacking/slowdown/can_have_status()
	if((owner.status_flags & GODMODE) || HAS_TRAIT(owner, TRAIT_SLOWDOWNIMMUNE))
		return FALSE
	return ..()

/datum/status_effect/stacking/slowdown/can_gain_stacks()
	return can_have_status()

/datum/status_effect/stacking/slowdown/add_stacks(stacks_added)
	. = ..()
	if(!.)
		return
	owner.add_movespeed_modifier(MOVESPEED_ID_STAGGERSTUN, TRUE, 0, NONE, TRUE, stacks) // Updates modifier.

/// Returns the slowdown multiplier for this mob type.
/mob/living/proc/get_slowdown_regen()
	return STANDARD_SLOWDOWN_REGEN

/mob/living/carbon/xenomorph/get_slowdown_regen()
	return XENO_SLOWDOWN_REGEN

//////////////////
// STATUS PROCS //
//////////////////

/// Returns the slowdown status effect, if they have it.
/// Otherwise, returns null.
/mob/living/proc/is_slowed()
	return has_status_effect(STATUS_EFFECT_SLOWDOWN)

/// Returns the remaining stacks of their slowdown status effect, if they have it.
/// Otherwise, returns 0.
/mob/living/proc/amount_slowed()
	var/datum/status_effect/stacking/slowdown/current_slow = is_slowed()
	return current_slow ? current_slow.stacks : 0

/// Applies slowdown up to the provided amount, but never greater than it.
/// Returns the status effect, if it exists.
/mob/living/proc/slowdown(amount)
	if(amount <= 0)
		CRASH("slowdown() called on a mob with an invalid amount.") // Wrong proc. Use [/proc/adjust_slowdown()] instead.
	if((status_flags & GODMODE) || stat == DEAD || HAS_TRAIT(src, TRAIT_SLOWDOWNIMMUNE))
		return

	amount *= get_slowdown_regen()

	var/datum/status_effect/stacking/slowdown/current_slow = is_slowed()
	if(!current_slow)
		return apply_status_effect(STATUS_EFFECT_SLOWDOWN, amount)

	if(current_slow.stacks >= amount)
		return current_slow

	current_slow.add_stacks(amount - current_slow.stacks)
	return QDELETED(current_slow) ? null : current_slow

/// Sets slowdown to a specific amount.
/// Returns the status effect, if it exists.
/mob/living/proc/set_slowdown(amount)
	if((status_flags & GODMODE) || stat == DEAD || HAS_TRAIT(src, TRAIT_SLOWDOWNIMMUNE))
		return

	amount *= get_slowdown_regen()

	var/datum/status_effect/stacking/slowdown/current_slow = is_slowed()
	if(!current_slow && amount > 0)
		return apply_status_effect(STATUS_EFFECT_SLOWDOWN, amount)

	current_slow.add_stacks(amount - current_slow.stacks) // Will delete itself if it reaches zero.
	return QDELETED(current_slow) ? null : current_slow

/// Applies (or removes) slowdown by the provided amount.
/// Returns the status effect, if it exists.
/mob/living/proc/adjust_slowdown(amount)
	if((status_flags & GODMODE) || stat == DEAD || HAS_TRAIT(src, TRAIT_SLOWDOWNIMMUNE))
		return

	amount *= get_slowdown_regen()

	var/datum/status_effect/stacking/slowdown/current_slow = is_slowed()
	if(!current_slow)
		return apply_status_effect(STATUS_EFFECT_SLOWDOWN, amount)

	current_slow.add_stacks(amount)
	return QDELETED(current_slow) ? null : current_slow
