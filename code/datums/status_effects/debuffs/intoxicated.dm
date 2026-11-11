/datum/status_effect/stacking/intoxicated
	id = "intoxicated"
	tick_interval = 2 SECONDS
	stacks = 1
	max_stacks = 30
	consumed_on_threshold = FALSE
	/// The xenomorph who will receive healing.
	var/mob/living/carbon/xenomorph/xenomorph_to_heal
	/// The amount of health to restore for each stack.
	var/healing_per_stack = 0
	/// Used for particles. Holds the particles instead of the mob. See particle_holder for documentation.
	var/obj/effect/abstract/particle_holder/particle_holder

/datum/status_effect/stacking/intoxicated/on_creation(mob/living/new_owner, stacks_to_apply, mob/living/carbon/xenomorph/expected_xenomorph_to_heal, expected_healing_per_stack)
	if(expected_xenomorph_to_heal)
		xenomorph_to_heal = expected_xenomorph_to_heal
	if(expected_healing_per_stack)
		healing_per_stack = expected_healing_per_stack
	return ..()

/datum/status_effect/stacking/intoxicated/on_apply()
	if(new_owner.status_flags & GODMODE || new_owner.stat == DEAD)
		return FALSE
	. = ..()
	if(!.)
		return
	RegisterSignal(owner, COMSIG_LIVING_DO_RESIST, PROC_REF(call_resist_debuff))
	owner.balloon_alert(owner, "intoxicated!")
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 30)
	particle_holder = new(owner, /particles/toxic_slash)
	particle_holder.particles.spawning = 1 + round(stacks / 2)
	particle_holder.pixel_x = -2
	particle_holder.pixel_y = 0
	if(HAS_TRAIT(owner, TRAIT_INTOXICATION_RESISTANT) || (owner.get_soft_armor(BIO) >= 65))
		stack_decay = 2

/datum/status_effect/stacking/intoxicated/on_remove()
	UnregisterSignal(owner, COMSIG_LIVING_DO_RESIST)
	xenomorph_to_heal = null
	QDEL_NULL(particle_holder)
	return ..()

/datum/status_effect/stacking/intoxicated/can_have_status()
	if((owner.status_flags & GODMODE) || HAS_TRAIT(owner, TRAIT_INTOXICATION_IMMUNE))
		return FALSE
	return ..()

/datum/status_effect/stacking/intoxicated/can_gain_stacks()
	return can_have_status()

/datum/status_effect/stacking/intoxicated/tick(seconds_between_ticks)
	. = ..()
	if(HAS_TRAIT(owner, TRAIT_INTOXICATION_RESISTANT) || (owner.get_soft_armor(BIO) > 65))
		stack_decay = 2
	var/debuff_damage = SENTINEL_INTOXICATED_BASE_DAMAGE + round(stacks / 10)
	owner.adjustFireLoss(debuff_damage)
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 4, FALSE, ADJACENT_SOUND_RANGE)
	particle_holder.particles.spawning = 1 + round(stacks / 2)
	if(stacks >= 20)
		owner.adjust_slowdown(1)
		owner.adjust_stagger(1 SECONDS)
	if(healing_per_stack && xenomorph_to_heal?.Adjacent(owner))
		var/amount_to_heal = stacks * healing_per_stack
		HEAL_XENO_DAMAGE(xenomorph_to_heal, amount_to_heal, FALSE)

/// Called when the debuff's owner uses the Resist action for this debuff.
/datum/status_effect/stacking/intoxicated/proc/call_resist_debuff()
	SIGNAL_HANDLER
	INVOKE_ASYNC(src, PROC_REF(resist_debuff)) // grilled cheese sandwich

/// Resisting the debuff will allow the debuff's owner to remove some stacks from themselves.
/datum/status_effect/stacking/intoxicated/proc/resist_debuff()
	if(!owner)
		return
	if(length(owner.do_actions))
		return
	if(!do_after(owner, 5 SECONDS, NONE, owner, BUSY_ICON_GENERIC))
		owner?.balloon_alert(owner, "interrupted!")
		return
	if(QDELETED(src))
		return
	playsound(owner, 'sound/effects/slosh.ogg', 30)
	stacks -= SENTINEL_INTOXICATED_RESIST_REDUCTION
	if(stacks > 0)
		resist_debuff() // We repeat ourselves as long as the debuff persists.
