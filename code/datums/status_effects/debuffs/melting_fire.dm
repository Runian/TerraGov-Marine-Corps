// ***************************************
// *********** Melting fire
// ***************************************
/datum/status_effect/stacking/melting_fire
	id = "melting_fire"
	tick_interval = 2 SECONDS
	stacks = 1
	stack_decay = 1
	max_stacks = 10
	consumed_on_threshold = FALSE
	/// Pyrogen creator of the debuff.
	var/mob/living/carbon/xenomorph/pyrogen/debuff_creator
	/// Used for the fire effect.
	var/obj/vis_melt_fire/visual_fire
	// The percentage of brute/burn healing that should be negated.
	var/healing_debuff = 0

/datum/status_effect/stacking/melting_fire/on_creation(mob/living/new_owner, stacks_to_apply, atom/new_creator)
	. = ..()
	if(!.)
		return
	visual_fire = new
	visual_fire.icon_state = "melting_low_stacks"
	owner.vis_contents += visual_fire
	owner.balloon_alert(owner, "melting fire!")
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 30)
	RegisterSignal(owner, COMSIG_LIVING_DO_RESIST, PROC_REF(call_resist_debuff))
	set_creator(new_creator)

/datum/status_effect/stacking/melting_fire/on_remove()
	if(visual_fire)
		owner.vis_contents -= visual_fire
		QDEL_NULL(visual_fire)
	set_creator(null)
	return ..()

/datum/status_effect/stacking/melting_fire/tick(seconds_between_ticks)
	. = ..()
	if(QDELETED(src)) // Parent may delete us (no stacks, etc).
		return
	visual_fire.icon_state = (stacks > 4) ? "melting_high_stacks" : "melting_low_stacks"
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 4, FALSE, ADJACENT_SOUND_RANGE)
	owner.take_overall_damage(PYROGEN_DAMAGE_PER_STACK * stacks, BURN, FIRE, updating_health = TRUE)

	if(QDELETED(debuff_creator) || debuff_creator.stat == DEAD)
		return
	var/amount_to_heal = 2 // HEAL_XENO_DAMAGE requires it as a variable.
	HEAL_XENO_DAMAGE(debuff_creator, amount_to_heal, FALSE)
	debuff_creator.gain_plasma(5, TRUE)

/datum/status_effect/stacking/melting_fire/can_have_status()
	if((owner.status_flags & GODMODE) || owner.soft_armor?.getRating(FIRE) >= 100)
		return FALSE
	return ..() // Parent already checks if they're dead.

/datum/status_effect/stacking/melting_fire/can_gain_stacks()
	return can_have_status()

/datum/status_effect/stacking/melting_fire/add_stacks(stacks_added, atom/xeno_creator)
	. = ..()
	if(!.)
		return
	set_creator(xeno_creator)

/// Sets the debuff creator of this status effect. Sets and (un)registers signals regarding healing reduction accordingly.
/datum/status_effect/stacking/melting_fire/proc/set_creator(atom/xeno_creator)
	if(!xeno_creator || !isxenopyrogen(xeno_creator))
		if(healing_debuff)
			UnregisterSignal(owner, list(COMSIG_HUMAN_BRUTE_DAMAGE, COMSIG_HUMAN_BURN_DAMAGE))
			healing_debuff = 0
		debuff_creator = null
		return
	var/mob/living/carbon/xenomorph/pyrogen/new_creator = xeno_creator
	if(healing_debuff && !new_creator.melting_fire_healing_reduction)
		UnregisterSignal(owner, list(COMSIG_HUMAN_BRUTE_DAMAGE, COMSIG_HUMAN_BURN_DAMAGE))
	if(!healing_debuff && new_creator.melting_fire_healing_reduction)
		RegisterSignals(owner, list(COMSIG_HUMAN_BRUTE_DAMAGE, COMSIG_HUMAN_BURN_DAMAGE), PROC_REF(on_heal))
	debuff_creator = new_creator
	healing_debuff = new_creator.melting_fire_healing_reduction

/// Called when the debuff's owner uses the Resist action for this debuff.
/datum/status_effect/stacking/melting_fire/proc/call_resist_debuff(datum/source)
	SIGNAL_HANDLER
	INVOKE_ASYNC(src, PROC_REF(resist_debuff)) // grilled cheese sandwich

/// Resisting the debuff will allow the debuff's owner to remove some stacks from themselves.
/datum/status_effect/stacking/melting_fire/proc/resist_debuff()
	if(!owner)
		if(!QDELETED(src))
			qdel(src)
		return
	if(length(owner.do_actions))
		return
	owner.spin(30, 1.5)
	owner.Paralyze(3 SECONDS)
	if((stacks - PYROGEN_MELTING_FIRE_STACKS_PER_RESIST) > 0)
		owner.visible_message(span_danger("[owner] rolls on the floor, trying to put themselves out!"), \
		span_notice("You stop, drop, and roll!"), null, 5)
	else
		owner.visible_message(span_danger("[owner] has successfully extinguished themselves!"), \
		span_notice("You extinguish yourself."), null, 5)
	add_stacks(-PYROGEN_MELTING_FIRE_STACKS_PER_RESIST) // If their stacks hit zero, it is qdel'd right here.

// If the owner of this status effect were to heal, a percentage of that healing will be negated.
/datum/status_effect/stacking/melting_fire/proc/on_heal(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(amount >= 0 || !healing_debuff)
		return
	amount_mod += floor(amount) * healing_debuff

/obj/vis_melt_fire
	name = "ouch ouch ouch"
	icon = 'icons/mob/OnFire.dmi'
	layer = ABOVE_MOB_LAYER
	vis_flags = VIS_INHERIT_DIR | VIS_INHERIT_ID | VIS_INHERIT_PLANE
