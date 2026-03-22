//*********************//
//        Shell        //
//*********************//

/datum/mutation_upgrade/defense/armor_surge
	name = "Armor Surge"
	desc = "Drain Surge's duration is reduced to 4 seconds, but it grants a total of 50 bonus armor while it is active."

/datum/mutation_upgrade/defense/armor_surge/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_strength = 50
	ability.drain_surge_duration = 4 SECONDS

/datum/mutation_upgrade/defense/armor_surge/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_strength = initial(ability.drain_surge_strength)
	ability.drain_surge_duration = initial(ability.drain_surge_duration)

/datum/mutation_upgrade/defense/healing_surge
	name = "Healing Surge"
	desc = "Drain Surge heals over time. Every second, it heals equal to the amount of bonus armor it gives."

/datum/mutation_upgrade/defense/healing_surge/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_heals_overtime = TRUE

/datum/mutation_upgrade/defense/healing_surge/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_heals_overtime = initial(ability.drain_surge_heals_overtime)

/datum/mutation_upgrade/offense/toxic_blood
	name = "Toxic Blood"
	desc = "For every 20 points of damage taken, you apply a stack of Intoxicated to humans adjacent to you."
	/// The amount of damage taken so far.
	var/accumulated_damage = 0

/datum/mutation_upgrade/offense/toxic_blood/on_gain()
	RegisterSignals(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE), PROC_REF(on_damage_taken))
	return ..()

/datum/mutation_upgrade/offense/toxic_blood/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE))
	return ..()

/// Applies intoxicated stacks to nearby alive humans whenever there is enough accumulated damage.
/datum/mutation_upgrade/offense/toxic_blood/proc/on_damage_taken(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(amount <= 0 || xenomorph_owner.stat == DEAD) // It is fine to be unconscious!
		return
	accumulated_damage += amount
	if(accumulated_damage < 20)
		return
	var/intoxicated_stacks_to_apply = floor(accumulated_damage / 20)
	accumulated_damage -= intoxicated_stacks_to_apply * 20
	for (var/mob/living/carbon/human/nearby_human AS in cheap_get_humans_near(xenomorph_owner, 1))
		if(nearby_human.stat == DEAD)
			continue
		var/datum/status_effect/stacking/intoxicated/debuff = nearby_human.has_status_effect(STATUS_EFFECT_INTOXICATED)
		if(!debuff)
			nearby_human.apply_status_effect(STATUS_EFFECT_INTOXICATED, intoxicated_stacks_to_apply)
			continue
		debuff.add_stacks(intoxicated_stacks_to_apply)

/datum/mutation_upgrade/offense/toxic_claws
	name = "Toxic Claws"
	desc = "You no longer have access to Toxic Slash. While you have at least 30 plasma, slashing a human will consume that amount of plasma and apply 5 Intoxicated stacks."

/datum/mutation_upgrade/offense/toxic_claws/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack))
	var/datum/action/ability/xeno_action/toxic_slash/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/toxic_slash]
	if(ability)
		ability.remove_action(xenomorph_owner)

/datum/mutation_upgrade/offense/toxic_claws/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING)
	var/datum/action/ability/xeno_action/toxic_slash/ability = new()
	ability.give_action(xenomorph_owner)

/datum/mutation_upgrade/offense/toxic_claws/on_xenomorph_upgrade()
	var/datum/action/ability/xeno_action/toxic_slash/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/toxic_slash]
	if(ability)
		ability.remove_action(xenomorph_owner)

/// Applies a variable amount of Intoxicated stacks to those that they attack.
/datum/mutation_upgrade/offense/toxic_claws/proc/on_postattack(mob/living/source, mob/living/target, damage)
	SIGNAL_HANDLER
	if(xenomorph_owner.plasma_stored < 30)
		return
	xenomorph_owner.use_plasma(30)
	var/datum/status_effect/stacking/intoxicated/debuff = target.has_status_effect(STATUS_EFFECT_INTOXICATED)
	if(!debuff)
		target.apply_status_effect(STATUS_EFFECT_INTOXICATED, 5)
		return
	debuff.add_stacks(5)

/datum/mutation_upgrade/offense/claw_surge
	name = "Claw Surge"
	desc = "Drain Surge no longer gives armor, but applies a buff. The buff increases your melee damage by 1% for every point of armor Drain Surge would of given."

/datum/mutation_upgrade/offense/claw_surge/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_modifier_instead = TRUE

/datum/mutation_upgrade/offense/claw_surge/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_modifier_instead = initial(ability.drain_surge_modifier_instead)

/datum/mutation_upgrade/utility/toxic_compatibility
	name = "Toxic Compatibility"
	desc = "Drain Sting's potency is increased by the amount of xeno-affiliated reagents in your target. Each unit counts as a third of an Intoxicated stack."

/datum/mutation_upgrade/utility/toxic_compatibility/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.potency_per_xenochemical = SENTINEL_DRAIN_MULTIPLIER / 3

/datum/mutation_upgrade/utility/toxic_compatibility/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.potency_per_xenochemical = initial(ability.potency_per_xenochemical)

/datum/mutation_upgrade/utility/vaporous_toxins
	name = "Vaporous Toxins"
	desc = "Drain Sting creates a small cloud of acidic gas ontop your target. If Drain Sting is at half potency or above, its radius increases. If Drain Sting is at maximum potency, it becomes opaque."

/datum/mutation_upgrade/utility/baton_surge
	name = "Baton Surge"
	desc = "Drain Sting can target xenomorphs. When used on xenomorphs, it immediately grants them Drain Surge."

/datum/mutation_upgrade/utility/baton_surge/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.can_target_allies = TRUE

/datum/mutation_upgrade/utility/baton_surge/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.can_target_allies = initial(ability.can_target_allies)
