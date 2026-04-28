//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/sentinel
	allowed_caste_names = list(/datum/xeno_caste/sentinel)

/datum/mutation_upgrade/offense/sentinel
	allowed_caste_names = list(/datum/xeno_caste/sentinel)

/datum/mutation_upgrade/utility/sentinel
	allowed_caste_names = list(/datum/xeno_caste/sentinel)

//*********************//
//       Defense       //
//*********************//
/datum/mutation_upgrade/defense/sentinel/comforting_acid
	name = "Comforting Acid"
	desc = "Toxic Slash will cause humans to passively heal you for 1 health per stack of Intoxicated as long you are adjacent to them."

/datum/mutation_upgrade/defense/sentinel/comforting_acid/on_gain()
	var/datum/action/ability/xeno_action/toxic_slash/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/toxic_slash]
	if(!ability)
		return
	ability.healing_per_stack += 1

/datum/mutation_upgrade/defense/sentinel/comforting_acid/on_loss()
	var/datum/action/ability/xeno_action/toxic_slash/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/toxic_slash]
	if(!ability)
		return
	ability.healing_per_stack -= 1

/datum/mutation_upgrade/defense/sentinel/healing_sting
	name = "Healing Sting"
	desc = "Drain Sting's healing is increased by 50%. Any leftover healing is converted to overheal health."

/datum/mutation_upgrade/defense/sentinel/healing_sting/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.heal_multiplier += 0.5

/datum/mutation_upgrade/defense/sentinel/healing_sting/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.heal_multiplier -= 0.5

/datum/mutation_upgrade/defense/sentinel/constant_surge
	name = "Constant Surge"
	desc = "Drain Sting always triggers Drain Surge. Drain Surge only gives 1/1.25/1.5 soft armor for each Intoxicated stack."

/datum/mutation_upgrade/defense/sentinel/constant_surge/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_strength -= initial(ability.drain_surge_strength)
	ability.drain_surge_armor_per += 1

/datum/mutation_upgrade/defense/sentinel/constant_surge/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_strength += initial(ability.drain_surge_strength)
	ability.drain_surge_armor_per -= 1

//*********************//
//         Spur        //
//*********************//
/datum/mutation_upgrade/offense/sentinel/acidic_slasher
	name = "Acidic Slasher"
	desc = "Your attack delay will be 0.05s faster and will always apply a stack of Intoxicated against humans, but all melee damage is reduced by 30%."


/datum/mutation_upgrade/offense/sentinel/acidic_slasher/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack))
	xenomorph_owner.xeno_melee_damage_modifier -= 0.3
	xenomorph_owner.next_move_adjust -= 0.5 // In deciseconds.

/datum/mutation_upgrade/offense/sentinel/acidic_slasher/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING)
	xenomorph_owner.xeno_melee_damage_modifier += 0.3
	xenomorph_owner.next_move_adjust += 0.5

/// Applies a variable amount of Intoxicated stacks to those that they attack.
/datum/mutation_upgrade/offense/sentinel/acidic_slasher/proc/on_postattack(mob/living/source, mob/living/target, damage)
	SIGNAL_HANDLER
	var/datum/status_effect/stacking/intoxicated/debuff = target.has_status_effect(STATUS_EFFECT_INTOXICATED)
	if(!debuff)
		target.apply_status_effect(STATUS_EFFECT_INTOXICATED, 1)
		return
	debuff.add_stacks(1)

/datum/mutation_upgrade/offense/sentinel/far_sting
	name = "Far Sting"
	desc = "Drain Sting can be used at targets 1 additional tile away. If the target is at maximum range, Drain Sting is 50% effective."

/datum/mutation_upgrade/offense/sentinel/far_sting/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.targetable_range += 1
	ability.ranged_effectiveness += 0.5

/datum/mutation_upgrade/offense/sentinel/far_sting/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.targetable_range -= 1
	ability.ranged_effectiveness -= 0.5

/datum/mutation_upgrade/offense/sentinel/far_sting/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!sting_ability)
		return
	sting_ability.ranged_effectiveness += get_effectiveness(new_amount - previous_amount, FALSE)

/datum/mutation_upgrade/offense/sentinel/imbued_claws
	name = "Imbued Claws"
	desc = "Drain Surge's armor is converted to a melee damage modifier."

/datum/mutation_upgrade/offense/sentinel/imbued_claws/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_melee = TRUE

/datum/mutation_upgrade/offense/sentinel/imbued_claws/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.drain_surge_melee = initial(ability.drain_surge_melee)

//*********************//
//         Veil        //
//*********************//
/datum/mutation_upgrade/utility/sentinel/toxic_compatibility
	name = "Toxic Compatibility"
	desc = "Every 5u of xeno-chemicals in your target will count as one stack of Intoxicated when calculating the the strength of your Drain Sting."

/datum/mutation_upgrade/utility/sentinel/toxic_compatibility/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.chemical_potency += SENTINEL_DRAIN_MULTIPLIER / 5

/datum/mutation_upgrade/utility/sentinel/toxic_compatibility/on_loss()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	ability.chemical_potency -= SENTINEL_DRAIN_MULTIPLIER / 5

/datum/mutation_upgrade/utility/sentinel/toxic_blood
	name = "Toxic Blood"
	desc = "Every 80 damage you take, 1 stacks of Intoxicated will be applied to nearby humans."
	/// The amount of damage taken so far before threshold is calculated.
	var/damage_taken_so_far = 0

/datum/mutation_upgrade/utility/sentinel/toxic_blood/on_gain()
	RegisterSignals(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE), PROC_REF(on_damage))

/datum/mutation_upgrade/utility/sentinel/toxic_blood/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE, COMSIG_MOB_STAT_CHANGED))

/// Apply intoxicated stacks to nearby alive humans if the damage threshold is reached.
/datum/mutation_upgrade/utility/sentinel/toxic_blood/proc/on_damage(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(amount <= 0 || xenomorph_owner.stat == DEAD) // It is fine to be unconscious!
		return
	damage_taken_so_far += amount
	if(damage_taken_so_far < 80)
		return
	damage_taken_so_far = 0
	for (var/mob/living/carbon/human/nearby_human AS in cheap_get_humans_near(xenomorph_owner, 1))
		if(nearby_human.stat == DEAD)
			continue
		var/datum/status_effect/stacking/intoxicated/debuff = nearby_human.has_status_effect(STATUS_EFFECT_INTOXICATED)
		if(!debuff)
			nearby_human.apply_status_effect(STATUS_EFFECT_INTOXICATED, 1)
			continue
		debuff.add_stacks(1)

/datum/mutation_upgrade/utility/sentinel/automatic_sting
	name = "Automatic Sting"
	desc = "Drain Sting starts at 3 Intoxication stacks. It is automatically used against slashed humans if there are 12 Intoxication stacks or more."
	/// The amount of Intoxication stacks worth of potency to add to the ability.
	var/potency_as_intoxication_stacks = 3

/datum/mutation_upgrade/utility/sentinel/automatic_sting/on_gain()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack))
	ability.base_potency += (potency_as_intoxication_stacks * SENTINEL_DRAIN_MULTIPLIER)

/datum/mutation_upgrade/utility/sentinel/automatic_sting/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability)
		return
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_POSTATTACK_LIVING)
	ability.base_potency -= (potency_as_intoxication_stacks * SENTINEL_DRAIN_MULTIPLIER)

/// Automatically uses Drain Sting on the attacked target if applicable.
/datum/mutation_upgrade/utility/sentinel/automatic_sting/proc/on_postattack(mob/living/source, mob/living/target, damage)
	SIGNAL_HANDLER
	var/mob/living/carbon/human/human_target = target
	var/datum/action/ability/activable/xeno/drain_sting/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/drain_sting]
	if(!ability || !ability.action_cooldown_finished() || !ability.can_use_ability(human_target, TRUE))
		return
	var/potency = ability.get_potency(human_target)
	var/potency_in_sets = round(potency / SENTINEL_DRAIN_MULTIPLIER)
	if(potency_in_sets < 12)
		return
	INVOKE_NEXT_TICK(ability, TYPE_PROC_REF(/datum/action/ability/activable/xeno/drain_sting, use_ability), human_target)
