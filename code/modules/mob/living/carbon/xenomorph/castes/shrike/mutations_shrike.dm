//*********************//
//        Base        //
//*********************//

/datum/mutation_upgrade/defense/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

/datum/mutation_upgrade/offense/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

/datum/mutation_upgrade/utility/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

//*********************//
//       Defense       //
//*********************//

//*********************//
//        Shell        //
//*********************//
/datum/mutation_upgrade/defense/shrike/lone_healer
	name = "Lone Healer"
	desc = "Psychic Cure can now target yourself."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/psychic_cure
	)

/datum/mutation_upgrade/defense/shrike/lone_healer/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.use_state_flags |= ABILITY_TARGET_SELF

/datum/mutation_upgrade/defense/shrike/lone_healer/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.use_state_flags &= ~(ABILITY_TARGET_SELF)

/datum/mutation_upgrade/defense/shrike/feedback_healing
	name = "Feedback Healing"
	desc = "Psychic Cure heals you for 50% of the health that was healed."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/psychic_cure
	)

/datum/mutation_upgrade/defense/shrike/feedback_healing/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.self_heal_percentage = 0.50

/datum/mutation_upgrade/defense/shrike/feedback_healing/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.self_heal_percentage = initial(ability.self_heal_percentage)

/datum/mutation_upgrade/defense/shrike/fireproof_healing
	name = "Fireproof Healing"
	desc = "Psychic Cure applies the Resin Jelly effect to your target for its cooldown duration."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/psychic_cure
	)

/datum/mutation_upgrade/defense/shrike/fireproof_healing/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.applies_resin_jelly = TRUE

/datum/mutation_upgrade/defense/shrike/fireproof_healing/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	ability.applies_resin_jelly = initial(ability.applies_resin_jelly)

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/defense/shrike/psychic_collusion
	name = "Psychic Collusion"
	desc = "Psychic Fling enables collision for human targets. Upon colliding with a human, they are stunned as well."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/psychic_fling
	)

/datum/mutation_upgrade/defense/shrike/psychic_collusion/on_gain()
	var/datum/action/ability/activable/xeno/psychic_fling/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	ability.impact_stuns = TRUE

/datum/mutation_upgrade/defense/shrike/psychic_collusion/on_loss()
	var/datum/action/ability/activable/xeno/psychic_fling/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	ability.impact_stuns = initial(ability.impact_stuns)



//*********************//
//         Spur        //
//*********************//


/datum/mutation_upgrade/offense/gravity_tide
	name = "Gravity Tide"
	desc = "Unrelenting Force pulls things towards you then pushes them away. The distance they are thrown is increased by 2/3/4."
	/// For the first structure, the amount of distance that Unrelenting Force will throw things.
	var/distance_initial = 1
	/// For each structure, the additional amount of distance that Unrelenting Force will throw things.
	var/distance_per_structure = -1

/datum/mutation_upgrade/offense/gravity_tide/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Unrelenting Force pulls things towards you then pushes them away. The distance they are thrown is increased by [get_distance(new_amount)]."

/datum/mutation_upgrade/offense/gravity_tide/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.rebound_throwing = TRUE
	force_ability.throwing_distance += get_distance(0)

/datum/mutation_upgrade/offense/gravity_tide/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.rebound_throwing = initial(force_ability.rebound_throwing)
	force_ability.throwing_distance -= get_distance(0)

/datum/mutation_upgrade/offense/gravity_tide/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.throwing_distance += get_distance(new_amount - previous_amount, FALSE)

/// Returns the amount of distance that Unrelenting Force will throw things.
/datum/mutation_upgrade/offense/gravity_tide/proc/get_distance(structure_count, include_initial = TRUE)
	return (include_initial ? distance_initial : 0) + (distance_per_structure * structure_count)

/datum/mutation_upgrade/offense/body_fling
	name = "Body Fling"
	desc = "Psychic Fling can be used on yourself and allied xenomorphs. Humans who are hit by a flung xenomorph are paralyzed for 2 seconds and dealt 150/175/200% of your slash damage."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/utility/psychic_choke
	)
	/// For the first structure, the multiplier of the owner's slash damage dealt when a xenomorph flung by Psychic Fling collides with a human.
	var/multiplier_initial = 1.25
	/// For each structure, the additional multiplier of the owner's slash damage dealt when a xenomorph flung by Psychic Fling collides with a human.
	var/multiplier_per_structure = 0.25

/datum/mutation_upgrade/offense/body_fling/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Psychic Fling can be used on yourself and allied xenomorphs. Humans who are hit by a flung xenomorph are paralyzed for 2 seconds and dealt [PERCENT(get_multiplier(new_amount))]% of your slash damage."

/datum/mutation_upgrade/offense/body_fling/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	//fling_ability.use_state_flags |= ABILITY_TARGET_SELF
	//fling_ability.collusion_damage_multiplier += get_multiplier(0)
	//fling_ability.collusion_paralyze_duration += 2 SECONDS
	//fling_ability.collusion_xenos_only = TRUE

/datum/mutation_upgrade/offense/body_fling/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	//fling_ability.use_state_flags &= ~ABILITY_TARGET_SELF
	//fling_ability.collusion_damage_multiplier -= get_multiplier(0)
	//fling_ability.collusion_paralyze_duration -= 2 SECONDS
	//fling_ability.collusion_xenos_only = initial(fling_ability.collusion_xenos_only)

/datum/mutation_upgrade/offense/body_fling/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	//fling_ability.collusion_damage_multiplier += get_multiplier(new_amount - previous_amount, FALSE)

/// Returns multiplier of the owner's slash damage dealt when a xenomorph flung by Psychic Fling collides with a human.
/datum/mutation_upgrade/offense/body_fling/proc/get_multiplier(structure_count, include_initial = TRUE)
	return (include_initial ? multiplier_initial : 0) + (multiplier_per_structure * structure_count)

//*********************//
//         Veil        //
//*********************//
/datum/mutation_upgrade/utility/delayed_condition
	name = "Delayed Condition"
	desc = "Psychic Heal grants slowdown immunity and delays all inbound stun, knockdown, and stagger effects caused to your target by 8/10/12 seconds. At the end of this duration, delayed status effects are reapplied."
	/// For the first structure, the amount of deciseconds that Psychic Cure will delay various status effects by.
	var/duration_initial = 6 SECONDS
	/// For each structure, the amount of deciseconds that Psychic Cure will delay various status effects by.
	var/duration_per_structure = 2 SECONDS

/datum/mutation_upgrade/utility/delayed_condition/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Psychic Heal grants slowdown immunity and delays all inbound stun, knockdown, and stagger effects caused to your target by [get_duration(new_amount) / 10] seconds. At the end of this duration, delayed status effects are reapplied."

/datum/mutation_upgrade/utility/delayed_condition/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.delayed_status_duration += get_duration(0)

/datum/mutation_upgrade/utility/delayed_condition/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.delayed_status_duration -= get_duration(0)

/datum/mutation_upgrade/utility/delayed_condition/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.delayed_status_duration += get_duration(new_amount - previous_amount, FALSE)

/// Returns the amount of deciseconds that Psychic Cure will delay various status effects by.
/datum/mutation_upgrade/utility/delayed_condition/proc/get_duration(structure_count, include_initial = TRUE)
	return (include_initial ? duration_initial : 0) + (duration_per_structure * structure_count)

/datum/mutation_upgrade/utility/deflective_force
	name = "Deflective Force"
	desc = "Unrelenting Force now reflects all projectiles in its affected area. Reflecting more than 50 projectile damage resets Psychic Scream's cooldown to 50/40/30% of its original value."
	/// For the first structure, the amount to multiply Psychic Scream's cooldown by if enough projectile damage was reflected.
	var/multiplier_initial = 0.6
	/// For each structure, the additional amount to multiply Psychic Scream's cooldown by if enough projectile damage was reflected.
	var/multiplier_per_structure = -0.1

/datum/mutation_upgrade/utility/deflective_force/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Unrelenting Force now reflects all projectiles in its affected area. Reflecting more than 50 projectile damage resets Psychic Scream's cooldown to [PERCENT(get_multiplier(new_amount))]% of its original value."

/datum/mutation_upgrade/utility/deflective_force/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.projectile_cooldown_mulitplier += get_multiplier(0)

/datum/mutation_upgrade/utility/deflective_force/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.projectile_cooldown_mulitplier -= get_multiplier(0)

/datum/mutation_upgrade/utility/deflective_force/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.projectile_cooldown_mulitplier += get_multiplier(new_amount - previous_amount, FALSE)

/// Returns the amount to multiply Psychic Scream's cooldown by if enough projectile damage was reflected.
/datum/mutation_upgrade/utility/deflective_force/proc/get_multiplier(structure_count, include_initial = TRUE)
	return (include_initial ? multiplier_initial : 0) + (multiplier_per_structure * structure_count)

/datum/mutation_upgrade/utility/psychic_choke
	name = "Psychic Choke"
	desc = "You lose the ability Psychic Fling in exchange for the ability Psychic Choke. Psychic Choke lets you paralyze a marine as long you channel it. The damage threshold to disrupt Psychic Choke is 20/35/50."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/offense/body_fling
	)
	/// For each structure, the amount to increase Psychic Choke's damage threshold.
	var/threshold_per_structure = 15

/datum/mutation_upgrade/utility/psychic_choke/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "You lose the ability Psychic Fling in exchange for the ability Psychic Choke. Psychic Choke lets you paralyze a marine as long you channel it. The damage threshold to disrupt Psychic Choke is [get_threshold(new_amount)]."

/datum/mutation_upgrade/utility/psychic_choke/on_gain()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(fling_ability)
		fling_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/activable/xeno/psychic_choke/choke_ability = new()
	choke_ability.give_action(xenomorph_owner)
	return ..()

/datum/mutation_upgrade/utility/psychic_choke/on_loss()
	var/datum/action/ability/activable/xeno/psychic_choke/choke_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_choke]
	if(choke_ability)
		choke_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = new()
	fling_ability.give_action(xenomorph_owner)
	return ..()

/datum/mutation_upgrade/utility/psychic_choke/on_structure_update(previous_amount, new_amount)
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_choke/choke_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_choke]
	if(!choke_ability)
		return
	choke_ability.damage_threshold += get_threshold(new_amount - previous_amount, FALSE)

/datum/mutation_upgrade/utility/psychic_choke/on_xenomorph_upgrade()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(fling_ability)
		fling_ability.remove_action(xenomorph_owner) // Since upgrading give abilities that are missing, we have to remove it again.

/// Returns the amount to increase Psychic Choke's damage threshold.
/datum/mutation_upgrade/utility/psychic_choke/proc/get_threshold(structure_count, include_initial = TRUE)
	return (include_initial ? PSYCHIC_CHOKE_DAMAGE_THRESHOLD : 0) + (threshold_per_structure * structure_count)
