//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/defender
	allowed_caste_names = list(/datum/xeno_caste/defender)

/datum/mutation_upgrade/offense/defender
	allowed_caste_names = list(/datum/xeno_caste/defender)

/datum/mutation_upgrade/utility/defender
	allowed_caste_names = list(/datum/xeno_caste/defender)

//*********************//
//       Defense       //
//*********************//
/datum/mutation_upgrade/defense/defender/carapace_waxing
	name = "Carapace Waxing"
	desc = "Regenerate Skin additionally reduces various debuffs either by a stack or 2 seconds."

/datum/mutation_upgrade/defense/defender/carapace_waxing/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.debuff_removal_amount += 1

/datum/mutation_upgrade/defense/defender/carapace_waxing/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.debuff_removal_amount -= 1

/datum/mutation_upgrade/defense/defender/brittle_upclose
	name = "Brittle Upclose"
	desc = "You can no longer be staggered by projectiles and gain 5 bullet armor, but lose 30 melee armor. Projectiles from pointblank range negate this bonus bullet armor."
	/// The armor that has been given and eventually removed.
	var/datum/armor/attached_armor

/datum/mutation_upgrade/defense/defender/brittle_upclose/on_gain()
	if(!attached_armor)
		attached_armor = new(-30, 5) // -30 melee, +5 bullet
		xenomorph_owner.soft_armor.attachArmor(attached_armor)
	RegisterSignal(xenomorph_owner, COMSIG_XENO_PROJECTILE_HIT, PROC_REF(pre_projectile_hit))
	ADD_TRAIT(xenomorph_owner, TRAIT_STAGGER_RESISTANT, MUTATION_TRAIT)

/datum/mutation_upgrade/defense/defender/brittle_upclose/on_loss()
	if(attached_armor)
		xenomorph_owner.soft_armor.detachArmor(attached_armor)
		attached_armor = null
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_PROJECTILE_HIT))
	REMOVE_TRAIT(xenomorph_owner, TRAIT_STAGGER_RESISTANT, MUTATION_TRAIT)

/// When hit by a non-friendly projectile at pointblank range, have the projectile deal additional damage.
/datum/mutation_upgrade/defense/defender/brittle_upclose/proc/pre_projectile_hit(datum/source, atom/movable/projectile/proj, cardinal_move, uncrossing)
	SIGNAL_HANDLER
	if(xenomorph_owner.issamexenohive(proj.firer))
		return
	if(proj.distance_travelled >= 2)
		return
	proj.damage *= 1.05 // Effectively negates the bonus bullet armor.

/datum/mutation_upgrade/defense/defender/carapace_regrowth
	name = "Carapace Regrowth"
	desc = "Regenerate Skin additionally recovers 50% of your maximum health, but will reduce all of your armor values by 30 for 6 seconds."

/datum/mutation_upgrade/defense/defender/carapace_regrowth/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.heal_multiplier += 0.5
	ability.armor_debuff_amount += 30

/datum/mutation_upgrade/defense/defender/carapace_regrowth/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.heal_multiplier -= 0.5
	ability.armor_debuff_amount -= 30

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/offense/defender/breathtaking_spin
	name = "Breathtaking Spin"
	desc = "Tail Swipe deals stamina damage instead. It no longer paralyzes and deals 1.5x more damage."

/datum/mutation_upgrade/offense/defender/breathtaking_spin/on_gain()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.damage_multiplier += 1.5
	ability.damage_type = STAMINA
	ability.paralyze_duration = 0 SECONDS

/datum/mutation_upgrade/offense/defender/breathtaking_spin/on_loss()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.damage_multiplier -= 1.5
	ability.damage_type = initial(ability.damage_type)
	ability.paralyze_duration = initial(ability.paralyze_duration)

/datum/mutation_upgrade/offense/defender/power_spin
	name = "Power Spin"
	desc = "Tail Swipe's knockback is increased by a tile and staggers for 1 second."


/datum/mutation_upgrade/offense/defender/power_spin/on_gain()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.knockback_distance += 1
	ability.stagger_duration += 1 SECONDS

/datum/mutation_upgrade/offense/defender/power_spin/on_loss()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.knockback_distance -= 1
	ability.stagger_duration -= 1 SECONDS

/datum/mutation_upgrade/offense/defender/sharpening_claws
	name = "Sharpening Claws"
	desc = "For each 10 sunder / missing armor, your melee damage multiplier is increased by 3%."
	/// The amount that the melee damage modifier has been increased by so far.
	var/modifier_so_far = 0

/datum/mutation_upgrade/offense/defender/sharpening_claws/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_SUNDER_CHANGE, PROC_REF(on_sunder_change))
	on_sunder_change(xenomorph_owner, 0, xenomorph_owner.sunder)

/datum/mutation_upgrade/offense/defender/sharpening_claws/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_SUNDER_CHANGE)
	on_sunder_change(xenomorph_owner, xenomorph_owner.sunder, 0)

/// Changes melee damage modifier based on the difference between old and current sunder values.
/datum/mutation_upgrade/offense/defender/sharpening_claws/proc/on_sunder_change(datum/source, old_sunder, new_sunder)
	SIGNAL_HANDLER
	var/new_modifier = round(new_sunder / 10) * 0.03
	if(new_modifier == modifier_so_far)
		return
	xenomorph_owner.xeno_melee_damage_modifier += (new_modifier - modifier_so_far)
	modifier_so_far = new_modifier

//*********************//
//       Utility       //
//*********************//
/datum/mutation_upgrade/utility/defender/carapace_sweat
	name = "Carapace Sweat"
	desc = "Regenerate Skin can be used while on fire and will apply Resin Jelly to you for 2 seconds. If you were on fire, you will be extinguished and set nearby humans on fire."
	/// For each structure, the amount of deciseconds that the Resin Jelly status effect will have.
	var/duration_per_structure = 2 SECONDS

/datum/mutation_upgrade/offense/defender/carapace_sweat/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.resin_jelly_duration += 2 SECONDS

/datum/mutation_upgrade/offense/defender/carapace_sweat/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.resin_jelly_duration -= 2 SECONDS

/datum/mutation_upgrade/utility/defender/slow_and_steady
	name = "Slow and Steady"
	desc = "You are no longer immobilized during Fortify. However, your move delay is increased by 1.2 seconds while it is active."

/datum/mutation_upgrade/utility/defender/slow_and_steady/on_gain()
	var/datum/action/ability/xeno_action/fortify/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/fortify]
	if(!ability)
		return
	ability.should_immobilize = FALSE
	ability.set_movement_delay(ability.movement_delay + 1.2 SECONDS)

/datum/mutation_upgrade/utility/defender/slow_and_steady/on_loss()
	var/datum/action/ability/xeno_action/fortify/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/fortify]
	if(!ability)
		return
	ability.should_immobilize = initial(ability.should_immobilize)
	ability.set_movement_delay(ability.movement_delay - 1.2 SECONDS)

/datum/mutation_upgrade/utility/defender/carapace_sharing
	name = "Carapace Sharing"
	desc = "Regenerate Skin additionally removes 8/16/24% sunder of a nearby friendly xenomorph. This prioritizes those with the highest sunder."

/datum/mutation_upgrade/utility/defender/carapace_sharing/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.ally_unsunder_multiplier -= 0.08

/datum/mutation_upgrade/utility/defender/carapace_sharing/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.ally_unsunder_multiplier -= 0.08
