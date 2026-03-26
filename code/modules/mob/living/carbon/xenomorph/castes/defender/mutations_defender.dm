
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
	desc = "Regenerate Skin removes various debuffs currently applied to you."

/datum/mutation_upgrade/defense/defender/carapace_waxing/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.removes_all_debuffs = TRUE

/datum/mutation_upgrade/defense/defender/scout/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.removes_all_debuffs = initial(ability.removes_all_debuffs)

/datum/mutation_upgrade/defense/defender/brittle_upclose
	name = "Brittle Upclose"
	desc = "You are immune to stagger caused by projectiles, but take 10% more damage from projectiles at pointblank range."

/datum/mutation_upgrade/defense/defender/brittle_upclose/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_XENO_PROJECTILE_HIT, PROC_REF(pre_projectile_hit))
	ADD_TRAIT(xenomorph_owner, TRAIT_STAGGER_RESISTANT, MUTATION_TRAIT)
	return ..()

/datum/mutation_upgrade/defense/defender/brittle_upclose/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_PROJECTILE_HIT))
	REMOVE_TRAIT(xenomorph_owner, TRAIT_STAGGER_RESISTANT, MUTATION_TRAIT)
	return ..()

/// When hit by a non-friendly projectile at pointblank range, have the projectile deal additional damage.
/datum/mutation_upgrade/defense/defender/brittle_upclose/proc/pre_projectile_hit(datum/source, atom/movable/projectile/proj, cardinal_move, uncrossing)
	SIGNAL_HANDLER
	if(xenomorph_owner.issamexenohive(proj.firer))
		return
	if(proj.distance_travelled >= 2)
		return
	proj.damage *= 1.1

/datum/mutation_upgrade/defense/defender/carapace_regrowth
	name = "Carapace Regrowth"
	desc = "Regenerate Skin heals you for a total of 60% of your maximum health, but temporarily reduces your soft armor by 30 for 6 seconds."

/datum/mutation_upgrade/defense/defender/carapace_waxing/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.maximum_health_percentage_to_heal = 0.60
	ability.armor_modifier_amount = -30

/datum/mutation_upgrade/defense/defender/scout/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.maximum_health_percentage_to_heal = initial(ability.maximum_health_percentage_to_heal)
	ability.armor_modifier_amount = initial(ability.armor_modifier_amount)

//*********************//
//       Offense       //
//*********************//

/datum/mutation_upgrade/offense/defender/breathtaking_spin
	name = "Breathtaking Spin"
	desc = "Tail Sweep no longer stuns. It deals twice as much damage, but only as stamina damage."

/datum/mutation_upgrade/offense/defender/breathtaking_spin/on_gain()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.damage_multiplier *= 2
	ability.damage_type = STAMINA
	ability.paralyze_duration = 0 SECONDS

/datum/mutation_upgrade/offense/defender/breathtaking_spin/on_loss()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.damage_multiplier /= 2
	ability.damage_type = initial(ability.damage_type)
	ability.paralyze_duration = initial(ability.paralyze_duration)

/datum/mutation_upgrade/offense/defender/power_spin
	name = "Power Spin"
	desc = "Tail Sweep's displacement is considered as a throw. Thrown victims may collide and impact others for additional effects."

/datum/mutation_upgrade/offense/defender/power_spin/on_gain()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.throws_targets_instead = TRUE

/datum/mutation_upgrade/offense/defender/power_spin/on_loss()
	var/datum/action/ability/xeno_action/tail_sweep/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/tail_sweep]
	if(!ability)
		return
	ability.throws_targets_instead = initial(ability.throws_targets_instead)

/datum/mutation_upgrade/offense/defender/sharpening_claws
	name = "Sharpening Claws"
	desc = "Regenerate Skin can be alternative cast to set your sunder to maximum instead. For every point of sunder, your slash damage is increased by 0.4%."
	/// The amount that the owner's melee damage modifier has been increased so far.
	var/modifier_so_far = 0

/datum/mutation_upgrade/offense/defender/sharpening_claws/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_SUNDER_CHANGE, PROC_REF(on_sunder_change))
	on_sunder_change(xenomorph_owner, 0, xenomorph_owner.sunder)
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.alternative_sundering = TRUE


/datum/mutation_upgrade/offense/defender/sharpening_claws/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_SUNDER_CHANGE)
	on_sunder_change(xenomorph_owner, xenomorph_owner.sunder, 0)
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.alternative_sundering = initial(ability.alternative_sundering)

/// Changes the melee damage modifier relative to the modifier already given so far.
/datum/mutation_upgrade/offense/defender/sharpening_claws/proc/on_sunder_change(datum/source, old_sunder, new_sunder)
	SIGNAL_HANDLER
	var/new_modifier = new_sunder * 0.004 // 0.4%
	if(new_modifier == modifier_so_far)
		return
	xenomorph_owner.xeno_melee_damage_modifier += (new_modifier - modifier_so_far)
	modifier_so_far = new_modifier

//*********************//
//       Utility       //
//*********************//

/datum/mutation_upgrade/utility/defender/carapace_sweat
	name = "Carapace Sweat"
	desc = "Regenerate Skin extinguishes and applies the Resin Jelly effect to you for 6 seconds."

/datum/mutation_upgrade/offense/defender/carapace_sweat/on_gain()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.resin_jelly_duration = 6 SECONDS

/datum/mutation_upgrade/offense/defender/carapace_sweat/on_loss()
	var/datum/action/ability/xeno_action/regenerate_skin/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/regenerate_skin]
	if(!ability)
		return
	ability.resin_jelly_duration = initial(ability.resin_jelly_duration)

/datum/mutation_upgrade/utility/defender/slow_and_steady
	name = "Slow and Steady"
	desc = "You can move during Fortify slowly."

/datum/mutation_upgrade/utility/defender/carapace_sweat/on_gain()
	var/datum/action/ability/xeno_action/fortify/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/fortify]
	if(!ability)
		return
	ability.set_immobilize(FALSE)
	ability.set_movement_delay(1 SECONDS)

/datum/mutation_upgrade/utility/defender/carapace_sweat/on_loss()
	var/datum/action/ability/xeno_action/fortify/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/fortify]
	if(!ability)
		return
	ability.set_immobilize(initial(ability.should_immobilize))
	ability.set_movement_delay(initial(ability.movement_delay))
