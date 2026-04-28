//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/retrograde
	allowed_caste_names = list(/datum/xeno_caste/sentinel/retrograde)

/datum/mutation_upgrade/offense/retrograde
	allowed_caste_names = list(/datum/xeno_caste/sentinel/retrograde)

/datum/mutation_upgrade/utility/retrograde
	allowed_caste_names = list(/datum/xeno_caste/sentinel/retrograde)

//*********************//
//       Defense       //
//*********************//
/datum/mutation_upgrade/defense/retrograde/gaseous_blood
	name = "Gaseous Blood"
	desc = "Everytime you take damage, you emit non-opaque light neurotoxin gas with a radius of 2. This can happen once every 8 seconds."
	COOLDOWN_DECLARE(activation_cooldown)

/datum/mutation_upgrade/defense/retrograde/gaseous_blood/on_gain()
	RegisterSignals(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE), PROC_REF(on_damage))

/datum/mutation_upgrade/defense/retrograde/gaseous_blood/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE))

/// Emits gas when damage is taken if it is ready to activate.
/datum/mutation_upgrade/defense/retrograde/gaseous_blood/proc/on_damage(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(amount <= 0 || xenomorph_owner.stat == DEAD) // It is fine to be unconscious!
		return
	if(!COOLDOWN_FINISHED(src, activation_cooldown))
		return
	COOLDOWN_START(src, activation_cooldown, 8 SECONDS)
	var/datum/effect_system/smoke_spread/smoke_system = new /datum/effect_system/smoke_spread/xeno/neuro/light()
	smoke_system.set_up(2, get_turf(xenomorph_owner), 2) // 4 seconds of gas.
	smoke_system.start()

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/offense/retrograde/toxic_claws
	name = "Toxic Claws"
	desc = "You gain an ability that makes your slashes inject 6u Neurotoxin for the next 3 slashes. You no longer have the ability Neurotoxin Sting."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/utility/retrograde/toxic_spillage
	)

/datum/mutation_upgrade/offense/retrograde/toxic_claws/on_gain()
	var/datum/action/ability/activable/xeno/neurotox_sting/old_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(old_ability)
		old_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/xeno_action/reagent_slash/new_ability = new()
	new_ability.give_action(xenomorph_owner)
	new_ability.reagent_slash_amount = 6
	xenomorph_owner.set_selected_reagent(/datum/reagent/toxin/xeno_neurotoxin)

/datum/mutation_upgrade/offense/retrograde/toxic_claws/on_loss()
	var/datum/action/ability/xeno_action/reagent_slash/slash_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(slash_ability)
		slash_ability.remove_action(xenomorph_owner) // No need to decrease the reagent slash amount since the ability is gone now.
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = new()
	sting_ability.give_action(xenomorph_owner)
	xenomorph_owner.set_selected_reagent(initial(xenomorph_owner.selected_reagent))

/datum/mutation_upgrade/offense/retrograde/toxic_claws/on_xenomorph_upgrade()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.remove_action(xenomorph_owner)

//*********************//
//       Utility       //
//*********************//
/datum/mutation_upgrade/utility/retrograde/toxic_spillage
	name = "Toxic Spillage"
	desc = "Neurotoxin Sting injects 25% as much neurotoxin. It creates non-opaque light neurotoxin gas spreading out up to 2/3/4 tiles on use."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/offense/retrograde/toxic_claws
	)

/datum/mutation_upgrade/utility/retrograde/toxic_spillage/on_gain()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.sting_amount *= 0.25
	sting_ability.sting_gas = /datum/effect_system/smoke_spread/xeno/neuro/light
	sting_ability.sting_gas_range += 2

/datum/mutation_upgrade/utility/retrograde/toxic_spillage/on_loss()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.sting_amount /= 0.25
	sting_ability.sting_gas = initial(sting_ability.sting_gas)
	sting_ability.sting_gas_range -= 2
