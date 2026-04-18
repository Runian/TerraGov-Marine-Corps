//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/retrograde
	allowed_caste_names = list(/datum/xeno_caste/runner/retrograde)

/datum/mutation_upgrade/offense/retrograde
	allowed_caste_names = list(/datum/xeno_caste/runner/retrograde)

/datum/mutation_upgrade/utility/retrograde
	allowed_caste_names = list(/datum/xeno_caste/runner/retrograde)

//*********************//
//       Defense       //
//*********************//

/datum/mutation_upgrade/defense/retrograde/gaseous_blood
	name = "Gaseous Blood"
	desc = "Upon getting staggered, creates light gas of your selected chemical ontop of you. This can only be activated once, but resets when you reach full health."
	/// Can this be activated?
	var/can_be_activated = FALSE

/datum/mutation_upgrade/defense/retrograde/gaseous_blood/on_gain()
	RegisterSignals(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE), PROC_REF(on_damage))
	RegisterSignal(xenomorph_owner, COMSIG_LIVING_STATUS_STAGGER, PROC_REF(on_staggered))
	if(xenomorph_owner.health >= xenomorph_owner.maxHealth)
		can_be_activated = TRUE
	return ..()

/datum/mutation_upgrade/defense/retrograde/gaseous_blood/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE, COMSIG_LIVING_STATUS_STAGGER))
	can_be_activated = FALSE
	return ..()

/// When available, creates a variable type of gas upon stagger.
/datum/mutation_upgrade/defense/retrograde/gaseous_blood/proc/on_staggered(datum/source, amount, ignore_canstun)
	if(!can_be_activated || !isturf(xenomorph_owner.loc))
		return
	var/datum/effect_system/smoke_spread/emitted_gas
	switch(xenomorph_owner.selected_reagent)
		if(/datum/reagent/toxin/xeno_neurotoxin)
			emitted_gas = new /datum/effect_system/smoke_spread/xeno/neuro/light/extinguishing(xenomorph_owner)
		if(/datum/reagent/toxin/xeno_hemodile)
			emitted_gas = new /datum/effect_system/smoke_spread/xeno/hemodile/light(xenomorph_owner)
		if(/datum/reagent/toxin/xeno_transvitox)
			emitted_gas = new /datum/effect_system/smoke_spread/xeno/transvitox/light(xenomorph_owner)
		if(/datum/reagent/toxin/xeno_ozelomelyn)
			emitted_gas = new /datum/effect_system/smoke_spread/xeno/ozelomelyn/light(xenomorph_owner)
	if(!emitted_gas)
		return
	emitted_gas.set_up(1, get_turf(xenomorph_owner), 2) // 4 seconds of gas.
	emitted_gas.start()

/// If they have full health, make it available.
/datum/mutation_upgrade/defense/retrograde/gaseous_blood/proc/on_damage(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(can_be_activated || xenomorph_owner.health < xenomorph_owner.maxHealth)
		return
	can_be_activated = TRUE

//*********************//
//       Offense       //
//*********************//

/datum/mutation_upgrade/offense/retrograde/chemical_claws
	name = "Chemical Claws"
	desc = "Neurotoxin Sting is replaced with Reagent Slash. This ability temporarily causes your next 3 slashes to inject 7u of your selected reagent."

/datum/mutation_upgrade/offense/retrograde/chemical_claws/on_gain()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(sting_ability)
		sting_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/xeno_action/reagent_slash/slash_ability = new()
	slash_ability.give_action(xenomorph_owner)
	slash_ability.reagent_slash_amount = 6

/datum/mutation_upgrade/offense/retrograde/chemical_claws/on_loss()
	var/datum/action/ability/xeno_action/reagent_slash/slash_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(slash_ability)
		slash_ability.remove_action(xenomorph_owner) // No need to decrease the reagent slash amount since the ability is gone now.
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = new()
	sting_ability.give_action(xenomorph_owner)

/datum/mutation_upgrade/offense/retrograde/chemical_claws/on_xenomorph_upgrade()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(sting_ability)
		sting_ability.remove_action(xenomorph_owner)

/datum/mutation_upgrade/offense/retrograde/chemical_spit
	name = "Chemical Spit"
	desc = "Your spit now uses your selected reagent and may apply different effects accordingly."

/datum/mutation_upgrade/offense/chemical_spit/retrograde/on_gain()
	var/datum/action/ability/activable/xeno/retrograde_spit/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/retrograde_spit]
	if(!ability)
		return
	ability.uses_selected_reagent = TRUE

/datum/mutation_upgrade/offense/chemical_spit/retrograde/on_loss()
	var/datum/action/ability/activable/xeno/retrograde_spit/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/retrograde_spit]
	if(!ability)
		return
	ability.uses_selected_reagent = initial(ability.uses_selected_reagent)

//*********************//
//         Veil        //
//*********************//
/datum/mutation_upgrade/utility/retrograde/chemical_spillage
	name = "Chemical Spillage"
	desc = "Neurotoxin Sting creates a small cloud of non-opaque gas ontop of your target. The gas uses your selected reagent."

/datum/mutation_upgrade/utility/retrograde/chemical_spillage/on_gain()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.gas_range = 1

/datum/mutation_upgrade/utility/retrograde/chemical_spillage/on_loss()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.gas_range = initial(sting_ability.gas_range)

/datum/mutation_upgrade/utility/retrograde/chemical_development
	name = "Chemical Development"
	desc = "You can now choose your selected reagent. It can be any of the following: Neurotoxin, Hemodile, Transvitox, or Ozelomelyn."

/datum/mutation_upgrade/utility/retrograde/chemical_spillage/on_gain()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.gas_range = 1

/datum/mutation_upgrade/utility/retrograde/chemical_spillage/on_loss()
	var/datum/action/ability/activable/xeno/neurotox_sting/sting_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/neurotox_sting]
	if(!sting_ability)
		return
	sting_ability.gas_range = initial(sting_ability.gas_range)
