/datum/mutation_upgrade
	/// The name of the mutation.
	var/name
	/// The description of the mutation.
	var/desc
	/// The category it appears in.
	var/category
	/// The typepath of the alert to be given.
	var/atom/movable/screen/alert/alert_typepath
	/// The alert that was given to the owner.
	var/atom/movable/screen/alert/alert
	/// If any, the xenomorph owner of this mutation upgrade.
	var/mob/living/carbon/xenomorph/xenomorph_owner
	/// If the prospective xenomorph_owner is one of these castes (compared via name), they can view this option. Any /datum/xeno_caste are converted to names during /New().
	var/list/allowed_caste_names = list()
	/// If the prospective xenomorph_owner already has one of these mutation types, they cannot get this mutation.
	var/list/datum/mutation_upgrade/conflicting_mutation_types = list()

/// Handles creation of an mutation upgrade. If there will be an owner, applies the alert for having the mutation, registers various signals, and then updates with current structure count.
/datum/mutation_upgrade/New(mob/living/carbon/xenomorph/new_xenomorph_owner)
	if(length(allowed_caste_names) > 0)
		var/list/saner_caste_names = list()
		for(var/possible_caste_type AS in allowed_caste_names)
			if(ispath(possible_caste_type, /datum/xeno_caste))
				var/datum/xeno_caste/caste_type = possible_caste_type
				saner_caste_names += caste_type.caste_name
				continue
			saner_caste_names += possible_caste_type // We are assuming that it is the caste's name.
		allowed_caste_names.Cut()
		allowed_caste_names = saner_caste_names
	if(!new_xenomorph_owner)
		return
	xenomorph_owner = new_xenomorph_owner
	xenomorph_owner.owned_mutations += src
	alert = xenomorph_owner.throw_alert("mutation_[REF(src)]", alert_typepath)
	update_alert()
	RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_ABILITY_ON_UPGRADE, TYPE_PROC_REF(/datum/mutation_upgrade, on_xenomorph_upgrade))
	on_gain()

/// Handles destruction of an mutation upgrade. If there was an owner, removes the status effect for having the mutation, unregisters various signals, and then updates with zero structures.
/datum/mutation_upgrade/Destroy(force)
	if(!xenomorph_owner)
		return ..()
	if(alert)
		xenomorph_owner.clear_alert("mutation_[REF(src)]")
	if(xenomorph_owner.owned_mutations.Find(src))
		xenomorph_owner.owned_mutations -= src
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_ABILITY_ON_UPGRADE)
	on_loss()
	return ..()

/// Updates the alert's name and description.
/datum/mutation_upgrade/proc/update_alert()
	alert.name = get_name_for_alert()
	alert.desc = get_desc_for_alert()

/// The name that the alert will have after updating.
/datum/mutation_upgrade/proc/get_name_for_alert()
	return name

/// The desc that the alert will have after updating. This should be more informative than the description (by providing meaningful values to compare with) or easier to understand (by using less words or dumbing down the language).
/datum/mutation_upgrade/proc/get_desc_for_alert()
	return desc


/// Called when the mutation is gained by the xenomorph owner.
/datum/mutation_upgrade/proc/on_gain()
	return

/// Called when the mutation is lost by the xenomorph owner.
/datum/mutation_upgrade/proc/on_loss()
	return

/// Called whenever the xenomorph owner is upgraded (e.g. normal to primordial).
/datum/mutation_upgrade/proc/on_xenomorph_upgrade()
	return

/datum/mutation_upgrade/defense
	category = MUTATION_DEFENSE
	alert_typepath = MUTATION_DEFENSE_ALERT

/datum/mutation_upgrade/offense
	category = MUTATION_OFFENSE
	alert_typepath = MUTATION_OFFENSE_ALERT

/datum/mutation_upgrade/utility
	category = MUTATION_UTILITY
	alert_typepath = MUTATION_UTILITY_ALERT

/atom/movable/screen/alert/defense_mutation
	name = "defense mutation"
	icon_state = "xeno_mutation_defense"

/atom/movable/screen/alert/offense_mutation
	name = "offense mutation"
	icon_state = "xeno_mutation_offense"

/atom/movable/screen/alert/utility_mutation
	name = "utility mutation"
	icon_state = "xeno_mutation_utility"


/datum/mutation_upgrade/proc/on_structure_update(previous_amount, new_amount)
	return // TODO: Depreciated. Switch over to `/on_gain` and `/on_loss` instead.

/datum/mutation_upgrade/proc/get_total_structures(previous_amount, new_amount)
	return 0 // TODO: Depreciated. Do not use this at all.

