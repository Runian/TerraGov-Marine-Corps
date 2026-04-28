/datum/mutation_upgrade
	/// The name of the mutation.
	var/name
	/// The description of the mutation.
	var/desc
	/// The category it appears in.
	var/category
	/// The typepath of the alert to be given.
	var/atom/movable/screen/alert/alert_typepath
	/// If the xenomorph is one of these castes (compared via name), they can view and potentially buy this mutation. Any /datum/xeno_caste are converted to names during /New().
	var/list/allowed_caste_names = list()
	/// If the xenomorph already has one of these mutation types, they cannot get this mutation.
	var/list/datum/mutation_upgrade/conflicting_mutation_types = list()
	/// If the xenomorph does not have all of these abilities types, they cannot get this mutation.
	var/list/datum/action/ability/required_abilities_types = list()

/datum/mutation_upgrade/New(mob/living/carbon/xenomorph/new_xenomorph_owner)
	if(!length(allowed_caste_names))
		return
	var/list/saner_caste_names = list()
	for(var/possible_caste_type AS in allowed_caste_names)
		if(ispath(possible_caste_type, /datum/xeno_caste))
			var/datum/xeno_caste/caste_type = possible_caste_type
			saner_caste_names += caste_type.caste_name // We use names because we want to differentiate between the caste + caste primo vs. strain + strain primo.
			continue
		saner_caste_names += possible_caste_type // We are assuming that it is the caste's name.
	allowed_caste_names.Cut()
	allowed_caste_names = saner_caste_names

/// Called when the mutation is gained.
/datum/mutation_upgrade/proc/on_gain(mob/living/carbon/xenomorph/mutated_xenomorph)
	SHOULD_CALL_PARENT(TRUE)
	var/atom/movable/screen/alert/alert = mutated_xenomorph.throw_alert("mutation_[initial(name)]", alert_typepath)
	alert.name = name
	alert.desc = desc
	mutated_xenomorph.owned_mutations += type
	RegisterSignal(mutated_xenomorph, COMSIG_XENOMORPH_ABILITY_ON_UPGRADE, TYPE_PROC_REF(/datum/mutation_upgrade, on_xenomorph_upgrade))

/// Called when the mutation is lost.
/datum/mutation_upgrade/proc/on_loss(mob/living/carbon/xenomorph/mutated_xenomorph)
	SHOULD_CALL_PARENT(TRUE)
	mutated_xenomorph.clear_alert("mutation_[initial(name)]")
	mutated_xenomorph.owned_mutations -= type
	UnregisterSignal(mutated_xenomorph, COMSIG_XENOMORPH_ABILITY_ON_UPGRADE)

/// Called whenever the xenomorph owner is upgraded (e.g. normal to primordial).
/datum/mutation_upgrade/proc/on_xenomorph_upgrade(mob/living/carbon/xenomorph/mutated_xenomorph)
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
