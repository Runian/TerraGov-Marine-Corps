//////////////////
// HELPER PROCS //
//////////////////

/mob/living/proc/apply_status_effect(effect, ...) //applies a given status effect to this mob, returning the effect if it was successful
	. = FALSE
	var/datum/status_effect/S1 = effect
	LAZYINITLIST(status_effects)
	for(var/datum/status_effect/S in status_effects)
		if(S.id == initial(S1.id) && S.status_type)
			if(S.status_type == STATUS_EFFECT_REPLACE)
				qdel(S)
			else if(S.status_type == STATUS_EFFECT_REFRESH)
				S.refresh()
				return
			else
				return
	var/list/arguments = args.Copy()
	arguments[1] = src
	S1 = new effect(arguments)
	. = S1

/mob/living/proc/remove_status_effect(effect) //removes all of a given status effect from this mob, returning TRUE if at least one was removed
	. = FALSE
	if(status_effects)
		var/datum/status_effect/S1 = effect
		for(var/datum/status_effect/S in status_effects)
			if(initial(S1.id) == S.id)
				qdel(S)
				. = TRUE

/mob/living/proc/has_status_effect(effect) //returns the effect if the mob calling the proc owns the given status effect
	. = FALSE
	if(status_effects)
		var/datum/status_effect/S1 = effect
		for(var/datum/status_effect/S in status_effects)
			if(initial(S1.id) == S.id)
				return S

/mob/living/proc/has_status_effect_list(effect) //returns a list of effects with matching IDs that the mod owns; use for effects there can be multiple of
	. = list()
	if(status_effects)
		var/datum/status_effect/S1 = effect
		for(var/datum/status_effect/S in status_effects)
			if(initial(S1.id) == S.id)
				. += S

/**
 * Checks if this mob has a status effect that shares the passed effect's ID
 * and has the passed sources are in its list of sources (ONLY works for grouped efects!)
 *
 * checked_effect - TYPEPATH of a status effect to check for. Checks for its ID, not its typepath
 *
 * Returns an instance of a status effect, or NULL if none were found.
 */
/mob/living/proc/has_status_effect_from_source(datum/status_effect/grouped/checked_effect, sources)
	RETURN_TYPE(/datum/status_effect)

	if(!ispath(checked_effect))
		CRASH("has_status_effect_from_source passed with an improper status effect path.")

	if(!islist(sources))
		sources = list(sources)

	for(var/datum/status_effect/grouped/present_effect in status_effects)
		if(present_effect.id != initial(checked_effect.id))
			continue
		var/list/matching_sources = present_effect.sources & sources
		if(length(matching_sources))
			return present_effect

	return null

/mob/living/proc/remove_all_status_effect()
	. = 0
	for(var/i in status_effects)
		.++
		qdel(i)

