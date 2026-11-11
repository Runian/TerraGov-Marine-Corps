//////////////////
// HELPER PROCS //
//////////////////

/**
 * Applies a given status effect to this mob.
 *
 * new_effect - TYPEPATH of a status effect to apply.
 * Additional status effect arguments can be passed.
 *
 * Returns the instance of the created effected, if successful.
 * Returns 'null' if unsuccessful.
 */
/mob/living/proc/apply_status_effect(datum/status_effect/new_effect, ...)
	RETURN_TYPE(/datum/status_effect)

	var/list/arguments = args.Copy()

	// If the status effect we're applying doesn't allow multiple effects, we need to handle it.
	if(initial(new_effect.status_type) != STATUS_EFFECT_MULTIPLE)
		for(var/datum/status_effect/existing_effect AS in status_effects)
			if(existing_effect.id != initial(new_effect.id))
				continue

			switch(existing_effect.status_type)
				// Multiple are allowed, continue as normal. (Not normally reachable)
				if(STATUS_EFFECT_MULTIPLE)
					break
				// Only one is allowed of this type - early return.
				if(STATUS_EFFECT_UNIQUE)
					return
				// Replace the existing instance (deletes it).
				if(STATUS_EFFECT_REPLACE)
					existing_effect.on_replace()
				// Refresh the existing type, then early return.
				if(STATUS_EFFECT_REFRESH)
					existing_effect.on_refresh(arglist(arguments))
					return

	// For the new effect, the 1st argument is this mob
	arguments[1] = src

	// Create the status effect with our mob + our arguments.
	var/datum/status_effect/new_instance = new new_effect(arguments)
	if(!QDELETED(new_instance))
		return new_instance

/**
 * Removes all instances of a given status effect from this mob
 *
 * removed_effect - TYPEPATH of a status effect to remove.
 * Additional status effect arguments can be passed - these are passed into before_remove.
 *
 * Returns TRUE if at least one was removed.
 */
/mob/living/proc/remove_status_effect(datum/status_effect/removed_effect, ...)
	var/list/arguments = args.Copy(2)

	. = FALSE
	for(var/datum/status_effect/existing_effect as anything in status_effects)
		if(existing_effect.id == initial(removed_effect.id) && existing_effect.before_remove(arglist(arguments)))
			qdel(existing_effect)
			. = TRUE

	return .

/**
 * Checks if this mob has a status effect that shares the passed effect's ID.
 *
 * checked_effect - TYPEPATH of a status effect to check for. Checks for its ID, not its typepath.
 *
 * Returns an instance of a status effect, or NULL if none were found.
 */
/mob/proc/has_status_effect(datum/status_effect/checked_effect)
	// Yes I'm being cringe and putting this on the mob level even though status effects only apply to the living level.
	// There's quite a few places (namely examine) where it's easier to not need to cast to living before checking
	// for an effect such as blindness.
	return null

/mob/living/has_status_effect(datum/status_effect/checked_effect)
	RETURN_TYPE(/datum/status_effect)

	for(var/datum/status_effect/present_effect as anything in status_effects)
		if(present_effect.id == initial(checked_effect.id))
			return present_effect

	return null

/**
 * Checks if this mob has a status effect that shares the passed effect's ID
 * and has the passed sources are in its list of sources. (ONLY works for grouped effects!)
 *
 * checked_effect - TYPEPATH of a status effect to check for. Checks for its ID, not its typepath.
 *
 * Returns an instance of a status effect, or NULL if none were found.
 */
/mob/proc/has_status_effect_from_source(datum/status_effect/grouped/checked_effect, sources)
	// See [/mob/proc/has_status_effect] for reason behind having this on the mob level.
	return null

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

