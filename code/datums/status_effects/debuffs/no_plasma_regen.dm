
/datum/status_effect/no_plasma_regen
	id = "no_plasma_regen"
	duration = 2 SECONDS
	tick_interval = 2 SECONDS

/datum/status_effect/no_plasma_regen/on_creation(mob/living/new_owner, set_duration)
	if(!isxeno(new_owner))
		CRASH("Something applied no_plasma_regen on a non-xeno!")
	if(isnull(set_duration))
		CRASH("Something applied no_plasma_regen with no duration!")
	duration = set_duration
	return ..()

/datum/status_effect/no_plasma_regen/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_NOPLASMAREGEN, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/no_plasma_regen/on_remove()
	REMOVE_TRAIT(owner, TRAIT_NOPLASMAREGEN, TRAIT_STATUS_EFFECT(id))
	return ..()

/datum/status_effect/no_plasma_regen/tick(seconds_between_ticks)
	to_chat(owner, span_warning("You feel too weak to summon new plasma..."))
