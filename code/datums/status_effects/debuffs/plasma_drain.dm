
/datum/status_effect/plasma_drain
	id = "plasma_drain"
	alert_type = null

/datum/status_effect/plasmadrain/on_creation(mob/living/new_owner, set_duration)
	if(!isxeno(new_owner))
		CRASH("Something applied plasma_drain on a non-xeno!")
	if(isnull(set_duration))
		CRASH("Something applied plasma_drain with no duration!")
	duration = set_duration
	return ..()

/datum/status_effect/plasmadrain/tick(seconds_per_tick)
	var/mob/living/carbon/xenomorph/xenomorph_owner = owner
	// This proc can handle everything and hud updating, use it.
	xenomorph_owner.use_plasma(xenomorph_owner.xeno_caste.plasma_max * 0.1)
