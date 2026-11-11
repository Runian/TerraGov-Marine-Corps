/datum/status_effect/mute
	id = "mute"
	alert_type = /atom/movable/screen/alert/status_effect/mute

/datum/status_effect/mute/on_creation(mob/living/new_owner, set_duration)
	if(set_duration)
		duration = set_duration
	return ..()

/datum/status_effect/mute/on_apply()
	. = ..()
	if(!.)
		return
	ADD_TRAIT(owner, TRAIT_MUTE, TRAIT_STATUS_EFFECT(id))

/datum/status_effect/mute/on_remove()
	REMOVE_TRAIT(owner, TRAIT_MUTE, TRAIT_STATUS_EFFECT(id))
	return ..()

/atom/movable/screen/alert/status_effect/mute
	name = "Muted"
	desc = "You can't speak!"
	icon_state = "mute"
