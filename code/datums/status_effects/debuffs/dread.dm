/datum/status_effect/dread
	id = "dread"
	status_type = STATUS_EFFECT_REPLACE
	tick_interval = 2 SECONDS
	duration = 6 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/dread

/datum/status_effect/dread/on_apply()
	owner.add_movespeed_modifier(MOVESPEED_ID_XENO_DREAD, TRUE, 0, NONE, TRUE, 0.4)
	return TRUE

/datum/status_effect/dread/on_remove()
	owner.remove_movespeed_modifier(MOVESPEED_ID_XENO_DREAD)

/datum/status_effect/dread/tick(seconds_between_ticks)
	. = ..()
	owner.do_jitter_animation(250)

/atom/movable/screen/alert/status_effect/dread
	name = "Dread"
	desc = "A dreadful presence. You are slowed down until this expires."
	icon_state = "dread"
