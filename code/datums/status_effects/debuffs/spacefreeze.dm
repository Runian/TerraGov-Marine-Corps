
/datum/status_effect/spacefreeze
	id = "spacefreeze"
	alert_type = /atom/movable/screen/alert/status_effect/spacefreeze

/datum/status_effect/spacefreeze/on_apply()
	. = ..()
	if(!.)
		return
	to_chat(owner, span_danger("The cold vacuum instantly freezes you, maybe this was a bad idea?"))

/datum/status_effect/spacefreeze/tick(seconds_per_tick)
	owner.adjustFireLoss(40)

/datum/status_effect/spacefreeze/light
	id = "spacefreeze_light"

/datum/status_effect/spacefreeze/light/tick(seconds_per_tick)
	if(owner.is_on_fire())
		owner.extinguish_mob()
	if(owner.stat == DEAD)
		return
	owner.adjustFireLoss(10)

/atom/movable/screen/alert/status_effect/spacefreeze
	name = "Freezing"
	desc = "Space is very very cold, who would've thought?"
	icon_state = "cold3"
