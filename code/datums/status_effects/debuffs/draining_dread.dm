/datum/status_effect/draining_dread
	id = "draining_dread"
	status_type = STATUS_EFFECT_REPLACE
	duration = 6 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/draining_dread
	/// The amount of stamina damage per second.
	var/stamina_damage = 4

/datum/status_effect/draining_dread/on_creation(mob/living/new_owner, set_stamina_damage)
	if(set_stamina_damage)
		stamina_damage = set_stamina_damage
	return ..()

/datum/status_effect/draining_dread/tick(seconds_between_ticks)
	. = ..()
	owner.do_jitter_animation(250)
	owner.adjustStaminaLoss(stamina_damage)

/atom/movable/screen/alert/status_effect/draining_dread
	name = "Draining Dread"
	desc = "A dreadful presence. You take constant stamina damage until this expires."
	icon_state = "dread"
