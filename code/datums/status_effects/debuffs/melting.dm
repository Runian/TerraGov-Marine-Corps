///amount of damage done per tick by the melting status effect
#define STATUS_EFFECT_MELTING_DAMAGE 5
///Sunder inflicted per tick by the melting status effect
#define STATUS_EFFECT_MELTING_SUNDER_DAMAGE 3

/datum/status_effect/stacking/melting
	id = "melting"
	tick_interval = 1 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/melting
	max_stacks = 30
	consumed_on_threshold = FALSE
	alert_type = /atom/movable/screen/alert/status_effect/melting
	///Used for particles. Holds the particles instead of the mob. See particle_holder for documentation.
	var/obj/effect/abstract/particle_holder/particle_holder

/datum/status_effect/stacking/melting/on_apply()
	. = ..()
	if(!.)
		return
	owner.balloon_alert(owner, "melting!")
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 30)
	particle_holder = new(owner, /particles/melting_status)
	particle_holder.particles.spawning = 1 + round(stacks / 2)

/datum/status_effect/stacking/melting/on_remove()
	if(particle_holder)
		QDEL_NULL(particle_holder)
	return ..()

/datum/status_effect/stacking/melting/tick(seconds_between_ticks)
	. = ..()
	if(QDELETED(src))
		return
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 4, FALSE, ADJACENT_SOUND_RANGE)
	particle_holder.particles.spawning = 1 + round(stacks / 2)
	debuff_owner.apply_damage(STATUS_EFFECT_MELTING_DAMAGE, BURN, null, FIRE)

	if(!isxeno(owner))
		return
	var/mob/living/carbon/xenomorph/xenomorph_owner = debuff_owner
	xenomorph_owner.adjust_sunder(STATUS_EFFECT_MELTING_SUNDER_DAMAGE)

/datum/status_effect/stacking/melting/can_have_status()
	if(owner.status_flags & GODMODE || owner.stat == DEAD)
		return FALSE
	if(owner.has_status_effect(STATUS_EFFECT_RESIN_JELLY_COATING))
		return FALSE
	return ..()

/datum/status_effect/stacking/melting/can_gain_stacks()
	return can_have_status()

/atom/movable/screen/alert/status_effect/melting
	name = "Melting"
	desc = "You are melting away!"
	icon_state = "melting"

/particles/melting_status
	icon = 'icons/effects/particles/generic_particles.dmi'
	icon_state = "drip"
	width = 100
	height = 100
	count = 1000
	spawning = 4
	lifespan = 10
	fade = 8
	velocity = list(0, 0)
	position = generator(GEN_SPHERE, 16, 16, NORMAL_RAND)
	drift = generator(GEN_VECTOR, list(-0.1, 0), list(0.1, 0))
	gravity = list(0, -0.4)
	scale = generator(GEN_VECTOR, list(0.3, 0.3), list(1, 1), NORMAL_RAND)
	friction = -0.05
	color = "#cc5200"
