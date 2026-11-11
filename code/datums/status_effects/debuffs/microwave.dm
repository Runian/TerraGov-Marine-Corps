/// Amount of damage done per tick by the microwave status effect.
#define MICROWAVE_STATUS_DAMAGE_MULT 4
/// Duration of the microwave effect. Refreshed on application.
#define MICROWAVE_STATUS_DURATION 5 SECONDS

/datum/status_effect/stacking/microwave
	id = "microwaved"
	alert_type = /atom/movable/screen/alert/status_effect/microwave
	stack_decay = 0
	max_stacks = 5
	consumed_on_threshold = FALSE
	///Used for particles. Holds the particles instead of the mob. See particle_holder for documentation.
	var/obj/effect/abstract/particle_holder/particle_holder
	COOLDOWN_DECLARE(cooldown_microwave_status)

/datum/status_effect/stacking/microwave/on_apply()
	. = ..()
	if(!.)
		return

	owner.balloon_alert(owner, "microwaved!")
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 30)
	particle_holder = new(owner, /particles/microwave_status)
	COOLDOWN_START(src, cooldown_microwave_status, MICROWAVE_STATUS_DURATION)
	return ..()

/datum/status_effect/stacking/microwave/on_remove()
	QDEL_NULL(particle_holder)
	return ..()

/datum/status_effect/stacking/microwave/tick(seconds_between_ticks)
	. = ..()
	if(QDELETED(src))
		return
	if(COOLDOWN_FINISHED(src, cooldown_microwave_status))
		return qdel(src)
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 4, FALSE, ADJACENT_SOUND_RANGE)
	owner.adjustFireLoss(stacks * MICROWAVE_STATUS_DAMAGE_MULT * (owner.mob_size > MOB_SIZE_HUMAN ? 1 : 0.5)) //this shreds humans otherwise

/datum/status_effect/stacking/microwave/can_have_status()
	if(owner.status_flags & GODMODE)
		return FALSE
	return ..() // Parent already checks if they're dead.

/datum/status_effect/stacking/microwave/can_gain_stacks()
	return can_have_status()

/datum/status_effect/stacking/microwave/add_stacks(stacks_added)
	. = ..()
	if(!. || stacks_added < 1)
		return
	particle_holder.particles.spawning = stacks * 6
	COOLDOWN_START(src, cooldown_microwave_status, MICROWAVE_STATUS_DURATION)

/atom/movable/screen/alert/status_effect/microwave
	name = "Microwave"
	desc = "You are burning from the inside!"
	icon_state = "microwave"

/particles/microwave_status
	icon = 'icons/effects/particles/generic_particles.dmi'
	icon_state = "x"
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
	color = "#a7cc00"
