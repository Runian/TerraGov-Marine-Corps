/datum/status_effect/stacking/melting_acid
	id = "melting_acid"
	tick_interval = 1 SECONDS
	alert_type = /atom/movable/screen/alert/status_effect/melting_acid
	max_stacks = 30
	consumed_on_threshold = FALSE
	/// Used for particles. Holds the particles instead of the mob. See particle_holder for documentation.
	var/obj/effect/abstract/particle_holder/particle_holder

/datum/status_effect/stacking/melting_acid/on_apply()
	if(owner.status_flags & GODMODE || owner.stat == DEAD)
		return FALSE
	. = ..()
	if(.)
		return
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 30)
	particle_holder = new(owner, /particles/melting_acid_status)
	particle_holder.particles.spawning = 1 + round(stacks / 4)

/datum/status_effect/stacking/melting_acid/on_remove()
	if(particle_holder)
		QDEL_NULL(particle_holder)
	return ..()

/datum/status_effect/stacking/melting_acid/tick(seconds_between_ticks)
	. = ..()
	if(QDELETED(src)) // Parent may delete us (no stacks, etc).
		return
	playsound(owner.loc, "sound/bullets/acid_impact1.ogg", 4, FALSE, ADJACENT_SOUND_RANGE)
	particle_holder.particles.spawning = 1 + round(stacks / 4)
	particle_holder.pixel_x = -2
	particle_holder.pixel_y = 0
	owner.apply_damage(5, BURN, null, ACID)

/datum/status_effect/stacking/melting_acid/can_have_status()
	if(owner.status_flags & GODMODE)
		return FALSE
	return ..() // Parent already checks if they're dead.

/datum/status_effect/stacking/melting_acid/can_gain_stacks()
	return can_have_status()

/atom/movable/screen/alert/status_effect/melting_acid
	name = "Melting (Acid)"
	desc = "You are melting away!"
	icon_state = "melting"

/particles/melting_acid_status
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
	color = "#59ff4a"
