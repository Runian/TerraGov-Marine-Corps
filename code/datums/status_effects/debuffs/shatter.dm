/// The percentage reduction of armor removed by the shatter status effect.
#define SHATTER_STATUS_EFFECT_ARMOR_MULT 0.2

/datum/status_effect/shatter
	id = "shatter"
	alert_type = /atom/movable/screen/alert/status_effect/shatter
	duration = 10 SECONDS
	status_type = STATUS_EFFECT_REPLACE
	/// A holder for the exact armor modified by this status effect.
	var/datum/armor/armor_modifier
	///Used for particles. Holds the particles instead of the mob. See particle_holder for documentation.
	var/obj/effect/abstract/particle_holder/particle_holder

/datum/status_effect/shatter/on_creation(mob/living/new_owner, set_duration)
	if(set_duration)
		duration = set_duration
	return ..()

/datum/status_effect/shatter/on_apply()
	if((owner.status_flags & GODMODE) || owner.stat == DEAD)
		return FALSE
	. = ..()
	if(!.)
		return

	armor_modifier = owner.soft_armor.scaleAllRatings(SHATTER_STATUS_EFFECT_ARMOR_MULT)
	owner.soft_armor = owner.soft_armor.detachArmor(armor_modifier)
	particle_holder = new(owner, /particles/shattered_status)

/datum/status_effect/shatter/on_remove()
	owner.soft_armor = owner.soft_armor.attachArmor(armor_modifier)
	armor_modifier = null
	if(particle_holder)
		QDEL_NULL(particle_holder)
	return ..()

/atom/movable/screen/alert/status_effect/shatter
	name = "Shattered"
	desc = "Your armor has been shattered!"
	icon_state = "shatter"

/particles/shattered_status
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
	scale = generator(GEN_VECTOR, list(0.6, 0.6), list(1, 1), NORMAL_RAND)
	friction = -0.05
	color = "#818181"

#undef SHATTER_STATUS_EFFECT_ARMOR_MULT
