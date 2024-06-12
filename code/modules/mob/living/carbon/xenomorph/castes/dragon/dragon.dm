/mob/living/carbon/xenomorph/dragon
	caste_base_type = /datum/xeno_caste/dragon
	name = "Dragon"
	desc = "Placeholder description."
	icon = 'icons/Xeno/castes/dragon.dmi'
	icon_state = "Behemoth Walking"
	bubble_icon = "alienleft"
	health = 750
	maxHealth = 750
	plasma_stored = 200
	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_NORMAL
	drag_delay = 6
	mob_size = MOB_SIZE_BIG
	pixel_x = -28.5
	old_x = -28.5
	footstep_type = FOOTSTEP_XENO_HEAVY
	xeno_flags = XENO_ROUNY // I have no sprites, so here is my own to use. :)
	// Particles from basic attack.
	//var/obj/effect/abstract/particle_holder/particle_holder

// They do not have a normal attack. Refer to the slash ability instead.
/mob/living/carbon/xenomorph/dragon/UnarmedAttack(atom/A, has_proximity, modifiers)
	return

	/*
	if(TIMER_COOLDOWN_CHECK(src, COOLDOWN_DRAGON_BASIC_ATTACK))
		src.balloon_alert(src, "Not ready to attack again!")
		return

	if(!do_after(src, 1 SECONDS, IGNORE_HELD_ITEM, clicked_atom, BUSY_ICON_DANGER))
		return

	TIMER_COOLDOWN_START(src, COOLDOWN_DRAGON_BASIC_ATTACK, 4 SECONDS)

	var/mob/living/carbon/xenomorph/dragon/X = src
	src.emote("roar2")
	src.visible_message(span_danger("\The [src] thrashes about in a murderous frenzy!"), \
	span_xenowarning("We thrash about in a murderous frenzy!"))

	src.face_atom(clicked_atom)
	activate_particles(src.dir)

	var/list/atom/movable/atoms_to_ravage = get_step(src, src.dir).contents.Copy()
	atoms_to_ravage += get_step(src, turn(src.dir, -45)).contents
	atoms_to_ravage += get_step(src, turn(src.dir, 45)).contents
	for(var/atom/movable/ravaged AS in atoms_to_ravage)
		if(!(ravaged.resistance_flags & XENO_DAMAGEABLE) || !src.Adjacent(ravaged))
			continue
		if(!ishuman(ravaged))
			ravaged.attack_alien(X, src.xeno_caste.melee_damage)
			ravaged.knockback(X, DRAGON_BASIC_THROW_RANGE, DRAGON_BASIC_THROW_SPEED)
			continue
		var/mob/living/carbon/human/human_victim = ravaged
		if(human_victim.stat == DEAD)
			continue
		human_victim.attack_alien_harm(X, src.xeno_caste.melee_damage * src.xeno_melee_damage_modifier, FALSE, TRUE, FALSE, TRUE)
		human_victim.knockback(X, DRAGON_BASIC_THROW_RANGE, DRAGON_BASIC_THROW_SPEED)
		shake_camera(human_victim, 2, 1)
		human_victim.Paralyze(1 SECONDS)

// Ungracefully stolen from ravager's code; look there for doc and comments instead.
/mob/living/carbon/xenomorph/dragon/proc/activate_particles(direction)
	particle_holder = new(get_turf(src), /particles/dragon_slash)
	QDEL_NULL_IN(src, particle_holder, 5)
	particle_holder.particles.rotation += dir2angle(direction)
	switch(direction)
		if(NORTH)
			particle_holder.particles.position = list(8, 4)
			particle_holder.particles.velocity = list(0, 20)
		if(EAST)
			particle_holder.particles.position = list(3, -8)
			particle_holder.particles.velocity = list(20, 0)
		if(SOUTH)
			particle_holder.particles.position = list(-9, -3)
			particle_holder.particles.velocity = list(0, -20)
		if(WEST)
			particle_holder.particles.position = list(-4, 9)
			particle_holder.particles.velocity = list(-20, 0)

/particles/dragon_slash
	icon = 'icons/effects/200x200.dmi'
	icon_state = "ravager_slash"
	width = 600
	height = 600
	count = 1
	spawning = 1
	lifespan = 4
	fade = 4
	scale = 0.6
	grow = -0.02
	rotation = -160
	friction = 0.6
*/
