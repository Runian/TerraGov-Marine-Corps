/mob/living/carbon/xenomorph/dragon
	caste_base_type = /datum/xeno_caste/dragon
	name = "Dragon"
	desc = "A massive, ancient beast with scales that shimmer like polished armor. The fiercest and most formidable creature."
	icon = 'icons/Xeno/castes/dragon.dmi'
	icon_state = "Dragon Walking"
	attacktext = "bites"
	attack_sound = null
	friendly = "nuzzles"
	health = 500
	maxHealth = 500
	plasma_stored = 300
	pixel_x = -16
	mob_size = MOB_SIZE_BIG
<<<<<<< HEAD
=======
	drag_delay = 6
>>>>>>> 994a30ec14 (half of dragon flight)
	resistance_flags = BANISH_IMMUNE
	initial_language_holder = /datum/language_holder/xeno/dragon
<<<<<<< HEAD
	tier = XENO_TIER_FOUR // Dragon, like queen, doesn't count towards population limit.
=======
	tier = XENO_TIER_FOUR //Dragon, like queen, doesn't count towards population limit.
>>>>>>> 8fe40bfdb9 (desc pt. 2)
	upgrade = XENO_UPGRADE_NORMAL
	bubble_icon = "alienroyal"
	inherent_verbs = list(
		/mob/living/carbon/xenomorph/proc/hijack,
	)
	xeno_flags = XENO_ROUNY // Because my sprites are so trash.
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
	/// Whether we are attacking.
	var/is_attacking = FALSE
=======
	/// Whether we are doing something (in relation to our attacks/actions) and do not want to do anything else.
	var/is_busy = FALSE
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
	/// Whether we are attacking.
	var/is_attacking = FALSE
>>>>>>> b0d4c5eb57 (dont actually need this)
	/// Whether we are currently in flight.
	var/is_flying = FALSE
=======
	/// Whether we are currently attacking (aka in the progress of the do_after) or not.
	var/is_attacking = FALSE
<<<<<<< HEAD
>>>>>>> 935f42018a (basic slash attack)
=======
	/// Whether we are currently in flight.
	var/is_flying = FALSE
>>>>>>> b1fc44b9df (3/4 of dragon flight)

/mob/living/carbon/xenomorph/dragon/Initialize(mapload)
	. = ..()
	playsound(loc, 'sound/voice/alien/xenos_roaring.ogg', 75, 0)

/mob/living/carbon/xenomorph/dragon/generate_name()
	var/playtime_mins = client?.get_exp(xeno_caste.caste_name)
	var/prefix = (hive.prefix || xeno_caste.upgrade_name) ? "[hive.prefix][xeno_caste.upgrade_name] " : ""
	if(!client?.prefs.show_xeno_rank || !client)
		name = prefix + "Dragon ([nicknumber])"
		real_name = name
		if(mind)
			mind.name = name
		return
	switch(playtime_mins)
		if(0 to 600)
			name = prefix + "Young Dragon ([nicknumber])"
		if(601 to 1500)
			name = prefix + "Mature Dragon ([nicknumber])"
		if(1501 to 4200)
			name = prefix + "Elder Dragon ([nicknumber])"
		if(4201 to 10500)
			name = prefix + "Ancient Dragon ([nicknumber])"
		if(10501 to INFINITY)
			name = prefix + "Prime Dragon ([nicknumber])"
		else
			name = prefix + "Young Dragon ([nicknumber])"

	real_name = name
	if(mind)
		mind.name = name

/mob/living/carbon/xenomorph/dragon/death_cry()
	playsound(loc, 'sound/voice/alien/king_died.ogg', 75, 0)

<<<<<<< HEAD
/mob/living/carbon/xenomorph/dragon/can_receive_aura(aura_type, atom/source, datum/aura_bearer/bearer)
	. = ..()
	return FALSE

/obj/effect/dragon_telegraphed_warning
	icon_state = "shallnotpass"

#define DRAGON_BASIC_THROW_RANGE 1
#define DRAGON_BASIC_THROW_SPEED 2

/mob/living/carbon/xenomorph/dragon/UnarmedAttack(atom/clicked_atom, has_proximity, modifiers)
	if(is_attacking)
		return
	if(is_flying)
		src.balloon_alert(src, "Can't while flying!")
		return
	if(TIMER_COOLDOWN_CHECK(src, COOLDOWN_DRAGON_BASIC_ATTACK))
		src.balloon_alert(src, "Not ready to attack again!")
		return

	src.face_atom(clicked_atom)

	// Targetted turfs.
	var/list/turf/targetted_turfs = list()
	targetted_turfs += get_step(src, src.dir)
	targetted_turfs += get_step(src, turn(src.dir, -45))
	targetted_turfs += get_step(src, turn(src.dir, 45))

	// Telegraph this attack.
	var/list/obj/effect/dragon_telegraphed_warning/warnings = list()
	for(var/turf/targetted_turf in targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

	is_attacking = TRUE
	var/successful = do_after(src, 1 SECONDS, NONE, src, BUSY_ICON_DANGER)

	// Delete the warnings regardless of outcome.
	for(var/obj/effect/warning in warnings)
		qdel(warning)

	// No attacking if they do not follow through.
	is_attacking = FALSE
	if(!successful)
		TIMER_COOLDOWN_START(src, COOLDOWN_DRAGON_BASIC_ATTACK, 1 SECONDS)
		return

	// Full cooldown if they follow through.
	TIMER_COOLDOWN_START(src, COOLDOWN_DRAGON_BASIC_ATTACK, 4 SECONDS)

	// Damage to inflict which varies on what is attacked and if we are enraged.
	var/damage = src.xeno_caste.melee_damage * src.xeno_melee_damage_modifier
	// Attack everything in these turfs.
	for(var/turf/targetted_turf in targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf.contents)
			if(!(attacked_atom.resistance_flags & XENO_DAMAGEABLE))
				continue
			// If they hit every section possible, it is essentially 2x damage.
			if(ishitbox(attacked_atom) || isarmoredvehicle(attacked_atom))
				attacked_atom.attack_alien(src, damage * 2/3)
				continue
			// Handles every other vehicle (motorbikes, unmanned vehicles,  mechs, etc).
			if(isvehicle(attacked_atom))
				attacked_atom.attack_alien(src, damage * 2)
				continue
			if(ishuman(attacked_atom))
				var/mob/living/carbon/human/attacked_human = attacked_atom
				if(attacked_human.stat == DEAD)
					continue
				attacked_human.attack_alien_harm(src, damage, FALSE, TRUE, FALSE, TRUE)
				attacked_human.knockback(src, DRAGON_BASIC_THROW_RANGE, DRAGON_BASIC_THROW_SPEED)
				attacked_human.Paralyze(1 SECONDS)
				shake_camera(attacked_human, 2, 1)
				continue
			// Catches everything else that needs to be hit (like cades).
			if(isobj(attacked_atom))
				attacked_atom.attack_alien(src, damage)
				continue

	// Other cool stuff!
	src.emote("roar6")

/mob/living/carbon/xenomorph/dragon/handle_special_state()
	if(is_flying)
		icon_state = "[xeno_caste.caste_name][(xeno_flags & XENO_ROUNY) ? " rouny" : ""] Flight"
		return TRUE
	return FALSE

/// Handles all variables associated with flying (or landing).
/mob/living/carbon/xenomorph/dragon/proc/switch_flight()
	is_flying = !is_flying
	if(is_flying)
		src.alpha = 50
		src.density = FALSE
		src.status_flags |= (GODMODE|INCORPOREAL)
		src.resistance_flags |= RESIST_ALL
		src.allow_pass_flags = PASSABLE
		src.pass_flags = HOVERING
		return
	src.alpha = initial(src.alpha)
	src.density = initial(src.density)
	src.status_flags &= ~(GODMODE|INCORPOREAL)
	src.resistance_flags &= ~RESIST_ALL
	src.allow_pass_flags = initial(src.allow_pass_flags)
	src.pass_flags = initial(src.pass_flags)
=======

/mob/living/carbon/xenomorph/dragon/can_receive_aura(aura_type, atom/source, datum/aura_bearer/bearer)
	. = ..()
	return FALSE
<<<<<<< HEAD
>>>>>>> bde1171a30 (no phero received)
=======

/obj/effect/dragon_telegraphed_warning
	icon_state = "shallnotpass"

#define DRAGON_BASIC_THROW_RANGE 1
#define DRAGON_BASIC_THROW_SPEED 2

/mob/living/carbon/xenomorph/dragon/UnarmedAttack(atom/clicked_atom, has_proximity, modifiers)
	if(is_attacking)
		return
	if(is_flying)
		src.balloon_alert(src, "Can't while flying!")
		return
	if(TIMER_COOLDOWN_CHECK(src, COOLDOWN_DRAGON_BASIC_ATTACK))
		src.balloon_alert(src, "Not ready to attack again!")
		return

	src.face_atom(clicked_atom)

	// Targetted turfs.
	var/list/turf/targetted_turfs = list()
	targetted_turfs += get_step(src, src.dir)
	targetted_turfs += get_step(src, turn(src.dir, -45))
	targetted_turfs += get_step(src, turn(src.dir, 45))

	// Telegraph this attack.
	var/list/obj/effect/dragon_telegraphed_warning/warnings = list()
	for(var/turf/targetted_turf in targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

	is_attacking = TRUE
	var/successful = do_after(src, 1 SECONDS, NONE, src, BUSY_ICON_DANGER)

	// Delete the warnings regardless of outcome.
	for(var/obj/effect/warning in warnings)
		qdel(warning)

	// No attacking if they do not follow through.
	is_attacking = FALSE
	if(!successful)
		TIMER_COOLDOWN_START(src, COOLDOWN_DRAGON_BASIC_ATTACK, 1 SECONDS)
		return

	// Full cooldown if they follow through.
	TIMER_COOLDOWN_START(src, COOLDOWN_DRAGON_BASIC_ATTACK, 4 SECONDS)

	// Damage to inflict which varies on what is attacked and if we are enraged.
	var/damage = src.xeno_caste.melee_damage * src.xeno_melee_damage_modifier
	// Attack everything in these turfs.
	for(var/turf/targetted_turf in targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf.contents)
			if(!(attacked_atom.resistance_flags & XENO_DAMAGEABLE))
				continue
			// If they hit every section possible, it is essentially 2x damage.
			if(ishitbox(attacked_atom) || isarmoredvehicle(attacked_atom))
				attacked_atom.attack_alien(src, damage * 2/3)
				continue
			// Handles every other vehicle (motorbikes, unmanned vehicles,  mechs, etc).
			if(isvehicle(attacked_atom))
				attacked_atom.attack_alien(src, damage * 2)
				continue
			if(ishuman(attacked_atom))
				var/mob/living/carbon/human/attacked_human = attacked_atom
				if(attacked_human.stat == DEAD)
					continue
				attacked_human.attack_alien_harm(src, damage, FALSE, TRUE, FALSE, TRUE)
				attacked_human.knockback(src, DRAGON_BASIC_THROW_RANGE, DRAGON_BASIC_THROW_SPEED)
				attacked_human.Paralyze(1 SECONDS)
				shake_camera(attacked_human, 2, 1)
				continue
			// Catches everything else that needs to be hit (like cades).
			if(isobj(attacked_atom))
				attacked_atom.attack_alien(src, damage)
				continue

	// Other cool stuff!
	src.emote("roar6")
<<<<<<< HEAD
>>>>>>> 935f42018a (basic slash attack)
=======

/mob/living/carbon/xenomorph/dragon/handle_special_state()
	if(is_flying)
		icon_state = "[xeno_caste.caste_name][(xeno_flags & XENO_ROUNY) ? " rouny" : ""] Flight"
		return TRUE
	return FALSE

/// Handles all variables associated with flying (or landing).
/mob/living/carbon/xenomorph/dragon/proc/switch_flight()
	is_flying = !is_flying
	if(is_flying)
		src.alpha = 50
		src.density = FALSE
		src.status_flags |= (GODMODE|INCORPOREAL)
		src.resistance_flags |= RESIST_ALL
		src.allow_pass_flags = PASSABLE
		src.pass_flags = HOVERING
		return
	src.alpha = initial(src.alpha)
	src.density = initial(src.density)
	src.status_flags &= ~(GODMODE|INCORPOREAL)
	src.resistance_flags &= ~RESIST_ALL
	src.allow_pass_flags = initial(src.allow_pass_flags)
	src.pass_flags = initial(src.pass_flags)
>>>>>>> b1fc44b9df (3/4 of dragon flight)
