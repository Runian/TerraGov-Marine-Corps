/// The radius/range to check for landing turfs.
#define DRAGON_FLIGHT_FLIGHT_RANGE 2
/// The amount of time it takes to begin flight.
#define DRAGON_FLIGHT_FLIGHT_TIME 4 SECONDS
/// The amount of time it takes to finish landing.
#define DRAGON_FLIGHT_LAND_TIME 4 SECONDS
/// How much brute damage is dealt for being in the landing radius (non-epicenter + epicenter)
// Note: Double damage for vehicles and directly stomped on (if carbon).

#define DRAGON_FLIGHT_LAND_DAMAGE 75
/// How long the knockdown is for being in the landing radius (not-epicenter + epicenter)
#define DRAGON_FLIGHT_KNOCKDOWN 2 SECONDS

/* All dragon abilities generally satisfy the following conditions unless there is a reasonable exception:
 * Lengthy cooldown.
 * Lengthy cast time.
 * Telegraphed / must be plainly obviously that something is happening.
 * More powerful the ability, more costly it is in terms of stamina.
 * Not able to easily cancel it (e.g. moving to preventing the ability from completing).
 * Offensive abilities temporarily either: remove stagger immunity or cancel upon taking a certain amount of damage.
 * Self-contained code wise (e.g. does not refer to /mob/living/carbon/xenomorph/dragon or any types beyond itself).
*/

// ***************************************
// *********** Dragon's Flight
// ***************************************
#define DRAGON_FLIGHT_FLIGHT_TIME 3 SECONDS
#define DRAGON_FLIGHT_LAND_TIME 3 SECONDS

#define DRAGON_FLIGHT_LAND_DAMAGE 75
/// How long the knockdown is for being in the landing radius (not-epicenter + epicenter)
#define DRAGON_FLIGHT_KNOCKDOWN 2 SECONDS

/* All dragon abilities generally satisfy the following conditions unless there is a reasonable exception:
 * Lengthy cooldown.
 * Lengthy cast time.
 * Telegraphed / must be plainly obviously that something is happening.
 * More powerful the ability, more costly it is in terms of stamina.
 * Not able to easily cancel it (e.g. moving to preventing the ability from completing).
 * Offensive abilities temporarily either: remove stagger immunity or cancel upon taking a certain amount of damage.
 * Self-contained code wise (e.g. does not refer to /mob/living/carbon/xenomorph/dragon or any types beyond itself).
*/

/datum/action/ability/xeno_action/dragon_flight
	name = "Dragon's Flight"
	action_icon_state = "shattering_roar"
	action_icon = 'icons/Xeno/actions/king.dmi'
	desc = "After a long wind-up, take flight to the skies. While flying, you slowly regenerate your health and can choose where you'll land with a similar wind-up."
	cooldown_duration = 60 SECONDS
	/// Is the owner currently flying?
	var/is_in_flight = FALSE
	/// The timer id used to play the introduction sound (multiple times).
	var/intro_sound_timer_id
	/// The targeted turfs that is referenced for telegraphing and impact.
	var/list/turf/targetted_turfs = list()
	/// The effects that shows the telegraphed area to avoid getting hit (for marines).
	var/list/obj/effect/dragon_telegraphed_warning/telegraph_effects = list()

/datum/action/ability/xeno_action/dragon_flight/can_use_action(silent = FALSE, override_flags)
	silent = TRUE
	. = ..()
	silent = TRUE
	var/area/current_area = get_area(owner)
	if(!is_in_flight)
		// Roofed areas.
		if(current_area.ceiling > CEILING_OBSTRUCTED)
			if(!silent)
				owner.balloon_alert(owner, "Can't fly in caves!")
			return FALSE
		// Shipside (aka not groundside).
		if(!is_groundside())
			if(!silent)
				owner.balloon_alert(owner, "Cannot fly shipside.")
			return FALSE
	else
		// Marine-friendly areas.
		if(isdropshiparea(current_area) || (current_area.area_flags & MARINE_BASE))
			if(!silent)
				owner.balloon_alert(owner, "No landing in marine base!")
			return FALSE

/datum/action/ability/xeno_action/dragon_flight/succeed_activate(ability_cost_override)
	..()
	owner.update_icons()

/datum/action/ability/xeno_action/dragon_flight/action_activate()
	// Both: General Variables
	var/area/current_area = get_area(owner)
	var/is_outside = current_area.outside
	var/cast_time = is_in_flight ? DRAGON_FLIGHT_LAND_TIME : DRAGON_FLIGHT_FLIGHT_TIME
	if(!is_outside)
		cast_time *= 1.5

	// Both: Introduction Sound
	play_introduction_sound()

	// Landing: Preparation & Telegraphing
	if(is_in_flight)
		targetted_turfs = get_landing_turfs()
		for(var/turf/targetted_turf in targetted_turfs)
			telegraph_effects += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

	// Both: Casting
	ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
	if(!is_in_flight)
		REMOVE_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)
	var/successfully_casted = do_after(owner, cast_time, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
	if(!is_in_flight)
		ADD_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)

	// Both: Casting Success/Fail
	for(var/obj/effect/warning in telegraph_effects)
		qdel(warning)

	deltimer(intro_sound_timer_id)
	intro_sound_timer_id = null

	if(!successfully_casted)
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()
	else
		is_in_flight = !is_in_flight

	// Flight: Now flying!
	if(is_in_flight)
		RegisterSignal(owner, COMSIG_MOVABLE_PRE_MOVE, PROC_REF(on_pre_move))
		set_flight_variables()
		add_cooldown()
		succeed_activate()
		return

	// Landing: Now landing!
	var/mob/living/carbon/xenomorph/xeno_owner = owner
	var/turf/current_turf = get_turf(xeno_owner)
	var/land_damage = DRAGON_FLIGHT_LAND_DAMAGE * xeno_owner.xeno_melee_damage_modifier
	var/living_got_stomped = FALSE // To prevent multiple splat sounds.
	for(var/turf/targetted_turf in targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf)
			if(!(attacked_atom.resistance_flags & XENO_DAMAGEABLE))
				continue
			if(isxeno(attacked_atom))
				continue
			if(isobj(attacked_atom))
				var/obj/attacked_obj = attacked_atom
				if(ishitbox(attacked_obj))
					attacked_obj.take_damage(land_damage / 4.5, BRUTE, MELEE, blame_mob = xeno_owner)
					continue
				if(isarmoredvehicle(attacked_obj))
					var/obj/vehicle/sealed/armored/attacked_apc = attacked_obj
					attacked_apc.take_damage(land_damage / 4.5, BRUTE, MELEE, blame_mob = xeno_owner)
					if(targetted_turf == current_turf) // Extra punishment for getting directly stomped on.
						for(var/mob/living/living_passenger AS in attacked_apc.occupants)
							living_passenger.Knockdown(DRAGON_FLIGHT_KNOCKDOWN)
							to_chat(living_passenger, "You fall to the floor from an impact hitting the vehicle!")
					continue
				if(isvehicle(attacked_obj))
					attacked_obj.take_damage(land_damage * 2, BRUTE, MELEE, blame_mob = xeno_owner)
					continue
			if(isliving(attacked_atom))
				var/mob/living/attacked_living = attacked_atom
				if(attacked_living.stat == DEAD)
					continue
				shake_camera(attacked_living, 2, 1)
				attacked_living.Knockdown(DRAGON_FLIGHT_KNOCKDOWN)
				if(targetted_turf != current_turf)
					attacked_living.apply_damage(land_damage, BRUTE, blocked = MELEE, updating_health = TRUE)
					continue
				attacked_living.apply_damage(land_damage * 2, BRUTE, blocked = MELEE, updating_health = TRUE)
				attacked_living.emote("scream")
				living_got_stomped = TRUE

	// Landing: Indoors ceiling loot.
	if(!is_outside)
		current_turf.ceiling_debris(1)

	// Landing: Conclusion Sound
	playsound(owner, 'sound/effects/slam3.ogg', 70, sound_range = 20)
	if(living_got_stomped)
		playsound(owner, 'sound/effects/splat.ogg', 70, sound_range = 20)

	UnregisterSignal(owner, COMSIG_MOVABLE_PRE_MOVE)
	set_flight_variables()
	add_cooldown()
	succeed_activate()

/// Get nearby visible turfs of a certain range.
/datum/action/ability/xeno_action/dragon_flight/proc/get_landing_turfs()
	var/list/turf/nearby_visible_turfs = list()
	for(var/turf/nearby_turf in range(DRAGON_FLIGHT_FLIGHT_RANGE, get_turf(owner)))
		if(nearby_turf.density)
			continue
		if(!line_of_sight(owner, nearby_turf))
			continue
		nearby_visible_turfs += nearby_turf
	return nearby_visible_turfs

/// Plays the sound associated with beginning flight / landing and repeats until stopped.
/datum/action/ability/xeno_action/dragon_flight/proc/play_introduction_sound()
	SIGNAL_HANDLER
	playsound(owner, is_in_flight ? 'sound/effects/alien/behemoth/landslide_roar.ogg' : 'sound/effects/shieldbash.ogg' , 70, sound_range = 20)
	intro_sound_timer_id = addtimer(CALLBACK(src, PROC_REF(play_introduction_sound)), 2 SECONDS, TIMER_STOPPABLE)

/// Sets various variables associated with being in flight or not.
/datum/action/ability/xeno_action/dragon_flight/proc/set_flight_variables()
	if(is_in_flight)
		owner.alpha = 50
		owner.density = FALSE
		owner.status_flags |= (GODMODE|INCORPOREAL)
		owner.resistance_flags |= RESIST_ALL
		owner.allow_pass_flags = PASSABLE
		owner.pass_flags = HOVERING
		return
	owner.alpha = initial(owner.alpha)
	owner.density = initial(owner.density)
	owner.status_flags &= ~(GODMODE|INCORPOREAL)
	owner.resistance_flags &= ~RESIST_ALL
	owner.allow_pass_flags = initial(owner.allow_pass_flags)
	owner.pass_flags = initial(owner.pass_flags)

/// Checks if the owner is groundside.
/datum/action/ability/xeno_action/dragon_flight/proc/is_groundside()
	var/list/ground_z_levels = SSmapping.levels_by_trait(ZTRAIT_GROUND)
	for(var/ground_z in ground_z_levels)
		if(owner.z == ground_z)
			return TRUE
	return FALSE

/// Prevents movement in certain areas while flying.
/datum/action/ability/xeno_action/dragon_flight/proc/on_pre_move(datum/source, atom/newloc, var/direction)
	SIGNAL_HANDLER
	var/area/new_area = get_area(newloc)
	if(new_area.ceiling > CEILING_OBSTRUCTED)
		return COMPONENT_MOVABLE_BLOCK_PRE_MOVE

/datum/action/ability/xeno_action/dragon_flight/can_use_action(silent = FALSE, override_flags)
	silent = TRUE
	. = ..()
	silent = TRUE
	var/area/current_area = get_area(owner)
	if(!is_in_flight)
		// Roofed areas.
		if(current_area.ceiling > CEILING_OBSTRUCTED)
			if(!silent)
				owner.balloon_alert(owner, "Can't fly in caves!")
			return FALSE
		// Shipside (aka not groundside).
		if(!is_groundside())
			if(!silent)
				owner.balloon_alert(owner, "Cannot fly shipside.")
			return FALSE
	else
		// Marine-friendly areas.
		if(isdropshiparea(current_area) || (current_area.area_flags & MARINE_BASE))
			if(!silent)
				owner.balloon_alert(owner, "No landing in marine base!")
			return FALSE


/datum/action/ability/xeno_action/dragon_flight/succeed_activate(ability_cost_override)
	..()
	owner.update_icons()

/datum/action/ability/xeno_action/dragon_flight/action_activate()
	// Both: General Variables
	var/area/current_area = get_area(owner)
	var/is_outside = current_area.outside
	var/cast_time = is_in_flight ? DRAGON_FLIGHT_LAND_TIME : DRAGON_FLIGHT_FLIGHT_TIME
	if(!is_outside)
		cast_time *= 1.5

	// Both: Introduction Sound
	play_introduction_sound()

	// Landing: Preparation & Telegraphing
	if(is_in_flight)
		targetted_turfs = get_landing_turfs()
		for(var/turf/targetted_turf in targetted_turfs)
			telegraph_effects += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

	// Both: Casting
	ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
	if(!is_in_flight)
		REMOVE_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)
	var/successfully_casted = do_after(owner, cast_time, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
	if(!is_in_flight)
		ADD_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)

	// Both: Casting Success/Fail
	for(var/obj/effect/warning in telegraph_effects)
		qdel(warning)

	deltimer(intro_sound_timer_id)
	intro_sound_timer_id = null

	if(!successfully_casted)
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()
	else
		is_in_flight = !is_in_flight

	// Flight: Now flying!
	if(is_in_flight)
		RegisterSignal(owner, COMSIG_MOVABLE_PRE_MOVE, PROC_REF(on_pre_move))
		set_flight_variables()
		add_cooldown()
		succeed_activate()
		return

	// Landing: Now landing!
	var/mob/living/carbon/xenomorph/xeno_owner = owner
	var/turf/current_turf = get_turf(xeno_owner)
	var/land_damage = DRAGON_FLIGHT_LAND_DAMAGE * xeno_owner.xeno_melee_damage_modifier
	var/living_got_stomped = FALSE // To prevent multiple splat sounds.
	for(var/turf/targetted_turf in targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf)
			if(!(attacked_atom.resistance_flags & XENO_DAMAGEABLE))
				continue
			if(isxeno(attacked_atom))
				continue
			if(isobj(attacked_atom))
				var/obj/attacked_obj = attacked_atom
				if(ishitbox(attacked_obj))
					attacked_obj.take_damage(land_damage / 4.5, BRUTE, MELEE, blame_mob = xeno_owner)
					continue
				if(isarmoredvehicle(attacked_obj))
					var/obj/vehicle/sealed/armored/attacked_apc = attacked_obj
					attacked_apc.take_damage(land_damage / 4.5, BRUTE, MELEE, blame_mob = xeno_owner)
					if(targetted_turf == current_turf) // Extra punishment for getting directly stomped on.
						for(var/mob/living/living_passenger AS in attacked_apc.occupants)
							living_passenger.Knockdown(DRAGON_FLIGHT_KNOCKDOWN)
							to_chat(living_passenger, "You fall to the floor from an impact hitting the vehicle!")
					continue
				if(isvehicle(attacked_obj))
					attacked_obj.take_damage(land_damage * 2, BRUTE, MELEE, blame_mob = xeno_owner)
					continue
			if(isliving(attacked_atom))
				var/mob/living/attacked_living = attacked_atom
				if(attacked_living.stat == DEAD)
					continue
				shake_camera(attacked_living, 2, 1)
				attacked_living.Knockdown(DRAGON_FLIGHT_KNOCKDOWN)
				if(targetted_turf != current_turf)
					attacked_living.apply_damage(land_damage, BRUTE, blocked = MELEE, updating_health = TRUE)
					continue
				attacked_living.apply_damage(land_damage * 2, BRUTE, blocked = MELEE, updating_health = TRUE)
				attacked_living.emote("scream")
				living_got_stomped = TRUE

	// Landing: Indoors ceiling loot.
	if(!is_outside)
		current_turf.ceiling_debris(1)

	// Landing: Conclusion Sound
	playsound(owner, 'sound/effects/slam3.ogg', 70, sound_range = 20)
	if(living_got_stomped)
		playsound(owner, 'sound/effects/splat.ogg', 70, sound_range = 20)

	UnregisterSignal(owner, COMSIG_MOVABLE_PRE_MOVE)
	set_flight_variables()
	add_cooldown()
	succeed_activate()

/// Handles all variables that need to be set upon landing or flight.
/datum/action/ability/xeno_action/dragon_flight/proc/handle_flight_variables()
	if(flying)
		owner.status_flags |= (GODMODE|INCORPOREAL)
		owner.resistance_flags |= RESIST_ALL
		owner.pass_flags = HOVERING|PASSABLE|PASS_XENO
		return
	owner.status_flags &= (GODMODE|INCORPOREAL)
	owner.resistance_flags &= RESIST_ALL
	owner.pass_flags = initial(owner.pass_flags)

/// Get nearby visible turfs of a certain range.
/datum/action/ability/xeno_action/dragon_flight/proc/get_landing_turfs()
	var/list/turf/nearby_visible_turfs = list()
	for(var/turf/nearby_turf in range(DRAGON_FLIGHT_FLIGHT_RANGE, get_turf(owner)))
		if(nearby_turf.density)
			continue
		if(!line_of_sight(owner, nearby_turf))
			continue
		nearby_visible_turfs += nearby_turf
	return nearby_visible_turfs

/// Plays the sound associated with beginning flight / landing and repeats until stopped.
/datum/action/ability/xeno_action/dragon_flight/proc/play_introduction_sound()
	SIGNAL_HANDLER
	playsound(owner, is_in_flight ? 'sound/effects/alien/behemoth/landslide_roar.ogg' : 'sound/effects/shieldbash.ogg' , 70, sound_range = 20)
	intro_sound_timer_id = addtimer(CALLBACK(src, PROC_REF(play_introduction_sound)), 2 SECONDS, TIMER_STOPPABLE)

/// Sets various variables associated with being in flight or not.
/datum/action/ability/xeno_action/dragon_flight/proc/set_flight_variables()
	if(is_in_flight)
		owner.alpha = 50
		owner.density = FALSE
		owner.status_flags |= (GODMODE|INCORPOREAL)
		owner.resistance_flags |= RESIST_ALL
		owner.allow_pass_flags = PASSABLE
		owner.pass_flags = HOVERING
		return
	owner.alpha = initial(owner.alpha)
	owner.density = initial(owner.density)
	owner.status_flags &= ~(GODMODE|INCORPOREAL)
	owner.resistance_flags &= ~RESIST_ALL
	owner.allow_pass_flags = initial(owner.allow_pass_flags)
	owner.pass_flags = initial(owner.pass_flags)

/// Checks if the owner is groundside.
/datum/action/ability/xeno_action/dragon_flight/proc/is_groundside()
	var/list/ground_z_levels = SSmapping.levels_by_trait(ZTRAIT_GROUND)
	for(var/ground_z in ground_z_levels)
		if(owner.z == ground_z)
			return TRUE
	return FALSE

/// Prevents movement in certain areas while flying.
/datum/action/ability/xeno_action/dragon_flight/proc/on_pre_move(datum/source, atom/newloc, var/direction)
	SIGNAL_HANDLER
	var/area/new_area = get_area(newloc)
	if(new_area.ceiling > CEILING_OBSTRUCTED)
		return COMPONENT_MOVABLE_BLOCK_PRE_MOVE


>>>>>>> c80747b8b1 (ill get back to this)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
// ***************************************
// *********** Dragon's Breath
// ***************************************

#define DRAGON_BREATH_RANGE 5
#define DRAGON_BREATH_ANGLE 90
#define DRAGON_BREATH_SPEED 2
#define DRAGON_BREATH_DAMAGE 20
#define DRAGON_BREATH_VEHICLE_DAMAGE 10
#define DRAGON_BREATH_STACKS 10
#define DRAGON_BREATH_CHARGE_TIME 2 SECONDS

// This acts similar to the King's Shattering Roar, except it is fire-themed!
/datum/action/ability/activable/xeno/dragon_breath
	name = "Dragon Breath"
	action_icon_state = "shattering_roar"
	action_icon = 'icons/Xeno/actions/king.dmi'

// ***************************************
// *********** Dragon's Breath
// ***************************************

// This acts similar to the King's Shattering Roar, except it is fire-themed!
/datum/action/ability/activable/xeno/dragon_breath
	name = "Dragon Breath"
	action_icon_state = "shattering_roar"
	action_icon = 'icons/Xeno/actions/king.dmi'
>>>>>>> f5bda68175 (dragon breath)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	desc = "Unleash a massive cone of fire in front of you after a wind-up."
	ability_cost = 1
	cooldown_duration = 12 SECONDS
	target_flags = ABILITY_TURF_TARGET
	keybinding_signals = list(
		KEYBINDING_NORMAL = COMSIG_XENOABILITY_DRAGON_BREATH,
	)
	/// Tracks victims to avoid double-hits in case they flee backwards into the incoming fire.
	var/list/victims_hit = list()

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

<<<<<<< HEAD
=======

>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======

>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
/datum/action/ability/activable/xeno/dragon_breath/can_use_action(silent = FALSE, override_flags)
	. = ..()
	var/mob/living/carbon/xenomorph/dragon/dragon_owner = owner
	if(!dragon_owner)
		return FALSE
	if(dragon_owner.is_flying)
		if(!silent)
			dragon_owner.balloon_alert(src, "Can't while flying!")
		return FALSE

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> b1fc44b9df (3/4 of dragon flight)
/datum/action/ability/activable/xeno/dragon_breath/use_ability(atom/target)
	var/mob/living/carbon/xenomorph/dragon/dragon_owner = owner
=======
/datum/action/ability/activable/xeno/dragon_breath/use_ability(atom/target)
>>>>>>> f5bda68175 (dragon breath)
	if(!target)
		return

<<<<<<< HEAD
	dragon_owner.face_atom(target)
	playsound(dragon_owner, 'sound/voice/alien/king_roar.ogg', 70, sound_range = 20)

	ADD_TRAIT(dragon_owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)
	var/successful = do_after(dragon_owner, DRAGON_BREATH_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE))
=======
	playsound(owner, 'sound/voice/alien/king_roar.ogg', 70, sound_range = 20)
	ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)

	var/successful = do_after(owner, DRAGON_BREATH_CHARGE_TIME, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
>>>>>>> f5bda68175 (dragon breath)
=======
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
/datum/action/ability/activable/xeno/dragon_breath/use_ability(atom/target)
	var/mob/living/carbon/xenomorph/dragon/dragon_owner = owner
	if(!target)
		return

	dragon_owner.face_atom(target)
	playsound(dragon_owner, 'sound/voice/alien/king_roar.ogg', 70, sound_range = 20)

<<<<<<< HEAD
<<<<<<< HEAD
	dragon_owner.is_busy = TRUE
	var/successful = do_after(dragon_owner, DRAGON_BREATH_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	dragon_owner.is_busy = FALSE
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
	ADD_TRAIT(dragon_owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)
	var/successful = do_after(dragon_owner, DRAGON_BREATH_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE))
>>>>>>> b0d4c5eb57 (dont actually need this)
=======
	ADD_TRAIT(dragon_owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)
	var/successful = do_after(dragon_owner, DRAGON_BREATH_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE))
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)

	// They generally shouldn't be interrupted given that they are immune to stun and stagger, but if they are interrupted:
	if(!successful)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
		dragon_owner.balloon_alert(dragon_owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	playsound(dragon_owner, 'sound/voice/alien/xenos_roaring.ogg', 90, sound_range = 30)
	for(var/mob/living/carbon/human/human_victim AS in GLOB.humans_by_zlevel["[dragon_owner.z]"])
		if(get_dist(human_victim, dragon_owner) > 9)
<<<<<<< HEAD
=======
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	playsound(owner, 'sound/voice/alien/xenos_roaring.ogg', 90, sound_range = 30)
	for(var/mob/living/carbon/human/human_victim AS in GLOB.humans_by_zlevel["[owner.z]"])
		if(get_dist(human_victim, owner) > 9)
>>>>>>> f5bda68175 (dragon breath)
=======
		dragon_owner.balloon_alert(dragon_owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	playsound(dragon_owner, 'sound/voice/alien/xenos_roaring.ogg', 90, sound_range = 30)
	for(var/mob/living/carbon/human/human_victim AS in GLOB.humans_by_zlevel["[dragon_owner.z]"])
		if(get_dist(human_victim, dragon_owner) > 9)
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
			continue
		shake_camera(human_victim, 2 SECONDS, 1)

	var/source = get_turf(dragon_owner)
	var/dir_to_target = Get_Angle(source, target)
	var/list/turf/turfs_to_attack = generate_true_cone(source, DRAGON_BREATH_RANGE, 1, DRAGON_BREATH_ANGLE, dir_to_target, bypass_window = TRUE, air_pass = TRUE)
	execute_attack(1, turfs_to_attack, DRAGON_BREATH_RANGE, target, source)

	add_cooldown()
	succeed_activate()

/// Carries out the attack iteratively based on distance from source.
/datum/action/ability/activable/xeno/dragon_breath/proc/execute_attack(iteration, list/turf/turfs_to_attack, range, target, turf/source)
	if(iteration > range)
		victims_hit.Cut()
		return

	for(var/turf/turf AS in turfs_to_attack)
		if(get_dist(turf, source) == iteration || get_dist(turf, source) == iteration - 1)
			attack_turf(turf, LERP(1, 0.3, iteration / DRAGON_BREATH_RANGE))

	iteration++
	addtimer(CALLBACK(src, PROC_REF(execute_attack), iteration, turfs_to_attack, range, target, source), DRAGON_BREATH_SPEED)

/// Creates melting fire in every turf and applies damage/effects as if directly hit by a Pyrogen fireball with max stacks.
/datum/action/ability/activable/xeno/dragon_breath/proc/attack_turf(turf/attacked_turf, severity)
	new /obj/fire/melting_fire(attacked_turf)
	for(var/atom/movable/attacked_atom AS in attacked_turf)
		if(isxeno(attacked_atom))
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
			continue
		if(iscarbon(attacked_atom))
			var/mob/living/carbon/attacked_carbon = attacked_atom
			attacked_carbon.take_overall_damage(DRAGON_BREATH_DAMAGE, BURN, FIRE, FALSE, FALSE, TRUE, 0, 2)
			var/datum/status_effect/stacking/melting_fire/debuff = attacked_carbon.has_status_effect(STATUS_EFFECT_MELTING_FIRE)
			if(debuff)
				debuff.add_stacks(DRAGON_BREATH_STACKS)
			else
				attacked_carbon.apply_status_effect(STATUS_EFFECT_MELTING_FIRE, DRAGON_BREATH_STACKS)
			continue
		if(ishitbox(attacked_atom))
			var/obj/attacked_obj = attacked_atom
			attacked_obj.take_damage(DRAGON_BREATH_VEHICLE_DAMAGE, BURN, FIRE)
			continue

// ***************************************
// *********** Tail Swipe
// ***************************************

#define TAIL_SWIPE_DAMAGE 75
#define TAIL_SWIPE_KNOCKDOWN 1 SECONDS
#define TAIL_SWIPE_STAGGER 2 SECONDS
#define TAIL_SWIPE_SLOWDOWN 0.6
#define TAIL_SWIPE_CHARGE_TIME 1.5 SECONDS

/datum/action/ability/activable/xeno/tail_swipe
	name = "Tail Swipe"
	action_icon_state = "shattering_roar"
	action_icon = 'icons/Xeno/actions/king.dmi'
	desc = "Swipes your tail behind you and knockdowns in a massive range."
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
	ability_cost = 1
	cooldown_duration = 12 SECONDS
=======
	ability_cost = 0 // 1
	cooldown_duration = 1 SECONDS //12 SECONDS
>>>>>>> 69bd3761e7 (tail swipe)
=======
	ability_cost = 1
	cooldown_duration = 12 SECONDS
>>>>>>> 52d1bd1aa7 (non-dev cost/cooldown)
=======
	ability_cost = 1
	cooldown_duration = 12 SECONDS
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	target_flags = ABILITY_TURF_TARGET
	keybinding_signals = list(
		KEYBINDING_NORMAL = COMSIG_XENOABILITY_TAIL_SWIPE,
	)

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
/datum/action/ability/activable/xeno/tail_swipe/can_use_action(silent = FALSE, override_flags)
	. = ..()
	var/mob/living/carbon/xenomorph/dragon/dragon_owner = owner
	if(!dragon_owner)
		return FALSE
	if(dragon_owner.is_flying)
		if(!silent)
			dragon_owner.balloon_alert(src, "Can't while flying!")
		return FALSE

/datum/action/ability/activable/xeno/tail_swipe/use_ability(atom/target)
	var/mob/living/carbon/xenomorph/dragon/dragon_owner = owner
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
/datum/action/ability/activable/xeno/tail_swipe/use_ability(atom/target)
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	if(!target)
		return

	// Turn to get the turfs in front of us.
<<<<<<< HEAD
<<<<<<< HEAD
	dragon_owner.face_atom(target)
=======
	owner.face_atom(target)
>>>>>>> 69bd3761e7 (tail swipe)
=======
	if(dragon_owner.is_busy || !target)
=======
	if(!target)
>>>>>>> b0d4c5eb57 (dont actually need this)
		return

	// Turn to get the turfs in front of us.
	dragon_owner.face_atom(target)
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
	dragon_owner.face_atom(target)
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291

	// Inner targetted turfs.
	var/list/turf/inner_targetted_turfs = list()
	var/list/turf/outer_targetted_turfs = list()

	// Column 1
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	var/turf/inner_north_turf = get_step(dragon_owner, turn(dragon_owner.dir, -90))
	var/turf/outer_north_turf = get_step(inner_north_turf, turn(dragon_owner.dir, -90))
	var/turf/inner_south_turf = get_step(dragon_owner, turn(dragon_owner.dir, 90))
	var/turf/outer_south_turf = get_step(inner_south_turf, turn(dragon_owner.dir, 90))
<<<<<<< HEAD
<<<<<<< HEAD
=======
	var/turf/inner_north_turf = get_step(owner, turn(owner.dir, -90))
	var/turf/outer_north_turf = get_step(inner_north_turf, turn(owner.dir, -90))
	var/turf/inner_south_turf = get_step(owner, turn(owner.dir, 90))
	var/turf/outer_south_turf = get_step(inner_south_turf, turn(owner.dir, 90))
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	inner_targetted_turfs += inner_north_turf
	outer_targetted_turfs += outer_north_turf
	inner_targetted_turfs += inner_south_turf
	outer_targetted_turfs += outer_south_turf

	// Column 2
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	var/turf/outer_north_turf_1 = get_step(outer_north_turf, dragon_owner.dir)
	var/turf/inner_north_corner_turf = get_step(inner_north_turf, dragon_owner.dir)
	var/turf/inner_behind_turf = get_step(dragon_owner, dragon_owner.dir)
	var/turf/inner_south_corner_turf = get_step(inner_south_turf, dragon_owner.dir)
	var/turf/outer_south_turf_1 = get_step(outer_south_turf, dragon_owner.dir)
<<<<<<< HEAD
<<<<<<< HEAD
=======
	var/turf/outer_north_turf_1 = get_step(outer_north_turf, owner.dir)
	var/turf/inner_north_corner_turf = get_step(inner_north_turf, owner.dir)
	var/turf/inner_behind_turf = get_step(owner, owner.dir)
	var/turf/inner_south_corner_turf = get_step(inner_south_turf, owner.dir)
	var/turf/outer_south_turf_1 = get_step(outer_south_turf, owner.dir)
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	outer_targetted_turfs += outer_north_turf_1
	inner_targetted_turfs += inner_north_corner_turf
	inner_targetted_turfs += inner_behind_turf
	inner_targetted_turfs += inner_south_corner_turf
	outer_targetted_turfs += outer_south_turf_1

	// Column 3
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	var/turf/outer_north_corner_turf = get_step(outer_north_turf_1, dragon_owner.dir)
	var/turf/outer_behind_north_turf = get_step(inner_north_corner_turf, dragon_owner.dir)
	var/turf/outer_behind_turf = get_step(inner_behind_turf, dragon_owner.dir)
	var/turf/outer_behind_south_turf = get_step(inner_south_corner_turf, dragon_owner.dir)
	var/turf/outer_south_corner_turf = get_step(outer_south_turf_1, dragon_owner.dir)
<<<<<<< HEAD
<<<<<<< HEAD
=======
	var/turf/outer_north_corner_turf = get_step(outer_north_turf_1, owner.dir)
	var/turf/outer_behind_north_turf = get_step(inner_north_corner_turf, owner.dir)
	var/turf/outer_behind_turf = get_step(inner_behind_turf, owner.dir)
	var/turf/outer_behind_south_turf = get_step(inner_south_corner_turf, owner.dir)
	var/turf/outer_south_corner_turf = get_step(outer_south_turf_1, owner.dir)
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	outer_targetted_turfs += outer_north_corner_turf
	outer_targetted_turfs += outer_behind_north_turf
	outer_targetted_turfs += outer_behind_turf
	outer_targetted_turfs += outer_behind_south_turf
	outer_targetted_turfs += outer_south_corner_turf

	// Then turn us around so it looks like we're using our tail.
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
	dragon_owner.setDir(turn(dragon_owner.dir, 180))
=======
	owner.setDir(turn(owner.dir, 180))
>>>>>>> 69bd3761e7 (tail swipe)
=======
	dragon_owner.setDir(turn(dragon_owner.dir, 180))
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
	dragon_owner.setDir(turn(dragon_owner.dir, 180))
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291

	// Telegraph this attack.
	var/list/obj/effect/dragon_telegraphed_warning/warnings = list()
	for(var/turf/targetted_turf in inner_targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)
	for(var/turf/targetted_turf in outer_targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	ADD_TRAIT(dragon_owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)
	var/successful = do_after(dragon_owner, TAIL_SWIPE_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE))
	for(var/obj/effect/warning in warnings)
		qdel(warning)
	REMOVE_TRAIT(dragon_owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)

	// They generally shouldn't be interrupted given that they are immune to stun and stagger, but if they are interrupted:
	if(!successful)
		dragon_owner.balloon_alert(dragon_owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

<<<<<<< HEAD
=======
	ADD_TRAIT(owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)
=======
	ADD_TRAIT(dragon_owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)
<<<<<<< HEAD

	dragon_owner.is_busy = TRUE
	var/successful = do_after(dragon_owner, TAIL_SWIPE_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	dragon_owner.is_busy = FALSE
>>>>>>> 1d39b17272 (7/8 of dragon flight)

=======
	var/successful = do_after(dragon_owner, TAIL_SWIPE_CHARGE_TIME, NONE, dragon_owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE))
>>>>>>> b0d4c5eb57 (dont actually need this)
	for(var/obj/effect/warning in warnings)
		qdel(warning)
	REMOVE_TRAIT(dragon_owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)

	// They generally shouldn't be interrupted given that they are immune to stun and stagger, but if they are interrupted:
	if(!successful)
		dragon_owner.balloon_alert(dragon_owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

<<<<<<< HEAD
	var/mob/living/carbon/xenomorph/xeno_owner = owner
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
	var/anyone_hit = FALSE
	for(var/turf/targetted_turf in inner_targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf)
			if(isxeno(attacked_atom))
				continue
			if(iscarbon(attacked_atom))
				var/mob/living/carbon/attacked_carbon = attacked_atom
				if(attacked_carbon.stat == DEAD)
					continue
				shake_camera(attacked_carbon, 2, 1)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN)
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * dragon_owner.xeno_melee_damage_modifier, BRUTE, blocked = MELEE, updating_health = TRUE)
<<<<<<< HEAD
=======
				playsound(attacked_carbon, 'sound/weapons/alien_claw_block.ogg', 50, 1)
=======
>>>>>>> 5d82b81846 (no double sound)
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN)
<<<<<<< HEAD
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * xeno_owner.xeno_melee_damage_modifier, BRUTE, blocked = MELEE, updating_health = TRUE)
>>>>>>> 69bd3761e7 (tail swipe)
=======
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * dragon_owner.xeno_melee_damage_modifier, BRUTE, blocked = MELEE, updating_health = TRUE)
>>>>>>> 1d39b17272 (7/8 of dragon flight)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
				anyone_hit = TRUE

	for(var/turf/targetted_turf in outer_targetted_turfs)
		for(var/atom/movable/attacked_atom AS in targetted_turf)
			if(isxeno(attacked_atom))
				continue
			if(iscarbon(attacked_atom))
				var/mob/living/carbon/attacked_carbon = attacked_atom
				if(attacked_carbon.stat == DEAD)
					continue
				shake_camera(attacked_carbon, 2, 1)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN/2)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER/2)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN/2)
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * dragon_owner.xeno_melee_damage_modifier/2, BRUTE, blocked = MELEE, updating_health = TRUE)
				anyone_hit = TRUE

	// Hit or miss sound effects!
	if(anyone_hit)
		playsound(dragon_owner, 'sound/weapons/alien_claw_block.ogg', 50, 1)
	else
		dragon_owner.emote("tail2")

	succeed_activate()
	add_cooldown()
<<<<<<< HEAD
=======
			continue
		if(iscarbon(attacked_atom))
			var/mob/living/carbon/attacked_carbon = attacked_atom
			attacked_carbon.take_overall_damage(DRAGON_BREATH_DAMAGE, BURN, FIRE, FALSE, FALSE, TRUE, 0, 2)
			var/datum/status_effect/stacking/melting_fire/debuff = attacked_carbon.has_status_effect(STATUS_EFFECT_MELTING_FIRE)
			if(debuff)
				debuff.add_stacks(DRAGON_BREATH_STACKS)
			else
				attacked_carbon.apply_status_effect(STATUS_EFFECT_MELTING_FIRE, DRAGON_BREATH_STACKS)
			continue
		if(ishitbox(attacked_atom))
			var/obj/attacked_obj = attacked_atom
			attacked_obj.take_damage(DRAGON_BREATH_VEHICLE_DAMAGE, BURN, FIRE)
			continue
>>>>>>> f5bda68175 (dragon breath)
=======
				playsound(attacked_carbon, 'sound/weapons/alien_claw_block.ogg', 50, 1)
=======
>>>>>>> 5d82b81846 (no double sound)
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN/2)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER/2)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN/2)
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * dragon_owner.xeno_melee_damage_modifier/2, BRUTE, blocked = MELEE, updating_health = TRUE)
				anyone_hit = TRUE

	// Hit or miss sound effects!
	if(anyone_hit)
		playsound(dragon_owner, 'sound/weapons/alien_claw_block.ogg', 50, 1)
	else
		dragon_owner.emote("tail2")

	succeed_activate()
	add_cooldown()
>>>>>>> 69bd3761e7 (tail swipe)
=======
>>>>>>> 9b7a3bd24b64bc7da5fb4691d3446eec96b06291
