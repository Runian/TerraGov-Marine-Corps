// ***************************************
// *********** Dragon's Flight
// ***************************************

#define DRAGON_FLIGHT_CHARGE_TIME 5 SECONDS

/datum/action/ability/xeno_action/dragon_flight
	name = "Dragon's Flight"
	action_icon_state = "shattering_roar"
	action_icon = 'icons/Xeno/actions/king.dmi'
	desc = "After a long wind-up, take flight to the skies. While flying, you slowly regenerate your health and choose where you'll land."
	cooldown_duration = 1 SECONDS // TODO: set to 60 SECONDS
	/// If we are currently in flight.
	var/flying = FALSE

/datum/action/ability/xeno_action/dragon_flight/can_use_action(silent = FALSE, override_flags)
	. = ..()
	var/area/current_area = get_area(owner)
	// No landing in marine-friendly areas.
	if(flying && (isdropshiparea(current_area) || current_area.area_flags & MARINE_BASE))
		if(!silent)
			owner.balloon_alert(owner, "No landing in marine base!")
		return FALSE
	// No flying or landing in caves.
	if(current_area.ceiling > CEILING_OBSTRUCTED)
		if(!silent)
			owner.balloon_alert(owner, flying ? "Can't land in caves!" : "Can't fly in caves!")
		return FALSE

/datum/action/ability/xeno_action/dragon_flight/action_activate()
	// We are landing!
	if(flying)
		playsound(owner, 'sound/effects/alien/behemoth/landslide_roar.ogg', 70, sound_range = 20)

		var/list/turf/nearby_visible_turfs = list()
		for(var/turf/nearby_turf in range(3, get_turf(owner)))
			if(nearby_turf.density)
				continue
			if(!line_of_sight(owner, nearby_turf))
				continue
			nearby_visible_turfs += nearby_turf

		// Telegraph this attack.
		var/list/obj/effect/dragon_telegraphed_warning/warnings = list()
		for(var/turf/targetted_turf in nearby_visible_turfs)
			warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

		// A long cast time for people to move away!
		ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
		var/successful = do_after(owner, DRAGON_FLIGHT_CHARGE_TIME, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
		for(var/obj/effect/warning in warnings)
			qdel(warning)
		REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)

		if(!successful)
			owner.balloon_alert(owner, "interrupted!")
			add_cooldown(cooldown_duration/2)
			return fail_activate()

		flying = FALSE
		handle_flight_variables()
		return

	// We want to fly instead!
	// TODO: Need better sounds!
	playsound(owner, 'sound/effects/shieldbash.ogg', 70, sound_range = 20)

	// A long cast time that must be committed to unless you're staggered.
	ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)
	REMOVE_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)
	var/successful = do_after(owner, DRAGON_FLIGHT_CHARGE_TIME, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	ADD_TRAIT(owner, TRAIT_STAGGERIMMUNE, XENO_TRAIT)
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_FLIGHT_ABILITY_TRAIT)

	if(!successful)
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	flying = TRUE
	handle_flight_variables()

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
	desc = "Unleash a massive cone of fire in front of you after a wind-up."
	ability_cost = 1
	cooldown_duration = 12 SECONDS
	target_flags = ABILITY_TURF_TARGET
	keybinding_signals = list(
		KEYBINDING_NORMAL = COMSIG_XENOABILITY_DRAGON_BREATH,
	)
	/// Tracks victims to avoid double-hits in case they flee backwards into the incoming fire.
	var/list/victims_hit = list()


/datum/action/ability/activable/xeno/dragon_breath/use_ability(atom/target)
	if(!target)
		return
	owner.dir = get_cardinal_dir(owner, target)

	playsound(owner, 'sound/voice/alien/king_roar.ogg', 70, sound_range = 20)
	ADD_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)

	var/successful = do_after(owner, DRAGON_BREATH_CHARGE_TIME, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, DRAGON_BREATH_ABILITY_TRAIT)

	// They generally shouldn't be interrupted given that they are immune to stun and stagger, but if they are interrupted:
	if(!successful)
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	playsound(owner, 'sound/voice/alien/xenos_roaring.ogg', 90, sound_range = 30)
	for(var/mob/living/carbon/human/human_victim AS in GLOB.humans_by_zlevel["[owner.z]"])
		if(get_dist(human_victim, owner) > 9)
			continue
		shake_camera(human_victim, 2 SECONDS, 1)

	var/source = get_turf(owner)
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
	ability_cost = 1
	cooldown_duration = 12 SECONDS
	target_flags = ABILITY_TURF_TARGET
	keybinding_signals = list(
		KEYBINDING_NORMAL = COMSIG_XENOABILITY_TAIL_SWIPE,
	)

/datum/action/ability/activable/xeno/tail_swipe/use_ability(atom/target)
	if(!target)
		return

	// Turn to get the turfs in front of us.
	owner.face_atom(target)

	// Inner targetted turfs.
	var/list/turf/inner_targetted_turfs = list()
	var/list/turf/outer_targetted_turfs = list()

	// Column 1
	var/turf/inner_north_turf = get_step(owner, turn(owner.dir, -90))
	var/turf/outer_north_turf = get_step(inner_north_turf, turn(owner.dir, -90))
	var/turf/inner_south_turf = get_step(owner, turn(owner.dir, 90))
	var/turf/outer_south_turf = get_step(inner_south_turf, turn(owner.dir, 90))
	inner_targetted_turfs += inner_north_turf
	outer_targetted_turfs += outer_north_turf
	inner_targetted_turfs += inner_south_turf
	outer_targetted_turfs += outer_south_turf

	// Column 2
	var/turf/outer_north_turf_1 = get_step(outer_north_turf, owner.dir)
	var/turf/inner_north_corner_turf = get_step(inner_north_turf, owner.dir)
	var/turf/inner_behind_turf = get_step(owner, owner.dir)
	var/turf/inner_south_corner_turf = get_step(inner_south_turf, owner.dir)
	var/turf/outer_south_turf_1 = get_step(outer_south_turf, owner.dir)
	outer_targetted_turfs += outer_north_turf_1
	inner_targetted_turfs += inner_north_corner_turf
	inner_targetted_turfs += inner_behind_turf
	inner_targetted_turfs += inner_south_corner_turf
	outer_targetted_turfs += outer_south_turf_1

	// Column 3
	var/turf/outer_north_corner_turf = get_step(outer_north_turf_1, owner.dir)
	var/turf/outer_behind_north_turf = get_step(inner_north_corner_turf, owner.dir)
	var/turf/outer_behind_turf = get_step(inner_behind_turf, owner.dir)
	var/turf/outer_behind_south_turf = get_step(inner_south_corner_turf, owner.dir)
	var/turf/outer_south_corner_turf = get_step(outer_south_turf_1, owner.dir)
	outer_targetted_turfs += outer_north_corner_turf
	outer_targetted_turfs += outer_behind_north_turf
	outer_targetted_turfs += outer_behind_turf
	outer_targetted_turfs += outer_behind_south_turf
	outer_targetted_turfs += outer_south_corner_turf

	// Then turn us around so it looks like we're using our tail.
	owner.setDir(turn(owner.dir, 180))

	// Telegraph this attack.
	var/list/obj/effect/dragon_telegraphed_warning/warnings = list()
	for(var/turf/targetted_turf in inner_targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)
	for(var/turf/targetted_turf in outer_targetted_turfs)
		warnings += new /obj/effect/dragon_telegraphed_warning(targetted_turf)

	ADD_TRAIT(owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)

	// Delete the warnings regardless of outcome.
	var/successful = do_after(owner, TAIL_SWIPE_CHARGE_TIME, NONE, owner, BUSY_ICON_DANGER, extra_checks = CALLBACK(src, PROC_REF(can_use_action), FALSE, ABILITY_USE_BUSY))
	for(var/obj/effect/warning in warnings)
		qdel(warning)

	REMOVE_TRAIT(owner, TRAIT_IMMOBILE, TAIL_SWIPE_ABILITY_TRAIT)

	// They generally shouldn't be interrupted given that they are immune to stun and stagger, but if they are interrupted:
	if(!successful)
		owner.balloon_alert(owner, "interrupted!")
		add_cooldown(cooldown_duration/2)
		return fail_activate()

	var/mob/living/carbon/xenomorph/xeno_owner = owner
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
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN)
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * xeno_owner.xeno_melee_damage_modifier, BRUTE, blocked = MELEE, updating_health = TRUE)
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
				attacked_carbon.Knockdown(TAIL_SWIPE_KNOCKDOWN/2)
				attacked_carbon.adjust_stagger(TAIL_SWIPE_STAGGER/2)
				attacked_carbon.adjust_slowdown(TAIL_SWIPE_SLOWDOWN/2)
				attacked_carbon.apply_damage(TAIL_SWIPE_DAMAGE * xeno_owner.xeno_melee_damage_modifier/2, BRUTE, blocked = MELEE, updating_health = TRUE)
				anyone_hit = TRUE

	// Miss or hit sound effect!
	if(anyone_hit)
		playsound(xeno_owner, 'sound/weapons/alien_claw_block.ogg', 50, 1)
	else
		xeno_owner.emote("tail2")

	succeed_activate()
	add_cooldown()
