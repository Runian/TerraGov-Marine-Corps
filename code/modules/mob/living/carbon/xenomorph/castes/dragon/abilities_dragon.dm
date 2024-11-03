#define DRAGON_BREATH_RANGE 4
#define DRAGON_BREATH_ANGLE 60
#define DRAGON_BREATH_SPEED 4
#define DRAGON_BREATH_DAMAGE 20
#define DRAGON_BREATH_VEHICLE_DAMAGE 10
#define DRAGON_BREATH_STACKS 10
#define DRAGON_BREATH_CHARGE_TIME 2.5 SECONDS

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
