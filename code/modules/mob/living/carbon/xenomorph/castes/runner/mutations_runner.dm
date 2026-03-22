//*********************//
//        Base        //
//*********************//

/datum/mutation_upgrade/defense/runner
	allowed_caste_names = list(/datum/xeno_caste/runner)

/datum/mutation_upgrade/offense/runner
	allowed_caste_names = list(/datum/xeno_caste/runner)

/datum/mutation_upgrade/utility/runner
	allowed_caste_names = list(/datum/xeno_caste/runner)

//*********************//
//       Defense       //
//*********************//

/datum/mutation_upgrade/defense/runner/borrowed_time
	name = "Borrowed Time"
	desc = "You can no longer enter critical. When you have negative health, you are staggered, heavily slowed, cannot slash attack instead."
	/// Is the debuff associated with being in critical currently applied?
	var/debuff_active = FALSE

/datum/mutation_upgrade/defense/runner/borrowed_time/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_LIVING_UPDATE_HEALTH, PROC_REF(on_health_update))
	xenomorph_owner.health_threshold_crit = xenomorph_owner.health_threshold_dead
	if(xenomorph_owner.health < 0)
		apply_debuff()

/datum/mutation_upgrade/defense/runner/borrowed_time/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_LIVING_UPDATE_HEALTH)
	xenomorph_owner.health_threshold_crit = initial(xenomorph_owner.health_threshold_crit)
	if(xenomorph_owner.health < 0)
		remove_debuff()

/// Depending on their new health, applies or remove the debuff.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/on_health_update(datum/source)
	SIGNAL_HANDLER
	var/health = (xenomorph_owner.status_flags & GODMODE) ? xenomorph_owner.maxHealth : (xenomorph_owner.maxHealth - xenomorph_owner.health)
	if(health <= xenomorph_owner.get_death_threshold())
		return
	if(xenomorph_owner.health < 0)
		apply_debuff()
		return
	remove_debuff()

/// Applies the debuff if it was not applied yet.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/apply_debuff()
	if(debuff_active)
		return
	debuff_active = TRUE
	ADD_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, MUTATION_TRAIT)
	ADD_TRAIT(xenomorph_owner, TRAIT_STAGGERED, MUTATION_TRAIT)
	xenomorph_owner.add_movespeed_modifier(MOVESPEED_ID_RUNNER_BORROWED_TIME, TRUE, 0, NONE, TRUE, 0.9)
	INVOKE_ASYNC(xenomorph_owner, TYPE_PROC_REF(/mob, emote), "roar")
	xenomorph_owner.balloon_alert(xenomorph_owner, "On borrowed time!");

/// Removes the debuff if it was not removed yet.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/remove_debuff()
	if(!debuff_active)
		return
	debuff_active = FALSE
	REMOVE_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, MUTATION_TRAIT)
	REMOVE_TRAIT(xenomorph_owner, TRAIT_STAGGERED, MUTATION_TRAIT)
	xenomorph_owner.remove_movespeed_modifier(MOVESPEED_ID_RUNNER_BORROWED_TIME)
	xenomorph_owner.updatehealth()
	xenomorph_owner.balloon_alert(xenomorph_owner, "Off borrowed time!");

/datum/mutation_upgrade/defense/runner/ingrained_evasion
	name = "Ingrained Evasion"
	desc = "You no longer have access to Evasion. With similar conditions to Evasion, you have a 40% to dodge enemy projectles and thrown items. Projectiles that have high accuracy are less likely to be dodged."
	/// After this amount of time since their last move, they will no longer dodge.
	var/movement_leniency = RUNNER_EVASION_RUN_DELAY
	/// If a projectile's accuracy is above this value, then it reduces the dodge chance by the amount above the value.
	var/accuracy_reduction_threshold = 75

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_gain()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(ability)
		ability.remove_action(xenomorph_owner)
	RegisterSignal(xenomorph_owner, COMSIG_XENO_PROJECTILE_HIT, PROC_REF(dodge_projectile))
	RegisterSignal(xenomorph_owner, COMSIG_PRE_MOVABLE_IMPACT, PROC_REF(dodge_thrown_item))

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_loss()
	var/datum/action/ability/xeno_action/evasion/ability = new()
	ability.give_action(xenomorph_owner)
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_PROJECTILE_HIT, COMSIG_PRE_MOVABLE_IMPACT))

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_xenomorph_upgrade()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(ability)
		ability.remove_action(xenomorph_owner)

// Checks if they can dodge at all.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/can_dodge()
	if(xenomorph_owner.stat != CONSCIOUS)
		return FALSE
	if(xenomorph_owner.IsStun())
		return FALSE
	if(xenomorph_owner.IsKnockdown())
		return FALSE
	if(xenomorph_owner.IsParalyzed())
		return FALSE
	if(xenomorph_owner.IsUnconscious())
		return FALSE
	if(xenomorph_owner.IsSleeping())
		return FALSE
	if(xenomorph_owner.IsStaggered())
		return FALSE
	if(xenomorph_owner.on_fire)
		return FALSE
	if((xenomorph_owner.last_move_time < (world.time - movement_leniency)))
		return FALSE
	return TRUE

/// Checks if they can dodge a projectile. If they can, they do so.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/dodge_projectile(datum/source, atom/movable/projectile/proj, cardinal_move, uncrossing)
	SIGNAL_HANDLER
	if(!can_dodge())
		return FALSE
	if(xenomorph_owner.issamexenohive(proj.firer))
		return COMPONENT_PROJECTILE_DODGE
	if(proj.ammo.ammo_behavior_flags & AMMO_FLAME) // We can't dodge literal fire.
		return FALSE
	if(proj.original_target == xenomorph_owner && proj.distance_travelled < 2) // Pointblank shot.
		return FALSE
	if(prob(proj.accuracy < accuracy_reduction_threshold ? 40 : max(0, 40 + accuracy_reduction_threshold - proj.accuracy)))
		dodge_fx(proj)
		return COMPONENT_PROJECTILE_DODGE
	return FALSE

/// Checks if they can dodge a thrown object. If they can, they do so.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/dodge_thrown_item(datum/source, atom/movable/thrown_atom)
	SIGNAL_HANDLER
	if(!isobj(thrown_atom) || !can_dodge())
		return FALSE
	if(prob(40))
		dodge_fx(thrown_atom)
		return COMPONENT_PRE_MOVABLE_IMPACT_DODGED
	return FALSE

/// Handles dodge effects and visuals.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/dodge_fx(atom/movable/proj)
	xenomorph_owner.visible_message(span_warning("[xenomorph_owner] effortlessly dodges the [proj.name]!"), span_xenodanger("We effortlessly dodge the [proj.name]!"))
	xenomorph_owner.add_filter("ingrained_evasion", 2, gauss_blur_filter(5))
	addtimer(CALLBACK(xenomorph_owner, TYPE_PROC_REF(/datum, remove_filter), "ingrained_evasion"), 0.5 SECONDS)
	xenomorph_owner.do_jitter_animation(4000)
	var/turf/current_turf = get_turf(xenomorph_owner)
	playsound(current_turf, pick('sound/effects/throw.ogg','sound/effects/alien/tail_swipe1.ogg', 'sound/effects/alien/tail_swipe2.ogg'), 25, 1) //sound effects
	var/obj/effect/temp_visual/after_image/after_image
	for(var/i = 0 to 2)
		after_image = new /obj/effect/temp_visual/after_image(current_turf, xenomorph_owner)
		after_image.pixel_x = pick(randfloat(xenomorph_owner.pixel_x * 3, xenomorph_owner.pixel_x * 1.5), rand(0, xenomorph_owner.pixel_x * -1))

/datum/mutation_upgrade/defense/runner/extended_evasion
	name = "Extended Evasion"
	desc = "Evasion starts with an additional 2 seconds, but cannot be refreshed to be longer."

/datum/mutation_upgrade/defense/runner/extended_evasion/on_gain()
	var/datum/action/ability/xeno_action/evasion/evasion = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!evasion)
		return
	evasion.automatic_refresh_togglable = FALSE
	evasion.starting_duration += 2 SECONDS
	if(evasion.automatic_refresh_toggled)
		evasion.toggle_automatic_refresh()

/datum/mutation_upgrade/defense/runner/extended_evasion/on_loss()
	var/datum/action/ability/xeno_action/evasion/evasion = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!evasion)
		return
	evasion.automatic_refresh_togglable = initial(evasion.automatic_refresh_togglable)
	evasion.starting_duration -= 2 SECONDS
	if(!evasion.automatic_refresh_toggled)
		evasion.toggle_automatic_refresh()

//*********************//
//       Offense       //
//*********************//

/datum/mutation_upgrade/offense/runner/duelist_evasion
	name = "Duelist Evasion"
	desc = "While Evasion is active, you gain the ability to slash. However, it will no longer dodge anything within 2 tiles of you."

/datum/mutation_upgrade/offense/runner/duelist_evasion/on_gain()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.dodge_prevention_range = 2
	ability.attacking_disabled = FALSE
	if(ability.active)
		REMOVE_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, RUNNER_ABILITY_TRAIT)

/datum/mutation_upgrade/offense/runner/duelist_evasion/on_loss()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.dodge_prevention_range = 2
	ability.attacking_disabled = TRUE
	if(ability.active)
		ADD_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, RUNNER_ABILITY_TRAIT)

/datum/mutation_upgrade/offense/runner/sneak_attack
	name = "Sneak Attack"
	desc = "If Pounce began in dim light, Savage will activate regardless of cooldown."

/datum/mutation_upgrade/offense/runner/sneak_attack/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_activates_in_darkness = TRUE

/datum/mutation_upgrade/offense/runner/sneak_attack/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_activates_in_darkness = FALSE

/datum/mutation_upgrade/offense/runner/mutilate
	name = "Mutilate"
	desc = "Savage now consumes all of your plasma, but gains an additional 0.1 damage per plasma you had."

/datum/mutation_upgrade/offense/runner/mutilate/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_plasma_conversion_rate += 0.1 // Ability will handle plasma consumption.

/datum/mutation_upgrade/offense/runner/mutilate/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_plasma_conversion_rate -= 0.1

//*********************//
//       Utility       //
//*********************//

/datum/mutation_upgrade/utility/runner/headslam
	name = "Head Slam"
	desc = "Pounce causes your target to be confused and blurs their vision for 3 seconds. You are no longer immobilized for using the ability, but your target's stun is reduced to a mini-stun."

/datum/mutation_upgrade/utility/runner/headslam/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.target_stun_duration = 0.1 SECONDS
	ability.self_immobilize_duration = 0 SECONDS
	ability.savage_debuff_amount = 3 SECONDS

/datum/mutation_upgrade/utility/runner/headslam/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.target_stun_duration = initial(ability.target_stun_duration)
	ability.self_immobilize_duration = initial(ability.self_immobilize_duration)
	ability.savage_debuff_amount = 0 SECONDS

/datum/mutation_upgrade/utility/runner/frenzy
	name = "Frenzy"
	desc = "Savage no longer deals damage, but applies a buff to you that lasts 7 seconds. The buff increases your melee damage by 1% for every point of damage Savage would of dealt."

/datum/mutation_upgrade/utility/runner/frenzy/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_damage_conversion_rate += 0.01

/datum/mutation_upgrade/utility/runner/frenzy/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_damage_conversion_rate -= 0.01

/datum/mutation_upgrade/utility/runner/passing_glance
	name = "Passing Glance"
	desc = "While Evasion is active, you gain the ability to pass through mobs. All humans that you stand ontop of will gain a malus of -25 accuracy for 3 seconds."

/datum/mutation_upgrade/utility/runner/passing_glance/on_gain()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_move))
	ability.pass_flags |= (PASS_MOB|PASS_XENO)
	if(ability.active)
		xenomorph_owner.allow_pass_flags |= ability.pass_flags
		xenomorph_owner.add_pass_flags(ability.pass_flags, ability.type)
	return ..()

/datum/mutation_upgrade/utility/runner/passing_glance/on_loss()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	UnregisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED)
	ability.pass_flags &= ~(PASS_MOB|PASS_XENO)
	if(ability.active)
		xenomorph_owner.allow_pass_flags &= ability.pass_flags
		xenomorph_owner.remove_pass_flags(ability.pass_flags, ability.type)
	return ..()

/// Upon moving on the same location as a human, apply the accuracy debuff to them.
/datum/mutation_upgrade/utility/runner/passing_glance/proc/on_move(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	if(xenomorph_owner.stat == DEAD)
		return FALSE
	for(var/mob/living/carbon/human/human_ontop_us in get_turf(xenomorph_owner))
		if(human_ontop_us.stat == DEAD)
			continue
		human_ontop_us.apply_status_effect(STATUS_EFFECT_GUN_SKILL_ACCURACY_DEBUFF, 3 SECONDS)
