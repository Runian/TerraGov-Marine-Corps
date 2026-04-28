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
/datum/mutation_upgrade/defense/runner/upfront_evasion
	name = "Upfront Evasion"
	desc = "Evasion last a second longer, but no longer can auto-refresh."
	required_abilities_types = list(
		/datum/action/ability/xeno_action/evasion
	)
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/defense/runner/ingrained_evasion
	)

/datum/mutation_upgrade/defense/runner/upfront_evasion/on_gain()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.evasion_starting_duration += 1 SECONDS
	ability.auto_evasion_togglable = FALSE
	if(ability.auto_evasion)
		return
	ability.alternate_action_activate() // Turning it off and giving them the notification it happened.

/datum/mutation_upgrade/defense/runner/upfront_evasion/on_loss()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.evasion_starting_duration -= 1 SECONDS
	ability.auto_evasion_togglable = initial(ability.auto_evasion_togglable)
	if(!ability.auto_evasion)
		return
	ability.alternate_action_activate() // Turning it on and giving them the notification it happened.

/datum/mutation_upgrade/defense/runner/borrowed_time
	name = "Borrowed Time"
	desc = "Your critical threshold is decreased by 100. While you have negative health, you are slowed, staggered and cannot slash attack. If you have negative health for more than 2 seconds, your critical threshold is increased back until you reach full health."
	/// Has the critical threshold been increased already?
	var/critical_threshold_boosted = FALSE
	/// The amount to decrease the critical threshold for.
	var/critical_threshold_amount = 100
	/// The timer id of the callback proc that will reverse the critical threshold.
	var/critical_threshold_timer
	/// The movement speed modifier to apply while they have negative health.
	var/movement_speed_modifier = 0.9

/datum/mutation_upgrade/defense/runner/borrowed_time/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_LIVING_UPDATE_HEALTH, PROC_REF(on_health_update))
	if(critical_threshold_boosted)
		return
	if(xenomorph_owner.maxHealth > xenomorph_owner.health)
		return
	toggle(TRUE)

/datum/mutation_upgrade/defense/runner/borrowed_time/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_LIVING_UPDATE_HEALTH)
	if(critical_threshold_timer)
		reverse_critical_threshold()
		return
	if(!critical_threshold_boosted)
		return
	toggle()

/// Increases or decreases the critical threshold amount for the owner.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/toggle(silent = FALSE)
	critical_threshold_boosted = !critical_threshold_boosted
	if(critical_threshold_boosted)
		if(!silent)
			xenomorph_owner.balloon_alert(xenomorph_owner, "Borrowed time ready!");
		xenomorph_owner.health_threshold_crit -= critical_threshold_amount
		return
	xenomorph_owner.health_threshold_crit += critical_threshold_amount

/// If their health is negative, activate it if possible. If it is full, let them activate it next time.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/on_health_update(datum/source)
	SIGNAL_HANDLER
	var/health = (xenomorph_owner.status_flags & GODMODE) ? xenomorph_owner.maxHealth : (xenomorph_owner.maxHealth - xenomorph_owner.getFireLoss() - xenomorph_owner.getBruteLoss())
	if(health <= xenomorph_owner.get_death_threshold())
		return // They're dead (and possibly gibbed) immediately after the signal is processed.
	if(!critical_threshold_boosted)
		if(health >= xenomorph_owner.maxHealth)
			toggle()
		return
	if(critical_threshold_timer || health > xenomorph_owner.get_crit_threshold() + critical_threshold_amount)
		return
	ADD_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, MUTATION_TRAIT)
	xenomorph_owner.add_movespeed_modifier(MOVESPEED_ID_RUNNER_BORROWED_TIME, TRUE, 0, NONE, TRUE, movement_speed_modifier)
	xenomorph_owner.Stagger(2 SECONDS)
	critical_threshold_timer = addtimer(CALLBACK(src, PROC_REF(reverse_critical_threshold)), 2 SECONDS, TIMER_UNIQUE|TIMER_STOPPABLE)
	INVOKE_ASYNC(xenomorph_owner, TYPE_PROC_REF(/mob, emote), "roar")
	xenomorph_owner.balloon_alert(xenomorph_owner, "On borrowed time!");

/// Effectively removes the effects of this mutation ands its active effect.
/datum/mutation_upgrade/defense/runner/borrowed_time/proc/reverse_critical_threshold()
	toggle()
	REMOVE_TRAIT(xenomorph_owner, TRAIT_HANDS_BLOCKED, MUTATION_TRAIT)
	xenomorph_owner.remove_movespeed_modifier(MOVESPEED_ID_RUNNER_BORROWED_TIME)
	deltimer(critical_threshold_timer)
	critical_threshold_timer = null
	xenomorph_owner.updatehealth()

/datum/mutation_upgrade/defense/runner/ingrained_evasion
	name = "Ingrained Evasion"
	desc = "You lose the ability, Evasion. You have a 30% to dodge projectiles with similar conditions as Evasion. Highly accurate projectiles will reduce your dodge chance."
	required_abilities_types = list(
		/datum/action/ability/xeno_action/evasion
	)
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/defense/runner/upfront_evasion,
		/datum/mutation_upgrade/defense/runner/passing_glance
	)
	/// The percentage chance that projectiles and thrown items will be dodged.
	var/dodge_chance = 30
	/// After this amount of time since their last move, they will no longer dodge projectiles.
	var/movement_leniency = 0.5 SECONDS
	/// If a projectile's accuracy is above this value, then the dodge chance is decreased by each point above it.
	var/accuracy_reduction_threshold = 75

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_mutation_enabled()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(ability)
		ability.remove_action(xenomorph_owner)
	RegisterSignal(xenomorph_owner, COMSIG_XENO_PROJECTILE_HIT, PROC_REF(dodge_projectile))
	RegisterSignal(xenomorph_owner, COMSIG_PRE_MOVABLE_IMPACT, PROC_REF(dodge_thrown_item))

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_mutation_disabled()
	var/datum/action/ability/xeno_action/evasion/ability = new()
	ability.give_action(xenomorph_owner)
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_PROJECTILE_HIT, COMSIG_PRE_MOVABLE_IMPACT))

/datum/mutation_upgrade/defense/runner/ingrained_evasion/on_xenomorph_upgrade()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(ability)
		ability.remove_action(xenomorph_owner) // Since upgrading give abilities that are missing, we have to remove it again.

// Checks if they can dodge at all.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/can_dodge()
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
	var/adjusted_dodge_chance = dodge_chance - (proj.accuracy ? max(0, proj.accuracy - accuracy_reduction_threshold) : 0)
	if(adjusted_dodge_chance && prob(adjusted_dodge_chance))
		dodge_fx(proj)
		return COMPONENT_PROJECTILE_DODGE
	return FALSE

/// Checks if they can dodge a thrown object. If they can, they do so.
/datum/mutation_upgrade/defense/runner/ingrained_evasion/proc/dodge_thrown_item(datum/source, atom/movable/thrown_atom)
	SIGNAL_HANDLER
	if(!isobj(thrown_atom) || !can_dodge())
		return FALSE
	if(prob(dodge_chance))
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

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/offense/runner/sneak_attack
	name = "Sneak Attack"
	desc = "Pounce will slash your target for 1x damage if it was started in dim light."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/pounce/runner
	)

/datum/mutation_upgrade/offense/runner/sneak_attack/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.dim_bonus_multiplier += 1

/datum/mutation_upgrade/offense/runner/sneak_attack/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.dim_bonus_multiplier -= 1

/datum/mutation_upgrade/offense/runner/right_here
	name = "Right Here"
	desc = "Pounce will slash your target for 0.5x slash damage based on the distance traveled. Every tile beyond the first reduces the amount by 20%."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/pounce/runner
	)

/datum/mutation_upgrade/offense/runner/right_here/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.upclose_bonus_multiplier += 0.5

/datum/mutation_upgrade/offense/runner/right_here/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.upclose_bonus_multiplier -= 0.5

/datum/mutation_upgrade/offense/runner/mutilate
	name = "Mutilate"
	desc = "Savage's plasma-to-damage conversion rate is increased by 0.05. Savage will consume all of your plasma."

/datum/mutation_upgrade/offense/runner/right_here/on_gain()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_plasma_conversion_rate += 0.05

/datum/mutation_upgrade/offense/runner/right_here/on_loss()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_plasma_conversion_rate -= 0.05

//*********************//
//       Utility       //
//*********************//
/datum/mutation_upgrade/utility/runner/headslam
	name = "Head Slam"
	desc = "Pounce stuns only for 25% as long. It now confuses and blurs your target's vision for a second."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/pounce/runner
	)

/datum/mutation_upgrade/utility/runner/headslam/on_mutation_enabled()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.stun_duration *= 0.25
	ability.self_immobilize_duration *= 0.25
	ability.savage_debuff_amount += 2 SECONDS

/datum/mutation_upgrade/utility/runner/headslam/on_mutation_disabled()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.stun_duration /= 0.25
	ability.self_immobilize_duration /= 0.25
	ability.savage_debuff_amount -= 2 SECONDS

/datum/mutation_upgrade/utility/runner/frenzy
	name = "Frenzy"
	desc = "Savage's damage is converted to a buff that increases your melee damage for 7 seconds. For each point of damage, your melee damage multiplier is increased by 0.5%."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/pounce/runner
	)

/datum/mutation_upgrade/utility/runner/frenzy/on_mutation_enabled()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_damage_conversion_rate += 0.005

/datum/mutation_upgrade/utility/runner/frenzy/on_mutation_disabled()
	var/datum/action/ability/activable/xeno/pounce/runner/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/pounce/runner]
	if(!ability)
		return
	ability.savage_damage_conversion_rate -= 0.005

/datum/mutation_upgrade/utility/runner/passing_glance
	name = "Passing Glance"
	desc = "While Evasion is on, moving onto the same location as a standing human will confuse them for 2 seconds. This can only happens once per human."
	required_abilities_types = list(
		/datum/action/ability/xeno_action/evasion
	)
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/defense/runner/ingrained_evasion
	)

/datum/mutation_upgrade/utility/runner/passing_glance/on_gain()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.evasion_passthrough = TRUE
	ability.passthrough_confusion_length += 2 SECONDS

/datum/mutation_upgrade/utility/runner/passing_glance/on_loss()
	var/datum/action/ability/xeno_action/evasion/ability = xenomorph_owner.actions_by_path[/datum/action/ability/xeno_action/evasion]
	if(!ability)
		return
	ability.evasion_passthrough = FALSE
	ability.passthrough_confusion_length -= 2 SECONDS
