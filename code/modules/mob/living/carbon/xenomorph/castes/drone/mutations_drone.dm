//*********************//
//        Base        //
//*********************//

/datum/mutation_upgrade/shell/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

/datum/mutation_upgrade/spur/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

/datum/mutation_upgrade/veil/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

//*********************//
//        Shell        //
//*********************//

/datum/mutation_upgrade/shell/drone/scout
	name = "Scout"
	desc = "You now gain the speed bonus associated with being on weeds at all times."

/datum/mutation_upgrade/shell/drone/scout/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_movement))
	return ..()

/datum/mutation_upgrade/shell/drone/scout/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED)
	return ..()

/// Upon moving onto a loc without weeds, adds the weed speed modifier to their next movement.
/datum/mutation_upgrade/shell/drone/scout/proc/on_movement(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	SIGNAL_HANDLER
	var/obj/alien/weeds/found_weed = locate(/obj/alien/weeds) in xenomorph_owner.loc
	if(found_weed)
		return
	xenomorph_owner.next_move_slowdown += xenomorph_owner.xeno_caste?.weeds_speed_mod

/datum/mutation_upgrade/shell/drone/together_in_claws
	name = "Together In Claws"
	desc = "While actively connected with your Essence Link partner, your partner's slash attacks heal you for 20% of the damage dealt."

/datum/mutation_upgrade/spur/drone/together_in_claws/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED, PROC_REF(on_essence_link_toggle))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, TRUE)
	return ..()

/datum/mutation_upgrade/spur/drone/together_in_claws/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_ESSENCE_LINK_TOGGLED))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, FALSE)
	return ..()

/datum/mutation_upgrade/spur/drone/together_in_claws/proc/on_essence_link_toggle(datum/source, mob/living/carbon/xenomorph/link_partner, toggled)
	SIGNAL_HANDLER
	if(toggled)
		RegisterSignal(link_partner, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack_living))
		return
	UnregisterSignal(link_partner, COMSIG_XENOMORPH_POSTATTACK_LIVING)

/// Heals the owner for a portion of damage dealt by the Essence Link partner's slash attacks.
/datum/mutation_upgrade/spur/drone/together_in_claws/proc/on_postattack_living(datum/source, mob/living/attacked_target, damage_dealt, list/damage_modifiers)
	SIGNAL_HANDLER
	var/damage_to_heal = damage_dealt * 0.2 // 20%
	HEAL_XENO_DAMAGE(xenomorph_owner, damage_to_heal, FALSE)

/datum/mutation_upgrade/spur/drone/emergency_repel
	name = "Emergency Repel"
	desc = "While actively connected with your Essence Link partner, cancelling the link will heal 20% of your missing health per attunement bar and throws you towards your partner bypassing wired barricades."

/datum/mutation_upgrade/spur/drone/emergency_repel/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED, PROC_REF(on_essence_link_toggle))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, TRUE)
	return ..()

/datum/mutation_upgrade/spur/drone/emergency_repel/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED)
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, FALSE)
	return ..()

/// Handles signal registration for when Essence Link begins to end.
/datum/mutation_upgrade/spur/drone/emergency_repel/proc/on_essence_link_toggle(datum/source, mob/living/carbon/xenomorph/link_partner, toggled)
	if(toggled)
		RegisterSignal(link_partner, COMSIG_XENO_ESSENCE_LINK_ENDING, PROC_REF(on_essence_link_ending))
		return
	UnregisterSignal(link_partner, COMSIG_XENO_ESSENCE_LINK_ENDING)

/// Upon manual disconnection, heal percentage of missing health, gives HOVERING pass flag, and throws them toward their partner.
/datum/mutation_upgrade/spur/drone/emergency_repel/proc/on_essence_link_ending(datum/source, datum/status_effect/stacking/essence_link/existing_link, was_manually_disconnected)
	if(!was_manually_disconnected)
		return
	var/missing_health_to_heal = (xenomorph_owner.status_flags & GODMODE) ? 0 : ((xenomorph_owner.getFireLoss() + xenomorph_owner.getBruteLoss()) * 0.2 * existing_link.stacks)
	if(missing_health_to_heal)
		HEAL_XENO_DAMAGE(xenomorph_owner, missing_health_to_heal, TRUE)
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_POST_THROW, PROC_REF(on_post_throw))
	xenomorph_owner.add_pass_flags(HOVERING, type)
	xenomorph_owner.throw_at(existing_link.link_target, 7, 3, xenomorph_owner)

/// Removes the HOVERING pass flag that was given during the throw.
/datum/mutation_upgrade/spur/drone/emergency_repel/proc/on_post_throw(datum/source)
	SIGNAL_HANDLER
	xenomorph_owner.remove_pass_flags(HOVERING, type)
	UnregisterSignal(source, COMSIG_MOVABLE_POST_THROW)

//*********************//
//         Spur        //
//*********************//

/datum/mutation_upgrade/spur/drone/combustive_jelly
	name = "Combustive Jelly"
	desc = "Resin jelly you throw explodes into thin sticky resin in a 3x3 where it lands that lasts for 8 seconds."
	/// The amount of deciseconds that the thin sticky resin will last for.
	var/sticky_resin_duration = 8 SECONDS

/datum/mutation_upgrade/spur/drone/combustive_jelly/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_MOB_THROW, PROC_REF(on_throw_anything))
	return ..()

/datum/mutation_upgrade/spur/drone/combustive_jelly/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, COMSIG_MOB_THROW)
	return ..()

/// Registers various throw signals for the thrown item if it is resin jelly.
/datum/mutation_upgrade/spur/drone/combustive_jelly/proc/on_throw_anything(atom/movable/thrower, target, thrown_thing, list/throw_modifiers)
	SIGNAL_HANDLER
	if(!isresinjelly(thrown_thing))
		return
	RegisterSignal(thrown_thing, COMSIG_MOVABLE_IMPACT, PROC_REF(on_jelly_throw_impact))
	RegisterSignal(thrown_thing, COMSIG_MOVABLE_POST_THROW, PROC_REF(on_jelly_throw_ended))

/// Unregisters various throw signals and creates thin sticky resin when it impacts a living being.
/datum/mutation_upgrade/spur/drone/combustive_jelly/proc/on_jelly_throw_impact(datum/source, atom/hit_atom, speed)
	SIGNAL_HANDLER
	if(!isliving(hit_atom))
		return
	UnregisterSignal(source, list(COMSIG_MOVABLE_IMPACT, COMSIG_MOVABLE_POST_THROW))
	var/mob/living/hit_living = hit_atom
	for(var/turf/sticky_tile AS in RANGE_TURFS(1, get_turf(hit_living)))
		if(!locate(/obj/alien/resin/sticky/thin) in sticky_tile.contents)
			var/obj/alien/resin/sticky/thin/temporary_resin = new(sticky_tile)
			QDEL_IN(temporary_resin, sticky_resin_duration)
	playsound(hit_atom.loc, SFX_ALIEN_RESIN_BUILD, 50, 1)
	qdel(source)

/// Unregisters various throw signals and creates thin sticky resin when it ends the throw.
/datum/mutation_upgrade/spur/drone/combustive_jelly/proc/on_jelly_throw_ended(datum/source)
	SIGNAL_HANDLER
	var/obj/item/resin_jelly/jelly_item = source
	UnregisterSignal(source, list(COMSIG_MOVABLE_IMPACT, COMSIG_MOVABLE_POST_THROW))
	for(var/turf/sticky_tile AS in RANGE_TURFS(1, jelly_item.loc))
		if(!locate(/obj/alien/resin/sticky/thin) in sticky_tile.contents)
			var/obj/alien/resin/sticky/thin/temporary_resin = new(sticky_tile)
			QDEL_IN(temporary_resin, sticky_resin_duration)
	playsound(jelly_item.loc, SFX_ALIEN_RESIN_BUILD, 50, 1)
	qdel(source)

/datum/mutation_upgrade/spur/drone/coordinated_slashes
	name = "Coordinated Slashes"
	desc = "While actively connected with your Essence Link partner, the last human you slashed is marked for a second. Your partner can consume the mark by slashing them to deal 10% more damage."
	/// The latest human that was slashed.
	var/mob/living/carbon/human/latest_marked_human
	/// The timer that resets the marked human (after a second).
	var/timer_id

/datum/mutation_upgrade/spur/drone/coordinated_slashes/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED, PROC_REF(on_essence_link_toggle))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, TRUE)
	return ..()

/datum/mutation_upgrade/spur/drone/coordinated_slashes/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED)
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, FALSE)
	remove_mark()
	return ..()

/// Resets everything related to the marked human.
/datum/mutation_upgrade/spur/drone/coordinated_slashes/proc/remove_mark()
	if(!latest_marked_human || !timer_id)
		return
	latest_marked_human.remove_filter("coordinated_slashes_outline")
	latest_marked_human = null
	deltimer(timer_id)
	timer_id = null

/// Sets everything related to the marked human.
/datum/mutation_upgrade/spur/drone/coordinated_slashes/proc/set_marked(mob/living/carbon/human/next_marked_human)
	remove_mark()
	latest_marked_human = next_marked_human
	latest_marked_human.add_filter("coordinated_slashes_outline", 2, outline_filter(1, COLOR_LIGHT_ORANGE))
	timer_id = addtimer(CALLBACK(src, PROC_REF(remove_mark)), 1 SECONDS, TIMER_UNIQUE|TIMER_STOPPABLE)

/// Handles signal registration for attacking living beings.
/datum/mutation_upgrade/spur/drone/coordinated_slashes/proc/on_essence_link_toggle(datum/source, mob/living/carbon/xenomorph/link_partner, toggled)
	if(toggled)
		RegisterSignal(xenomorph_owner, COMSIG_XENOMORPH_ATTACK_LIVING, PROC_REF(on_attack_living_owner))
		RegisterSignal(link_partner, COMSIG_XENOMORPH_ATTACK_LIVING, PROC_REF(on_attack_living_partner))
		return
	UnregisterSignal(xenomorph_owner, COMSIG_XENOMORPH_ATTACK_LIVING, PROC_REF(on_attack_living_owner))
	UnregisterSignal(link_partner, COMSIG_XENOMORPH_ATTACK_LIVING)

/// For the owner, sets the marked human or resets the duration if they are already marked.
/datum/mutation_upgrade/spur/drone/coordinated_slashes/proc/on_attack_living_owner(datum/source, mob/living/target, damage, list/damage_mod, list/armor_mod)
	if(!ishuman(target))
		return
	if(latest_marked_human == target && timer_id)
		deltimer(timer_id)
		timer_id = addtimer(CALLBACK(src, PROC_REF(remove_mark)), 1 SECONDS, TIMER_UNIQUE|TIMER_STOPPABLE)
		return
	set_marked(target)

/// For the partner, deal bonus damage if it is the marked human and unmark them.
/datum/mutation_upgrade/spur/drone/coordinated_slashes/proc/on_attack_living_partner(datum/source, mob/living/target, damage, list/damage_mod, list/armor_mod)
	if(!latest_marked_human || !ishuman(target) || latest_marked_human != target)
		return
	damage_mod += damage * 0.2
	remove_mark()

/datum/mutation_upgrade/spur/drone/aggressive_connection
	name = "Aggressive Connection"
	desc = "While actively connected with your Essence Link partner, your melee damage is increased up to 30% inversely proportional to your partner's health. Critical health grants the maximum amount."
	var/current_damage_multiplier = 0

/datum/mutation_upgrade/spur/drone/aggressive_connection/on_mutation_enabled()
	RegisterSignal(xenomorph_owner, COMSIG_XENO_ESSENCE_LINK_TOGGLED, PROC_REF(on_essence_link_toggle))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, TRUE)
	return ..()

/datum/mutation_upgrade/spur/drone/aggressive_connection/on_mutation_disabled()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENO_ESSENCE_LINK_TOGGLED))
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(ability?.existing_link && ability.existing_link.current_beam)
		on_essence_link_toggle(xenomorph_owner, ability.existing_link.link_target, FALSE)
	return ..()

/// Sets the owner's melee damage modifier relative to what it was previously set as.
/datum/mutation_upgrade/spur/drone/aggressive_connection/proc/set_damage_multiplier(new_damage_multiplier)
	if(current_damage_multiplier == new_damage_multiplier)
		return
	if(current_damage_multiplier)
		xenomorph_owner.remove_filter("aggressive_connection_outline")
	if(new_damage_multiplier)
		xenomorph_owner.add_filter("aggressive_connection_outline", 1, outline_filter(1 * new_damage_multiplier, COLOR_RED_LIGHT));
	xenomorph_owner.xeno_melee_damage_modifier += (new_damage_multiplier - current_damage_multiplier)
	current_damage_multiplier = new_damage_multiplier

/// Handles signal registration for checking the partner's health.
/datum/mutation_upgrade/spur/drone/aggressive_connection/proc/on_essence_link_toggle(datum/source, mob/living/carbon/xenomorph/link_partner, toggled)
	SIGNAL_HANDLER
	if(toggled)
		RegisterSignal(link_partner, COMSIG_LIVING_UPDATE_HEALTH, PROC_REF(on_living_update_health))
		on_living_update_health(link_partner)
		return
	UnregisterSignal(link_partner, COMSIG_LIVING_UPDATE_HEALTH)
	set_damage_multiplier(0)

/// Sets damage multiplier based on partner's missing health.
/datum/mutation_upgrade/spur/drone/aggressive_connection/proc/on_living_update_health(datum/source)
	SIGNAL_HANDLER
	var/mob/living/carbon/xenomorph/updating_xenomorph = source
	var/health = (updating_xenomorph.status_flags & GODMODE) ? updating_xenomorph.maxHealth : (updating_xenomorph.maxHealth - updating_xenomorph.getFireLoss() - updating_xenomorph.getBruteLoss())
	if(health >= updating_xenomorph.maxHealth)
		set_damage_multiplier(0)
		return
	if(health <= updating_xenomorph.get_crit_threshold())
		set_damage_multiplier(0.3)
		return
	set_damage_multiplier((1 - PERCENT(health / updating_xenomorph.maxHealth)) * 0.3)

//*********************//
//         Veil        //
//*********************//

/datum/mutation_upgrade/veil/drone/sacrificial_salve
	name = "Sacrificial Salve"
	desc = "Healing Salve can be toggled for additional effects. While toggled, healing will additionally transfer up to half of your current health to your target."

/datum/mutation_upgrade/veil/drone/sacrificial_salve/on_mutation_enabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.expeditious_salve_togglable = TRUE
	return ..()

/datum/mutation_upgrade/veil/drone/sacrificial_salve/on_mutation_disabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	if(ability.expeditious_salve_toggled)
		ability.alternate_action_activate()
	ability.expeditious_salve_togglable = initial(ability.expeditious_salve_togglable)
	return ..()

/datum/mutation_upgrade/veil/drone/saving_grace
	name = "Saving Grace"
	desc = "Acidic Salve has no cast time on your Essence Link partner. The maximum health threshold to qualify for bonus heal potency on your Essence Link partner is raised from 10% to 30%."

/datum/mutation_upgrade/veil/drone/saving_grace/on_mutation_enabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.bypass_cast_time_for_partner = TRUE
	ability.bonus_heal_maximum_health_threshold += 0.2
	return ..()

/datum/mutation_upgrade/veil/drone/saving_grace/on_mutation_disabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.bypass_cast_time_for_partner = initial(ability.bypass_cast_time_for_partner)
	ability.bonus_heal_maximum_health_threshold -= 0.2
	return ..()

/datum/mutation_upgrade/veil/drone/self_sufficiency
	name = "Self Sufficiency"
	desc = "Acidic Salve can be self-casted at 75% efficiency."

/datum/mutation_upgrade/veil/drone/self_sufficiency/on_mutation_enabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.use_state_flags |= ABILITY_TARGET_SELF // Ability handles the efficiency.
	return ..()

/datum/mutation_upgrade/veil/drone/self_sufficiency/on_mutation_disabled()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.use_state_flags &= ~(ABILITY_TARGET_SELF)
	return ..()
