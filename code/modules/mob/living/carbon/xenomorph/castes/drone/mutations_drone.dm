//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

/datum/mutation_upgrade/offense/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

/datum/mutation_upgrade/utility/drone
	allowed_caste_names = list(/datum/xeno_caste/drone)

//*********************//
//       Defense       //
//*********************//
/datum/mutation_upgrade/defense/drone/scout
	name = "Scout"
	desc = "While you are not on weeds, you gain 5 soft armor."
	/// The attached armor that been given, if any.
	var/datum/armor/attached_armor

/datum/mutation_upgrade/defense/drone/scout/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_LIVING_WEEDS_AT_LOC_CREATED, PROC_REF(entered_weeds))
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_movement))
	if(!xenomorph_owner.loc_weeds_type)
		grant_armor(xenomorph_owner)
		return
	entered_weeds(xenomorph_owner, xenomorph_owner.loc_weeds_type)

/datum/mutation_upgrade/defense/drone/scout/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_LIVING_WEEDS_AT_LOC_CREATED, COMSIG_MOVABLE_MOVED))
	revoke_armor(xenomorph_owner)

/// Grants armor if they do not have it.
/datum/mutation_upgrade/defense/drone/scout/proc/grant_armor()
	if(attached_armor)
		return
	attached_armor = new(5, 5, 5, 5, 5, 5, 5, 5)
	xenomorph_owner.soft_armor = xenomorph_owner.soft_armor.attachArmor(attached_armor)

/// Removes armor if they have it.
/datum/mutation_upgrade/defense/drone/scout/proc/revoke_armor()
	if(!attached_armor)
		return
	xenomorph_owner.soft_armor = xenomorph_owner.soft_armor.detachArmor(attached_armor)
	attached_armor = null

/// Grant armor if they entered somewhere that has no weeds.
/datum/mutation_upgrade/defense/drone/scout/proc/on_movement(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	SIGNAL_HANDLER
	var/obj/alien/weeds/found_weed = locate(/obj/alien/weeds) in xenomorph_owner.loc
	if(found_weed)
		entered_weeds(xenomorph_owner, found_weed)
		return
	grant_armor(xenomorph_owner)

/// Revokes armor if they entered somewhere that has weeds.
/datum/mutation_upgrade/defense/drone/scout/proc/entered_weeds(datum/source, obj/alien/weeds/location_weeds)
	SIGNAL_HANDLER
	revoke_armor(xenomorph_owner)

/datum/mutation_upgrade/defense/drone/together_in_claws
	name = "Together In Claws"
	desc = "While connected with Essence Link, you heal for 10% of your partner's damage when they slash a human."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/essence_link
	)

/datum/mutation_upgrade/defense/drone/together_in_claws/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.lifesteal_percentage = 0.1
	if(!ability.existing_link)
		return
	ability.existing_link.set_lifesteal(ability.lifesteal_percentage)

/datum/mutation_upgrade/defense/drone/together_in_claws/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.lifesteal_percentage = initial(ability.lifesteal_percentage)
	if(!ability.existing_link)
		return
	ability.existing_link.set_lifesteal(ability.lifesteal_percentage)

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/offense/drone/revenge
	name = "Revenge"
	desc = "While connected with Essence Link and it ends due to death, the survivor gains 75% additional melee damage for 10 seconds."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/essence_link
	)

/datum/mutation_upgrade/offense/drone/revenge/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.revenge_modifier = 0.75
	if(!ability.existing_link)
		return
	ability.existing_link.revenge_modifier = ability.revenge_modifier

/datum/mutation_upgrade/offense/drone/revenge/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.revenge_modifier = initial(ability.revenge_modifier)
	if(!ability.existing_link)
		return
	ability.existing_link.revenge_modifier = ability.revenge_modifier

//*********************//
//       Utility       //
//*********************//
/datum/mutation_upgrade/utility/drone/saving_grace
	name = "Saving Grace"
	desc = "Salve Heal has no cast time on your Essence Link partner if they qualify for bonus healing. Bonus healing multiplier is increased by an additive of 1."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/psychic_cure/acidic_salve
		/datum/action/ability/activable/xeno/essence_link
	)

/datum/mutation_upgrade/utility/drone/saving_grace/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.bypass_cast_time_on_threshold = TRUE
	ability.bonus_healing_additive_multiplier += 1

/datum/mutation_upgrade/utility/drone/saving_grace/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/acidic_salve/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure/acidic_salve]
	if(!ability)
		return
	ability.bypass_cast_time_on_threshold = initial(ability.bypass_cast_time_on_threshold)
	ability.bonus_healing_additive_multiplier -= 1

/datum/mutation_upgrade/utility/drone/vitality_transfer
	name = "Vitality Transfer"
	desc = "While connected with Essence Link, you can manually disconnect to heal your partner for 5% of their maximum health multiplied by the attunement amount. However, you take true damage equal to the amount healed. This damage can kill you."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/essence_link
	)

/datum/mutation_upgrade/utility/drone/vitality_transfer/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.disconnection_heal_percentage = 0.05

/datum/mutation_upgrade/utility/drone/vitality_transfer/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.disconnection_heal_percentage = initial(ability.disconnection_heal_percentage)
