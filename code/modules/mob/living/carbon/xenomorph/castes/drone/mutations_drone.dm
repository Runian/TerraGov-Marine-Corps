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
	desc = "While on non-weeds, you gain the weed speed bonus as if you were on weeds."

/datum/mutation_upgrade/defense/drone/scout/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_movement))

/datum/mutation_upgrade/defense/drone/scout/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED)

/// Changes the next move slowdown if there isn't any weeds where they moved onto.
/datum/mutation_upgrade/defense/drone/scout/proc/on_movement(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	SIGNAL_HANDLER
	var/obj/alien/weeds/found_weed = locate(/obj/alien/weeds) in xenomorph_owner.loc
	if(found_weed)
		return
	xenomorph_owner.next_move_slowdown += xenomorph_owner.xeno_caste.weeds_speed_mod

/datum/mutation_upgrade/defense/drone/together_in_claws
	name = "Together In Claws"
	desc = "While actively linked with your Essence Link partner, their slash attacks heal you for 50% of damage dealt."
	required_abilities_types = list(
		/datum/action/ability/activable/xeno/essence_link
	)

/datum/mutation_upgrade/defense/drone/together_in_claws/on_gain()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.lifesteal_percentage += 0.5
	ability.existing_link?.set_lifesteal(ability.lifesteal_percentage)

/datum/mutation_upgrade/defense/drone/together_in_claws/on_loss()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.lifesteal_percentage -= 0.5
	ability.existing_link?.set_lifesteal(ability.lifesteal_percentage)

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
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.revenge_modifier += 0.75
	if(!ability.existing_link)
		return
	ability.existing_link.revenge_modifier = ability.revenge_modifier

/datum/mutation_upgrade/offense/drone/revenge/on_loss()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.revenge_modifier -= 0.75
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
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.disconnection_heal_percentage += 0.05

/datum/mutation_upgrade/utility/drone/vitality_transfer/on_loss()
	var/datum/action/ability/activable/xeno/essence_link/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/essence_link]
	if(!ability)
		return
	ability.disconnection_heal_percentage -= 0.05
