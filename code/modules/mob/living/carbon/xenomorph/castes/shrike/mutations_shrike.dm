//*********************//
//        Base        //
//*********************//
/datum/mutation_upgrade/defense/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

/datum/mutation_upgrade/offense/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

/datum/mutation_upgrade/utility/shrike
	allowed_caste_names = list(/datum/xeno_caste/shrike)

//*********************//
//       Defense       //
//*********************//
/datum/mutation_upgrade/defense/shrike/lone_healer
	name = "Lone Healer"
	desc = "Psychic Cure can now target yourself. Healing yourself is only 50% as effective."
	/// For the first structure, the multiplier of Psychic Cure's initial healing power to add to the ability.
	var/self_heal_multiplier_initial = -0.5
	/// For each structure, the multiplier of Psychic Cure's initial healing power to add to the ability.
	var/self_heal_multiplier_per_structure = 0.1

/datum/mutation_upgrade/defense/shrike/lone_healer/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.use_state_flags |= ABILITY_TARGET_SELF
	cure_ability.self_heal_multiplier += 0.5

/datum/mutation_upgrade/defense/shrike/lone_healer/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.use_state_flags &= ~(ABILITY_TARGET_SELF)
	cure_ability.self_heal_multiplier -= 0.5

/datum/mutation_upgrade/defense/shrike/shared_cure
	name = "Shared Cure"
	desc = "20% of the health restored from Psychic Cure is reapplied to you."

/datum/mutation_upgrade/defense/shrike/shared_cure/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.rebound_percentage += 0.2

/datum/mutation_upgrade/defense/shrike/shared_cure/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.rebound_percentage -= 0.2

/datum/mutation_upgrade/defense/shrike/resistant_cure
	name = "Resistant Cure"
	desc = "Psychic Cure now also applies the effects of resin jelly to you and your target for 40 seconds."

/datum/mutation_upgrade/defense/shrike/resistant_cure/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Psychic Cure now also applies the effects of resin jelly to you and your target for [get_duration(new_amount) / 10] seconds."

/datum/mutation_upgrade/defense/shrike/resistant_cure/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.resin_jelly_duration += 40 SECONDS

/datum/mutation_upgrade/defense/shrike/resistant_cure/on_loss()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.resin_jelly_duration -= 40 SECONDS

//*********************//
//       Offense       //
//*********************//
/datum/mutation_upgrade/offense/shrike/smashing_fling
	name = "Smashing Fling"
	desc = "Psychic Fling deals 150% damage equal to your melee damage, enables collusions, but no longer immediately stuns. If the target collides with a human, object, or wall: both are briefly paralyzed and dealt damage again."

/datum/mutation_upgrade/offense/shrike/smashing_fling/on_gain()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	fling_ability.stun_duration = 0 SECONDS
	fling_ability.damage_multiplier += 1.5
	fling_ability.collusion_paralyze_duration = 0.1 SECONDS
	fling_ability.collusion_damage_multiplier += 1.5

/datum/mutation_upgrade/offense/shrike/smashing_fling/on_loss()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	fling_ability.stun_duration = initial(fling_ability.stun_duration)
	fling_ability.damage_multiplier -= 1.5
	fling_ability.collusion_paralyze_duration = initial(fling_ability.collusion_paralyze_duration)
	fling_ability.collusion_damage_multiplier -= 1.5

/datum/mutation_upgrade/offense/shrike/gravity_tide
	name = "Gravity Tide"
	desc = "Unrelenting Force pulls things towards you then pushes them away. The distance they are thrown is increased by 2."

/datum/mutation_upgrade/offense/shrike/gravity_tide/on_gain()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.rebound_throwing = TRUE
	force_ability.throwing_distance += 2

/datum/mutation_upgrade/offense/shrike/gravity_tide/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.rebound_throwing = initial(force_ability.rebound_throwing)
	force_ability.throwing_distance -= 2

/datum/mutation_upgrade/offense/shrike/body_fling
	name = "Body Fling"
	desc = "Psychic Fling can be used on yourself and allied xenomorphs. Humans who are hit by a flung xenomorph are paralyzed for 2 seconds and dealt 150% of your slash damage."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/utility/shrike/psychic_choke
	)

/datum/mutation_upgrade/offense/shrike/body_fling/on_gain()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	fling_ability.use_state_flags |= ABILITY_TARGET_SELF
	fling_ability.collusion_damage_multiplier += 1.5 SECONDS
	fling_ability.collusion_paralyze_duration += 2 SECONDS
	fling_ability.collusion_xenos_only = TRUE

/datum/mutation_upgrade/offense/shrike/body_fling/on_loss()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(!fling_ability)
		return
	fling_ability.use_state_flags &= ~ABILITY_TARGET_SELF
	fling_ability.collusion_damage_multiplier -= 1.5 SECONDS
	fling_ability.collusion_paralyze_duration -= 2 SECONDS
	fling_ability.collusion_xenos_only = initial(fling_ability.collusion_xenos_only)

//*********************//
//       Utility       //
//*********************//
/datum/mutation_upgrade/utility/shrike/delayed_condition
	name = "Delayed Condition"
	desc = "Psychic Heal grants slowdown immunity and delays all inbound stun, knockdown, and stagger effects caused to your target by 8 seconds. At the end of this duration, delayed status effects are reapplied."

/datum/mutation_upgrade/utility/shrike/delayed_condition/on_gain()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.delayed_status_duration += 8 SECONDS

/datum/mutation_upgrade/utility/shrike/delayed_condition/on_loss()
	. = ..()
	var/datum/action/ability/activable/xeno/psychic_cure/cure_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_cure]
	if(!cure_ability)
		return
	cure_ability.delayed_status_duration -= 8 SECONDS


/datum/mutation_upgrade/utility/shrike/deflective_force
	name = "Deflective Force"
	desc = "Unrelenting Force now reflects all projectiles in its affected area. Reflecting more than 50 projectile damage resets Psychic Scream's cooldown to 30% of its original value."


/datum/mutation_upgrade/utility/shrike/deflective_force/on_gain()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.projectile_cooldown_mulitplier += 0.3

/datum/mutation_upgrade/utility/shrike/deflective_force/on_loss()
	var/datum/action/ability/activable/xeno/unrelenting_force/force_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/unrelenting_force]
	if(!force_ability)
		return
	force_ability.projectile_cooldown_mulitplier -= 0.3

/datum/mutation_upgrade/utility/shrike/psychic_choke
	name = "Psychic Choke"
	desc = "You lose the ability Psychic Fling in exchange for the ability Psychic Choke. Psychic Choke lets you paralyze a marine as long you channel it. The damage threshold to disrupt Psychic Choke is 20/35/50."
	conflicting_mutation_types = list(
		/datum/mutation_upgrade/offense/shrike/body_fling
	)

/datum/mutation_upgrade/utility/shrike/psychic_choke/on_gain()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(fling_ability)
		fling_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/activable/xeno/psychic_choke/choke_ability = new()
	choke_ability.give_action(xenomorph_owner)

/datum/mutation_upgrade/utility/shrike/psychic_choke/on_loss()
	var/datum/action/ability/activable/xeno/psychic_choke/choke_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_choke]
	if(choke_ability)
		choke_ability.remove_action(xenomorph_owner)
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = new()
	fling_ability.give_action(xenomorph_owner)

/datum/mutation_upgrade/utility/shrike/psychic_choke/on_xenomorph_upgrade()
	var/datum/action/ability/activable/xeno/psychic_fling/fling_ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/psychic_fling]
	if(fling_ability)
		fling_ability.remove_action(xenomorph_owner) // Since upgrading give abilities that are missing, we have to remove it again.
