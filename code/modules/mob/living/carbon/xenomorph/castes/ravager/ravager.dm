/mob/living/carbon/xenomorph/ravager
	caste_base_type = /datum/xeno_caste/ravager
	name = "Ravager"
	desc = "A huge, nasty red alien with enormous scythed claws."
	icon = 'icons/Xeno/castes/ravager.dmi'
	icon_state = "Ravager Walking"
	health = 250
	maxHealth = 250
	plasma_stored = 50
	mob_size = MOB_SIZE_BIG
	drag_delay = 6 //pulling a big dead xeno is hard
	tier = XENO_TIER_THREE
	upgrade = XENO_UPGRADE_NORMAL
	pixel_x = -16
	bubble_icon = "alienroyal"
	/// Whether or not that they get special effects / plasma from being on fire.
	var/fiery_plasma = TRUE

/mob/living/carbon/xenomorph/ravager/Initialize(mapload)
	. = ..()
	ADD_TRAIT(src, TRAIT_LIGHT_STEP, XENO_TRAIT)

// ***************************************
// *********** Mob overrides
// ***************************************
/mob/living/carbon/xenomorph/ravager/fire_act(burn_level)
	. = ..()
	if(stat)
		return
	if(!fiery_plasma)
		return FALSE
	if(TIMER_COOLDOWN_CHECK(src, COOLDOWN_RAVAGER_FLAMER_ACT))
		return FALSE
	gain_plasma(50)
	TIMER_COOLDOWN_START(src, COOLDOWN_RAVAGER_FLAMER_ACT, 1 SECONDS)
	if(prob(30))
		emote("roar")
		to_chat(src, span_xenodanger("The heat of the fire roars in our veins! KILL! CHARGE! DESTROY!"))

// ***************************************
// *********** Ability related
// ***************************************
/mob/living/carbon/xenomorph/ravager/get_crit_threshold()
	. = ..()
	if(!endure)
		return
	var/datum/action/ability/xeno_action/endure/endure_ability = actions_by_path[/datum/action/ability/xeno_action/endure]
	return endure_ability.endure_threshold

/mob/living/carbon/xenomorph/ravager/get_death_threshold()
	. = ..()
	if(!endure)
		return
	var/datum/action/ability/xeno_action/endure/endure_ability = actions_by_path[/datum/action/ability/xeno_action/endure]
	return endure_ability.endure_threshold

// ***************************************
// *********** Berserker
// ***************************************
/mob/living/carbon/xenomorph/ravager/berserker
	caste_base_type = /datum/xeno_caste/ravager/berserker
	plasma_stored = 0
	fiery_plasma = FALSE

/mob/living/carbon/xenomorph/ravager/berserker/Initialize(mapload)
	. = ..()
	RegisterSignal(src, COMSIG_XENOMORPH_TAKING_DAMAGE, PROC_REF(on_attacked))
	RegisterSignal(src, COMSIG_XENOMORPH_ATTACK_LIVING, PROC_REF(on_attack)) // Even if it did zero damage or was blocked (aka didn't reach postattack), still want to give plasma.
	RegisterSignal(src, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack))
	// Need to make it so that six seconds "out-of-combat" starts draining 50 plasma per tick.

/// Resets the out-of-combat timer.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_attacked(damage)
	SIGNAL_HANDLER
	// Update out-of-combat timer.

/// Handles rage regeneration and resets the out-of-combat timer.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_attack(mob/living/source, mob/living/target, damage, list/damage_mod, list/armor_mod)
	SIGNAL_HANDLER
	// Increase rage by 100 per hit.
	// Update out-of-combat timer.

/// Handle life stealing.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_postattack(mob/living/source, mob/living/target, damage)
	SIGNAL_HANDLER
	// Increase health by 50% of damage plus 5% per 100 rage.

/// Updates armor, movement speed, and attack speed changes based on current rage.
/mob/living/carbon/xenomorph/ravager/berserker/proc/update_rage_stats()
	// Remove old buffs as needed.
	// Add new buff based on current rage.
