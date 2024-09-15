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
	var/rage_buffed = 0

/mob/living/carbon/xenomorph/ravager/berserker/Initialize(mapload)
	. = ..()
	RegisterSignal(src, COMSIG_XENOMORPH_TAKING_DAMAGE, PROC_REF(on_attacked))
	RegisterSignal(src, COMSIG_XENOMORPH_ATTACK_LIVING, PROC_REF(on_attack)) // Even if it did zero damage or was blocked (aka didn't reach postattack), still want to give plasma.
	RegisterSignal(src, COMSIG_XENOMORPH_POSTATTACK_LIVING, PROC_REF(on_postattack))

/// Resets the out-of-combat timer.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_attacked(damage)
	SIGNAL_HANDLER
	TIMER_COOLDOWN_START(src, COOLDOWN_OUT_OF_COMBAT, 6 SECONDS)

/// Handles rage regeneration and resets the out-of-combat timer.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_attack(mob/living/source, mob/living/target, damage, list/damage_mod, list/armor_mod)
	SIGNAL_HANDLER
	gain_plasma(100)
	update_rage_stats()
	TIMER_COOLDOWN_START(src, COOLDOWN_OUT_OF_COMBAT, 6 SECONDS)

/// Handle life stealing.
/mob/living/carbon/xenomorph/ravager/berserker/proc/on_postattack(mob/living/source, mob/living/target, damage)
	SIGNAL_HANDLER
	var/rage_level = round(min(plasma_stored, 0)/100)
	var/lifesteal_percentage = 0.5 + (0.05 * rage_level)
	var/damage_to_heal = damage * lifesteal_percentage
	HEAL_XENO_DAMAGE(src, damage_to_heal, FALSE)

/// Updates armor, movement speed, and attack speed changes based on current rage.
/mob/living/carbon/xenomorph/ravager/berserker/proc/update_rage_stats()
	var/rage_level = round(min(plasma_stored, 0)/100)

	// 2.5 armor per 100 plasma.
	var/datum/armor/base_armor = getArmor(arglist(xeno_caste.soft_armor))
	var/datum/armor/old_armor_diff = base_armor.scaleAllRatings(rage_level)
	var/datum/armor/new_armor_diff = base_armor.scaleAllRatings(rage_level * 2.5)

	if(rage_buffed)
		soft_armor = soft_armor.detachArmor(old_armor_diff)
	soft_armor = soft_armor.attachArmor(new_armor_diff)
	rage_buffed = rage_level

	// -0.5 attack delay per 100 plasma.
	xeno_caste.attack_delay = initial(xeno_caste.attack_delay) - (rage_level * 0.5)

	// -0.15 speed per 100 plasma.
	if(!rage_level && has_movespeed_modifier(MOVESPEED_ID_RAVAGER_BERSERKER_RAGE))
		remove_movespeed_modifier(MOVESPEED_ID_RAVAGER_BERSERKER_RAGE)
		return
	add_movespeed_modifier(MOVESPEED_ID_RAVAGER_BERSERKER_RAGE, TRUE, 0, NONE, TRUE, -0.15 * rage_level)

/// Drains 50 plasma every tick if considered out-of-combat.
/mob/living/carbon/xenomorph/ravager/berserker/Life()
	. = ..()
	if(!.)
		return
	if(TIMER_COOLDOWN_CHECK(src, COOLDOWN_OUT_OF_COMBAT))
		return
	use_plasma(50)
	update_rage_stats()
