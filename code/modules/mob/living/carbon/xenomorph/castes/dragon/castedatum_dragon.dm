/datum/xeno_caste/dragon
	caste_name = "Dragon"
	display_name = "Dragon"
	upgrade_name = ""
	caste_desc = "Placeholder caste description!"
	caste_type_path = /mob/living/carbon/xenomorph/dragon
	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_BASETYPE
	wound_type = "behemoth"

	// *** Melee Attacks *** //
	melee_damage = 80

	// *** Speed *** //
	speed = -0.6
	weeds_speed_mod = -5

	// *** Plasma *** //
	plasma_max = 300
	plasma_gain = 30

	// *** Health *** //
	max_health = 750

	// *** Evolution *** //
	upgrade_threshold = TIER_THREE_THRESHOLD
	evolve_min_xenos = 15
	maximum_active_caste = 1
	death_evolution_delay = 5 MINUTES

	// *** Flags *** //
	caste_flags = CASTE_EVOLUTION_ALLOWED|CASTE_IS_STRONG|CASTE_STAGGER_RESISTANT
	can_flags = 0
	caste_traits = null

	// *** Defense *** //
	soft_armor = list(MELEE = 75, BULLET = 75, LASER = 75, ENERGY = 75, BOMB = 50, BIO = 75, FIRE = 200, ACID = 75)
	hard_armor = list(MELEE = 10, BULLET = 10, LASER = 10, ENERGY = 10, BOMB = 19, BIO = 19, FIRE = 10, ACID = 10)

	// *** Minimap Icon *** //
	minimap_icon = "behemoth"

	// *** Abilities *** ///
	actions = list(
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain
	)

/*
/datum/xeno_caste/dragon/on_caste_applied(mob/xenomorph)
	return

/datum/xeno_caste/dragon/on_caste_removed(mob/xenomorph)
	return
*/


/datum/xeno_caste/dragon/normal
	upgrade = XENO_UPGRADE_NORMAL

/datum/xeno_caste/dragon/primordial
	upgrade = XENO_UPGRADE_PRIMO

	// *** Abilities *** ///
	actions = list(
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain
	)

