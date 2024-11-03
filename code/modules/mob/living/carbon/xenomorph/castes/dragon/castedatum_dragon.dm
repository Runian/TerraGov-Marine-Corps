/* The TODO list in order: https://hackmd.io/@wlLEbadtQoCyE4iytTkonw/S10wubC1A
base xeno_caste, xeno, etc ✔
Health ✔
Soft Armor ✔
Hard Armor ✔
Speed ✔
can speak english ✔
stamina sprites :) ✔
15 xeno limit ✔
ms-paint level of Sprites ✔
cool descriptions! ✔
Immunity to CC: slow / stun / fling / stagger
give caste_flag to prevent plasma/health regen on weeds ✔
no pheromone given ✔
no king summon allowed ✔
no slash like Hivemind
no benefit from friendly xeno skills (like deathwing in HoTS!) :)
EVERYTHING ELSE WAHHHH
REAL Sprites
*/

/datum/xeno_caste/dragon
	caste_name = "Dragon"
	display_name = "Dragon"
	upgrade_name = ""
	caste_type_path = /mob/living/carbon/xenomorph/dragon
	caste_desc = "A big scary monster with wings!"

	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_BASETYPE

	// *** Melee Attacks *** //
	melee_damage = 33

	// *** Speed *** //
	speed = -0.6

	// *** Plasma *** //
	plasma_max = 10
	plasma_gain = 0
	plasma_icon_state = "stamina"

	// *** Health *** //
	max_health = 1650 // Calm (750) + Enraged (900)

	// *** Evolution *** //
	upgrade_threshold = TIER_THREE_THRESHOLD
	maximum_active_caste = 1
	evolve_min_xenos = 15
	death_evolution_delay = 10 MINUTES

	// *** Flags *** //
	caste_flags = CASTE_IS_INTELLIGENT|CASTE_NO_HEALING|CASTE_NO_PLASMA_REGEN|CASTE_NO_PSYCHIC_SUMMON|CASTE_LEADER_TYPE|CASTE_INSTANT_EVOLUTION
	can_flags = CASTE_CAN_CORRUPT_GENERATOR
	caste_traits = null

	// *** Defense *** //
	soft_armor = list(MELEE = 75, BULLET = 75, LASER = 75, ENERGY = 75, BOMB = 50, BIO = 75, FIRE = 200, ACID = 75)
	hard_armor = list(MELEE = 10, BULLET = 10, LASER = 10, ENERGY = 10, BOMB = 10, BIO = 10, FIRE = 10, ACID = 10)

	minimap_icon = "xenoking"

	actions = list(
		// Actions given to all T0s:
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		// Actions given to all T4s:
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)


/datum/xeno_caste/dragon/normal
	upgrade = XENO_UPGRADE_NORMAL

/datum/xeno_caste/dragon/primordial
	upgrade_name = "Primordial"
	caste_desc = "Ancient terror. Your end has come, and it bears my wings."
	primordial_message = "Destruction is my creed; none shall withstand my fury."
	upgrade = XENO_UPGRADE_PRIMO

	actions = list(
		// Actions given to all T0s:
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		// Actions given to all T4s:
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)
