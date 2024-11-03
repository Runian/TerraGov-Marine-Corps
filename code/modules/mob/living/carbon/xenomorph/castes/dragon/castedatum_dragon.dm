/* The TODO list in order: https://hackmd.io/@wlLEbadtQoCyE4iytTkonw/S10wubC1A
base xeno_caste, xeno, etc ✔
Health ✔
Soft Armor ✔
Hard Armor ✔
Speed ✔
ms-paint level of Sprites
cool descriptions!
15 xeno limit ✔
Immunity to CC: slow / stun / fling / stagger
give caste_flag to prevent plasma/health regen on weeds
no pheromone given
no king summon allowed
no slash like Hivemind
can speak english
unique resource (kind of like blood/rage) except its yellow
no benefit from friendly xeno skills (like deathwing in HoTS!) :)
EVERYTHING ELSE WAHHHH
REAL Sprites
*/

/datum/xeno_caste/dragon
	caste_name = "Dragon"
	display_name = "Dragon"
	upgrade_name = ""
	caste_type_path = /mob/living/carbon/xenomorph/dragon
	caste_desc = "A dwagon! Wowza! Just imagine something cool was here and be amazed!"

	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_BASETYPE
	wound_type = "dragon" //used to match appropriate wound overlays

	// *** Melee Attacks *** //
	melee_damage = 33

	// *** Speed *** //
	speed = -0.6

	// *** Plasma *** //
	plasma_max = 1200
	plasma_gain = 90

	// *** Health *** //
	max_health = 1650 // Calm (750) + Enraged (900)

	// *** Evolution *** //
	upgrade_threshold = TIER_THREE_THRESHOLD
	maximum_active_caste = 1
	evolve_min_xenos = 15
	death_evolution_delay = 7 MINUTES

	// *** Flags *** //
	caste_flags = CASTE_IS_INTELLIGENT|CASTE_STAGGER_RESISTANT|CASTE_LEADER_TYPE|CASTE_INSTANT_EVOLUTION|CASTE_HAS_WOUND_MASK
	can_flags = CASTE_CAN_BE_QUEEN_HEALED|CASTE_CAN_BE_GIVEN_PLASMA|CASTE_CAN_BE_LEADER|CASTE_CAN_CORRUPT_GENERATOR
	caste_traits = null

	// *** Defense *** //
	soft_armor = list(MELEE = 75, BULLET = 75, LASER = 75, ENERGY = 75, BOMB = 50, BIO = 75, FIRE = 200, ACID = 75)
	hard_armor = list(MELEE = 10, BULLET = 10, LASER = 10, ENERGY = 10, BOMB = 10, BIO = 10, FIRE = 10, ACID = 10)

	minimap_icon = "xenoking"

	actions = list(
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		/datum/action/ability/activable/xeno/cocoon,
		/datum/action/ability/activable/xeno/plant_weeds,
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/activable/xeno/corrosive_acid/strong,
		/datum/action/ability/activable/xeno/nightfall,
		/datum/action/ability/xeno_action/petrify,
		/datum/action/ability/activable/xeno/off_guard,
		/datum/action/ability/activable/xeno/shattering_roar,
		/datum/action/ability/xeno_action/psychic_summon,
		/datum/action/ability/xeno_action/pheromones,
		/datum/action/ability/xeno_action/pheromones/emit_recovery,
		/datum/action/ability/xeno_action/pheromones/emit_warding,
		/datum/action/ability/xeno_action/pheromones/emit_frenzy,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)


/datum/xeno_caste/dragon/normal
	upgrade = XENO_UPGRADE_NORMAL

/datum/xeno_caste/dragon/primordial
	upgrade_name = "Primordial"
	caste_desc = "An avatar of death. Running won't help you now."
	primordial_message = "Death cannot create, but you definitely know how to destroy."
	upgrade = XENO_UPGRADE_PRIMO

	actions = list(
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		/datum/action/ability/activable/xeno/cocoon,
		/datum/action/ability/activable/xeno/plant_weeds,
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/activable/xeno/corrosive_acid/strong,
		/datum/action/ability/activable/xeno/nightfall,
		/datum/action/ability/xeno_action/petrify,
		/datum/action/ability/activable/xeno/off_guard,
		/datum/action/ability/activable/xeno/shattering_roar,
		/datum/action/ability/xeno_action/zero_form_beam,
		/datum/action/ability/xeno_action/psychic_summon,
		/datum/action/ability/xeno_action/pheromones,
		/datum/action/ability/xeno_action/pheromones/emit_recovery,
		/datum/action/ability/xeno_action/pheromones/emit_warding,
		/datum/action/ability/xeno_action/pheromones/emit_frenzy,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)
