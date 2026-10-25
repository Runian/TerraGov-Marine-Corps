/datum/tech_tier
	/// The name of the tech tier.
	var/name = "Unnamed Tier"
	/// The value of the tech tier.
	var/value = TECH_TIER_ZERO
	/// The color of the outer line overlay that is applied to all techs associated with this tech tier.
	var/color = COLOR_WHITE
	/// The tech tree that this belongs to.
	var/datum/tech_tree/tech_tree

/datum/tech_tier/New(datum/tech_tree/new_tech_tree)
	. = ..()
	tech_tree = new_tech_tree

/datum/tech_tier/zero
	name = "Tier 0"
	value = TECH_TIER_ZERO

/datum/tech_tier/zero_to_one
	name = "Tier 0 to Tier 1 transition"
	value = TECH_TIER_ZERO
	color = COLOR_GREEN

/datum/tech_tier/one
	name = "Tier 1"
	value = TECH_TIER_ONE
	color = COLOR_GREEN

/datum/tech_tier/one_to_two
	name = "Tier 1 to Tier 2 transition"
	value = TECH_TIER_ONE
	color = "#FFAA00"

/datum/tech_tier/two
	name = "Tier 2"
	value = TECH_TIER_TWO
	color = "#FFAA00"

/datum/tech_tier/two_to_three
	name = "Tier 2 to Tier 3 transition"
	value = TECH_TIER_TWO
	color = COLOR_RED

/datum/tech_tier/three
	name = "Tier 3"
	value = TECH_TIER_THREE
	color = COLOR_RED

/datum/tech_tier/three_to_four
	name = "Tier 3 to Tier 4 transition"
	value = TECH_TIER_THREE
	color = COLOR_MAGENTA

/datum/tech_tier/four
	name = "Tier 4"
	value = TECH_TIER_FOUR
	color = COLOR_MAGENTA
