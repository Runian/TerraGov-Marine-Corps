/datum/tech/transitory
	name = "Transitory tech"
	desc = "Transitions the tree to another tier."
	icon_state = "upgrade"

/datum/tech/transitory/tier1
	name = "Unlock Tier 1"
	flags = TECH_FLAG_MARINE
	required_tier = /datum/tech_tier/zero_to_one

/datum/tech/transitory/tier2
	name = "Unlock Tier 2"
	flags = TECH_FLAG_MARINE
	required_tier = /datum/tech_tier/one_to_two

/datum/tech/transitory/tier3
	name = "Unlock Tier 3"
	flags = TECH_FLAG_MARINE
	required_tier = /datum/tech_tier/two_to_three

/datum/tech/transitory/tier4
	name = "Unlock Tier 4"
	flags = TECH_FLAG_MARINE
	required_tier = /datum/tech_tier/three_to_four
