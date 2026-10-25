#define TECH_TIER_ZERO 0
#define TECH_TIER_ONE 1
#define TECH_TIER_TWO 2
#define TECH_TIER_THREE 3
#define TECH_TIER_FOUR 4

#define TECHTIER_GAMEPLAY list(\
	/datum/tech_tier/zero,\
	/datum/tech_tier/zero_to_one,\
	/datum/tech_tier/one,\
	/datum/tech_tier/one_to_two,\
	/datum/tech_tier/two,\
	/datum/tech_tier/two_to_three,\
	/datum/tech_tier/three,\
	/datum/tech_tier/three_to_four,\
	/datum/tech_tier/four\
)

#define TECH_TREE_NONE "Undefined"
#define TECH_TREE_MARINE "Marine Tech Tree"
#define TECH_TREE_XENOMORPH "Xenomorph Tech Tree"

#define TECH_FLAG_MARINE (1<<0)
#define TECH_FLAG_XENOMORPH (1<<1)

#define GET_TREE(tree_name) (SStechtree ? SStechtree.techtrees[tree_name] : null)
