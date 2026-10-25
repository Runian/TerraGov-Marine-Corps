
/obj/structure/tech_control
	name = "tech control console"
	desc = "A console used to make tech purchases."

	icon = 'icons/obj/stock_parts.dmi'
	icon_state = "0"

	density = TRUE
	anchored = TRUE

	var/datum/tech_tree/attached_tree

/obj/structure/tech_control/Initialize()
	. = ..()
	if(SStechtree.initialized)
		attached_tree = GET_TREE(TECH_TREE_MARINE)

/obj/structure/tech_control/Destroy()
	attached_tree = null
	return ..()

// Disallow deconstructing
/obj/structure/tech_control/attackby(obj/item/I, mob/user)
	return

/obj/structure/tech_control/attack_hand(mob/living/user)
	. = ..()

	if(!attached_tree)
		to_chat(world, "There is no attached tree!")
		return

	var/datum/tech_tree/tree = GET_TREE(TECH_TREE_MARINE)
	tree.enter_mob(user, FALSE)
