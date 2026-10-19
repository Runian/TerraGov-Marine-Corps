/obj/item/reagent_containers/jerrycan
	name = "\improper jerry can"
	desc = "A can filled with fuel to light things on fire. It has Absolut Jerry stamped in the side."
	icon = 'icons/obj/items/tank.dmi'
	icon_state = "canister"
	w_class = WEIGHT_CLASS_BULKY
	volume = 200
	list_reagents = list(/datum/reagent/fuel = 200)
	/// How much fuel we use up per spill?
	var/fuel_usage = 10

/obj/item/reagent_containers/jerrycan/examine(mob/user as mob)
	. = ..()
	if(!reagents)
		return
	if(get_dist(user, src) > 2)
		. += span_warning("You're too far away to see [src]'s reagent amount!")
		return
	. += "There is [reagents.reagent_list ? reagents.total_volume : 0] units of fuel remaining."

/obj/item/reagent_containers/jerrycan/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!isturf(interacting_with) && !istype(interacting_with, /obj/alien/weeds))
		return NONE
	var/turf/found_turf = get_turf(interacting_with)
	if(found_turf.density)
		return ITEM_INTERACT_BLOCKING
	if(!reagents.total_volume)
		balloon_alert(user, "no fuel!")
		return ITEM_INTERACT_BLOCKING
	new /obj/effect/decal/cleanable/liquid_fuel(found_turf, fuel_usage / 2)
	reagents.remove_reagent(/datum/reagent/fuel, fuel_usage)
	user.visible_message(span_notice("[user] splashes some fuel on \the [found_turf]."), span_notice("You splash some fuel on [found_turf]."))
	log_attack("[key_name(user)] has splashed fuel on [found_turf] in [AREACOORD(user)]")
	found_turf.add_fingerprint(user, "pour_fuel", "doused with fuel from [src]")
	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_containers/jerrycan/attack(mob/living/target_mob, mob/living/user, list/modifiers)
	. = ..()
	if(!.)
		return FALSE
	if(!reagents.total_volume)
		balloon_alert(user, "no fuel!")
		return TRUE
	target_mob.adjust_fire_stacks(10)
	reagents.remove_reagent(/datum/reagent/fuel, fuel_usage)
	user.visible_message(span_notice("[user] splashes some fuel on [target_mob]."), span_notice("You splash some fuel on [target_mob]."), ignored_mob = target_mob)
	to_chat(target_mob, span_warning("[user] drenches you in fuel from [src]!"))
	log_attack("[key_name(user)] has doused [target_mob] in fuel in [AREACOORD(user)]")
