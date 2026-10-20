// the light item
// can be tube or bulb subtypes
// will fit into empty /obj/machinery/light of the corresponding type

/obj/item/light_bulb
	icon = 'icons/obj/lighting.dmi'
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/equipment/lights_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/equipment/lights_right.dmi',
	)
	force = 2
	throwforce = 5
	w_class = WEIGHT_CLASS_SMALL
	base_icon_state
	///Condition of the bulb
	var/status = LIGHT_OK
	///Number of times toggled
	var/switchcount = 0
	///If its rigged to explode
	var/rigged = 0
	///Light level this gives off when on
	var/brightness = 2


/obj/item/light_bulb/Initialize(mapload)
	. = ..()
	switch(name)
		if("light tube")
			brightness = rand(6,9)
		if("light bulb")
			brightness = rand(4,6)
	update()

/obj/item/light_bulb/throw_impact(atom/hit_atom)
	. = ..()
	if(!.)
		return
	shatter()

/obj/item/light_bulb/attacked_by(obj/item/attacking_item, mob/living/user, list/modifiers)
	. = ..()
	if(!.)
		return
	// Certain things (/obj/machinery/light) already have handled interaction with us, so anything else are unhandled and shall be shattered.
	to_chat(world, "attacked_by shatter")
	shatter()

/obj/item/light_bulb/afterattack(atom/target_atom, mob/user, list/modifiers)
	// Certain things (/obj/machinery/light) already have handled interaction with us, so anything else are unhandled and shall be shattered.
	to_chat(world, "afterattack shatter")

/obj/item/light_bulb/proc/shatter()
	if(status == LIGHT_OK || status == LIGHT_BURNED)
		src.visible_message(span_warning("[name] shatters."),span_warning("You hear a small glass object shatter."))
		status = LIGHT_BROKEN
		force = 5
		sharp = IS_SHARP_ITEM_SIMPLE
		playsound(src.loc, 'sound/effects/Glasshit.ogg', 25, 1, MEDIUM_SOUND_RANGE)
		update()

// Updates the icon state and description of the light
/obj/item/light_bulb/proc/update()
	switch(status)
		if(LIGHT_OK)
			icon_state = base_icon_state
			desc = "A replacement [name]."
			worn_icon_state = "ltube"
		if(LIGHT_BURNED)
			icon_state = "[base_icon_state]_burned"
			desc = "A burnt-out [name]."
			worn_icon_state = "ltube-burned"
		if(LIGHT_BROKEN)
			icon_state = "[base_icon_state]_broken"
			desc = "A broken [name]."
			worn_icon_state = "ltube-broken"

/obj/item/light_bulb/tube
	name = "light tube"
	desc = "A replacement light tube."
	icon_state = "ltube"
	base_icon_state = "ltube"
	worn_icon_state = "ltube"
	brightness = 8

/obj/item/light_bulb/tube/blue
	icon_state = "btube1"

/obj/item/light_bulb/tube/large
	w_class = WEIGHT_CLASS_SMALL
	name = "large light tube"
	brightness = 15

/obj/item/light_bulb/bulb
	name = "light bulb"
	desc = "A replacement light bulb."
	icon_state = "lbulb"
	base_icon_state = "lbulb"
	brightness = 5

/obj/item/light_bulb/bulb/blue
	icon_state = "bbulb1"

/obj/item/light_bulb/bulb/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	. = ..()
	if(.)
		return

	var/turf/open/floor/light/floorlight_turf = interacting_with
	if(!istype(floorlight_turf))
		return NONE
	if(status != LIGHT_OK)
		to_chat(user, span_notice("The replacement bulb is broken."))
		return ITEM_INTERACT_BLOCKING
	var/obj/item/stack/tile/light/existing_bulb = floorlight_turf.floor_tile
	if(existing_bulb.state == LIGHT_TILE_OK)
		to_chat(user, span_notice("The lightbulb seems fine, no need to replace it."))
		return ITEM_INTERACT_BLOCKING
	if(!user.drop_held_item(src))
		to_chat(user, span_warning("The [src] is stuck to your hand!"))
		return ITEM_INTERACT_BLOCKING

	qdel(src)
	existing_bulb.state = LIGHT_TILE_OK
	existing_bulb.update_icon()
	to_chat(user, span_notice("You replace the light bulb."))
	return ITEM_INTERACT_SUCCESS

/obj/item/light_bulb/bulb/item_interaction(mob/living/user, obj/item/tool, list/modifiers)
	var/obj/item/reagent_containers/syringe/syringe_tool = tool
	if(istype(syringe_tool))
		to_chat(user, "You inject the solution into the [src].")
		if(syringe_tool.reagents.has_reagent(/datum/reagent/toxin/phoron, 5))
			rigged = TRUE
		syringe_tool.reagents.clear_reagents()
		return ITEM_INTERACT_SUCCESS
	return NONE
