/obj/item/stack/rods
	name = "metal rod"
	desc = "Some rods. Can be used for building, or something."
	singular_name = "metal rod"
	icon_state = "rods"
	atom_flags = CONDUCT
	w_class = WEIGHT_CLASS_NORMAL
	force = 9
	throwforce = 15
	throw_speed = 5
	throw_range = 20
	max_amount = 60
	attack_verb = list("hits", "bludgeons", "whacks")

/obj/item/stack/rods/welder_act(mob/living/user, obj/item/tool)
	if(amount < 4)
		to_chat(user, span_warning("You need at least four rods to do this."))
		return ITEM_INTERACT_BLOCKING

	if(!tool.use(0))
		return ITEM_INTERACT_BLOCKING

	var/obj/item/stack/sheet/metal/new_item = new(user.loc)
	new_item.add_to_stacks(user)
	visible_message(span_warning("[src] is shaped into metal by [user] with the weldingtool."), null, span_warning("You hear welding."))
	use(4)
	return ITEM_INTERACT_SUCCESS

/obj/item/stack/rods/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(istype(interacting_with, /turf/open/liquid))
		place_catwalk(interacting_with, user)
		return ITEM_INTERACT_SUCCESS
	if(isfloorturf(interacting_with))
		reinforce_floor(interacting_with, user)
		return ITEM_INTERACT_SUCCESS
	return NONE

/obj/item/stack/rods/item_interaction(mob/living/user, obj/item/tool, list/modifiers)
	var/obj/item/stack/barbed_wire/barbed_wire = tool
	if(!istype(tool))
		return NONE
	if(amount < 8)
		to_chat(user, span_warning("You need at least [8 - amount] more [src] to make razorwire obstacles!"))
		return ITEM_INTERACT_BLOCKING
	if(barbed_wire.amount < 2)
		to_chat(user, span_warning("You need [2 - barbed_wire.amount] more [barbed_wire] to make a razor wire obstacle!"))
		return ITEM_INTERACT_BLOCKING
	var/obj/item/stack/razorwire/crafted_razorwire = new(user.loc, 2)
	crafted_razorwire.add_to_stacks(user)
	to_chat(user, span_notice("You combine the rods and barbed wire into [crafted_razorwire]!"))
	use(8)
	barbed_wire.use(2)
	return ITEM_INTERACT_SUCCESS

/obj/item/stack/rods/attack_self(mob/user)
	. = ..()
	if(.)
		return TRUE
	var/turf/current_turf = get_turf(user.loc)
	var/obj/structure/grille/premade_grille = locate() in current_turf
	if(premade_grille)
		if(premade_grille.obj_integrity <= premade_grille.integrity_failure)
			premade_grille.repair_damage(10, user)
			premade_grille.density = TRUE
			premade_grille.icon_state = "grille"
			use(1)
			return TRUE
		return FALSE

	if(user.do_actions)
		return FALSE

	if(amount < 4)
		to_chat(user, span_notice("You need at least four rods to do this."))
		return TRUE

	if(!do_after(usr, 2 SECONDS, NONE, src, BUSY_ICON_BUILD))
		return TRUE

	new /obj/structure/grille(current_turf)
	to_chat(user, span_notice("You assemble a grille."))
	use(4)

///Builds a catwalk
/obj/item/stack/rods/proc/place_catwalk(turf/target_turf, mob/living/user)
	if(target_turf.is_covered())
		user.balloon_alert(user, "already covered!")
		return
	if(amount < CATWALK_ROD_REQ)
		user.balloon_alert(user, "[CATWALK_ROD_REQ] rods needed!")
		return
	user.balloon_alert(user, "building...")
	if(!do_after(user, 5 SECONDS, NONE, src, BUSY_ICON_BUILD))
		return
	if(target_turf.is_covered())
		user.balloon_alert(user, "already covered!")
		return
	if(!use(CATWALK_ROD_REQ))
		user.balloon_alert(user, "[CATWALK_ROD_REQ] rods needed!")
		return
	playsound(target_turf, 'sound/weapons/genhit.ogg', 50, TRUE)
	new /obj/structure/catwalk(target_turf)

///Reinforces a bare floor
/obj/item/stack/rods/proc/reinforce_floor(turf/target_turf, mob/living/user)
	if(!istype(target_turf, /turf/open/floor/plating))
		user.balloon_alert(user, "remove the plating!")
		return
	if(amount < REINFORCED_FLOOR_ROD_REQ)
		user.balloon_alert(user, "[CATWALK_ROD_REQ] rods needed!")
		return
	user.balloon_alert(user, "reinforcing floor...")
	if(!do_after(user, 3 SECONDS, NONE, src, BUSY_ICON_BUILD))
		return
	if(!istype(target_turf, /turf/open/floor/plating))
		user.balloon_alert(user, "remove the plating!")
		return
	if(!use(REINFORCED_FLOOR_ROD_REQ))
		user.balloon_alert(user, "[CATWALK_ROD_REQ] rods needed!")
		return
	target_turf.ChangeTurf(/turf/open/floor/engine)
	playsound(target_turf, 'sound/items/deconstruct.ogg', 25, TRUE, MEDIUM_SOUND_RANGE)
