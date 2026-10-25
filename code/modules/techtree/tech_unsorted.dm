/area/techtree
	base_lighting_alpha = 255
	requires_power = FALSE

/turf/open/blank
	name = "Blank"
	icon = 'icons/effects/effects.dmi'
	icon_state = "light"
	color = COLOR_WHITE
	mouse_opacity = FALSE
	can_bloody = FALSE

/turf/closed/void
	name = "void"
	icon = 'icons/effects/effects.dmi'
	icon_state = "light"
	color = COLOR_BLACK
	mouse_opacity = FALSE
	can_bloody = FALSE

/obj/effect/technode
	name = "tech node"

	var/datum/tech/info

/obj/effect/technode/Initialize(mapload, datum/tech/tech)
	. = ..()
	if(!tech)
		stack_trace("Tech node initialized without a tech to attach to! (Expected /datum/tech type, got [tech])")
		return INITIALIZE_HINT_QDEL

	info = tech
	name = tech.name
	tech.node = src

	tech.update_icon(src)

/obj/effect/technode/update_icon()
	overlays.Cut()

	. = ..()
	info.update_icon(src)

//obj/effect/technode/clicked(mob/user, list/mods)
	//. = ..()
	//tgui_interact(user)
	//return TRUE

///obj/effect/technode/tgui_interact(mob/user, datum/tgui/ui)
//	if(!info)
//		qdel(src)
//		return
	//info.tgui_interact(user, ui)
