// Recently sniped status effect. This is applied when hit by projectles with AMMO_SNIPER that have IFF.
/datum/status_effect/grouped/recently_sniped
	id = "sniped"
	/// Used for the sniped effect.
	var/obj/vis_sniped/visual_sniped

/datum/status_effect/grouped/recently_sniped/on_creation(mob/living/new_owner, set_duration)
	. = ..()

	if(!. || new_owner.stat != CONSCIOUS)
		return FALSE

	visual_sniped = new
	visual_sniped.icon_state = "sniper_zoom"

	new_owner.vis_contents += visual_sniped

/datum/status_effect/grouped/recently_sniped/on_remove()
	owner.vis_contents -= visual_sniped
	QDEL_NULL(visual_sniped)

/obj/vis_sniped
	name = "sniped"
	icon = 'icons/mob/actions.dmi'
	pixel_x = 16
	pixel_y = 10
	layer = ABOVE_MOB_LAYER
	mouse_opacity = MOUSE_OPACITY_TRANSPARENT
	vis_flags = VIS_INHERIT_DIR | VIS_INHERIT_ID | VIS_INHERIT_PLANE
