#define DRUGGY_OVERLAY "high"

// Applies the druggy overlay. That's it.
/datum/status_effect/incapacitating/druggy
	id = "druggy"
	remove_on_fullheal = TRUE

/datum/status_effect/incapacitating/druggy/on_apply()
	. = ..()
	if(!.)
		return
	owner.overlay_fullscreen(DRUGGY_OVERLAY, /atom/movable/screen/fullscreen/high)

/datum/status_effect/incapacitating/druggy/on_remove()
	owner.clear_fullscreen(DRUGGY_OVERLAY)
	return ..()

#undef DRUGGY_OVERLAY
