/mob/tech_hologram
	name = "Tech Hologram"
	icon = 'icons/mob/mob.dmi'
	icon_state = "blank" // This doesn't really matter since we're invisible.

	invisibility = INVISIBILITY_OBSERVER
	sight = SEE_SELF

	/// The action that makes the controller stop controlling the hologram.
	var/datum/action/leave_hologram/leave_button
	/// The mob that is controlling us.
	var/mob/linked_mob

/mob/tech_hologram/Initialize(mapload, mob/hologram)
	if(!hologram)
		return INITIALIZE_HINT_QDEL

	. = ..()

	RegisterSignal(hologram, COMSIG_MOVABLE_PRE_MOVE, PROC_REF(handle_move))
	RegisterSignals(hologram, list(COMSIG_LIVING_STATUS_STUN,
		COMSIG_LIVING_STATUS_KNOCKDOWN,
		COMSIG_LIVING_STATUS_PARALYZE,
		COMSIG_LIVING_STATUS_UNCONSCIOUS,
		COMSIG_LIVING_STATUS_SLEEP,
		COMSIG_LIVING_STATUS_STAGGER,
		COMSIG_LIVING_IGNITED,
		COMSIG_LIVING_TAKE_DAMAGE), PROC_REF(leave_hologram))

	linked_mob = hologram
	linked_mob.reset_perspective(src)

	leave_button = new /datum/action/leave_hologram(null, src)
	leave_button.linked_hologram = src
	leave_button.give_action(linked_mob)

/mob/tech_hologram/Destroy()
	if(linked_mob)
		linked_mob.reset_perspective()
		UnregisterSignal(linked_mob, list(COMSIG_MOVABLE_PRE_MOVE,
			COMSIG_LIVING_STATUS_STUN,
			COMSIG_LIVING_STATUS_KNOCKDOWN,
			COMSIG_LIVING_STATUS_PARALYZE,
			COMSIG_LIVING_STATUS_UNCONSCIOUS,
			COMSIG_LIVING_STATUS_SLEEP,
			COMSIG_LIVING_STATUS_STAGGER,
			COMSIG_LIVING_IGNITED,
			COMSIG_LIVING_TAKE_DAMAGE))
		linked_mob = null
	if(!QDELETED(leave_button))
		QDEL_NULL(leave_button)
	return ..()

/// Deletes us.
/mob/tech_hologram/proc/leave_hologram(datum/source)
	SIGNAL_HANDLER
	qdel(src)

/// Takes the controller's movement for own.
/mob/tech_hologram/proc/handle_move(datum/source, atom/newloc, direction)
	SIGNAL_HANDLER

	Move(get_step(loc, direction), direction)
	return COMPONENT_MOVABLE_BLOCK_PRE_MOVE

/datum/action/leave_hologram
	name = "Leave"
	icon_state = "hologram_leave"
	/// The hologram that we're linked to.
	var/mob/tech_hologram/linked_hologram

/datum/action/leave_hologram/action_activate()
	. = ..()
	if(!.)
		return
	qdel(src)

/datum/action/leave_hologram/Destroy()
	if(!QDELETED(linked_hologram))
		QDEL_NULL(linked_hologram)
	return ..()
