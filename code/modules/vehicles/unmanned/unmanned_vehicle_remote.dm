/obj/item/unmanned_vehicle_remote
	name = "handheld vehicle controller"
	desc = "Used to control an unmanned vehicle.<br>Tap the vehicle you want to control with the controller to link it."
	icon = 'icons/obj/device.dmi'
	icon_state = "multitool2"
	w_class = WEIGHT_CLASS_SMALL
	/// Reference to the unmanned vehicle that we're connected to or remote control.
	var/obj/vehicle/unmanned/vehicle

/obj/item/unmanned_vehicle_remote/Destroy()
	. = ..()
	clear_vehicle()

/obj/item/unmanned_vehicle_remote/attack_self(mob/user)
	if(SEND_SIGNAL(src, COMSIG_REMOTECONTROL_TOGGLE, user))
		return TRUE
	return ..()

/obj/item/unmanned_vehicle_remote/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	. = ..()
	if(.)
		return
	if(!istype(interacting_with, /obj/vehicle/unmanned))
		return NONE

	if(vehicle)
		SEND_SIGNAL(src, COMSIG_REMOTECONTROL_UNLINK)
		if(vehicle == interacting_with)
			to_chat(user, span_notice("You unlink [interacting_with] from [src]."))
			clear_vehicle()
			return ITEM_INTERACT_SUCCESS
		clear_vehicle()
	vehicle = interacting_with
	if(vehicle.controlled)
		to_chat(user, span_warning("Something is already controlling this vehicle."))
		vehicle = null
		return ITEM_INTERACT_BLOCKING
	vehicle.on_link(src)
	AddComponent(/datum/component/remote_control, interacting_with, vehicle.turret_type, vehicle.can_interact)
	to_chat(user, span_notice("You link [interacting_with] to [src]."))
	RegisterSignal(interacting_with, COMSIG_QDELETING, PROC_REF(clear_vehicle))
	return ITEM_INTERACT_SUCCESS

/// Wrapper to clear reference on target vehicle deletion.
/obj/item/unmanned_vehicle_remote/proc/clear_vehicle()
	SIGNAL_HANDLER
	if(!vehicle)
		return
	UnregisterSignal(vehicle, COMSIG_QDELETING)
	vehicle.on_unlink(src)
	vehicle = null
