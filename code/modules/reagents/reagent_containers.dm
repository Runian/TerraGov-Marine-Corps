/obj/item/reagent_containers
	name = "Container"
	desc = ""
	icon = 'icons/obj/items/chemistry.dmi'
	icon_state = null
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/equipment/medical_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/equipment/medical_right.dmi',
	)
	throwforce = 3
	w_class = WEIGHT_CLASS_SMALL
	throw_speed = 1
	throw_range = 5
	/// The maximum amount of reagents per transfer that will be moved out of this reagent container.
	var/amount_per_transfer_from_this = 5
	/// The different possible amounts of reagent to transfer out of the container
	var/list/possible_transfer_amounts = list(5,10,15,25,30)
	/// The maximum amount of reagents this container can hold
	var/volume = 30
	/// Reagent flags, a few examples being if the container is open or not, if its transparent, if you can inject stuff in and out of the container, and so on
	var/reagent_flags
	///Can liquify/grind pills without needing fluid to dissolve.
	var/liquifier = FALSE
	///List of reagents to add
	var/list/list_reagents
	///Whether we can restock this in a vendor without it having its starting reagents
	var/free_refills = TRUE

	/**
	 * The different thresholds at which the reagent fill overlay will change. See medical/reagent_fillings.dmi.
	 *
	 * Should be a list of integers which correspond to a reagent unit threshold.
	 * If null, no automatic fill overlays are generated.
	 *
	 * For example, list(0) will mean it will gain a the overlay with any reagents present. This overlay is "overlayname0".
	 * list(0, 10) whill have two overlay options, for 0-10 units ("overlayname0") and 10+ units ("overlayname10").
	 */
	var/list/fill_icon_thresholds = null
	/// The optional custom name for the reagent fill icon_state prefix
	/// If not set, uses the current icon state.
	var/fill_icon_state = null
	/// The icon file to take fill icon appearances from
	var/fill_icon = 'icons/obj/reagents/reagent_fillings.dmi'

/obj/item/reagent_containers/Initialize(mapload)
	. = ..()
	create_reagents(volume, reagent_flags, list_reagents)
	if(!possible_transfer_amounts)
		verbs -= /obj/item/reagent_containers/verb/change_transfer_amount_verb

/obj/item/reagent_containers/create_reagents(max_vol, flags)
	. = ..()
	RegisterSignals(reagents, list(COMSIG_REAGENTS_NEW_REAGENT, COMSIG_REAGENTS_ADD_REAGENT, COMSIG_REAGENTS_DEL_REAGENT, COMSIG_REAGENTS_REM_REAGENT), PROC_REF(on_reagent_add))

/obj/item/reagent_containers/attack_hand_alternate(mob/living/user)
	. = ..()
	if(.)
		return TRUE
	return change_transfer_amount(user)

/obj/item/reagent_containers/attack_self_secondary(mob/living/user)
	. = ..()
	if(.)
		return TRUE
	return change_transfer_amount(user)

/obj/item/reagent_containers/unique_action(mob/user, special_treatment)
	. = ..()
	if(.)
		return TRUE
	return change_transfer_amount(user)

/obj/item/reagent_containers/update_overlays()
	. = ..()
	if(!fill_icon_thresholds)
		return
	if(!reagents.total_volume)
		return

	var/fill_name = fill_icon_state ? fill_icon_state : icon_state
	var/mutable_appearance/filling = mutable_appearance(fill_icon, "[fill_name][fill_icon_thresholds[1]]")

	var/percent = round((reagents.total_volume / volume) * 100)
	for(var/i in 1 to fill_icon_thresholds.len)
		var/threshold = fill_icon_thresholds[i]
		var/threshold_end = (i == fill_icon_thresholds.len) ? INFINITY : fill_icon_thresholds[i+1]
		if(threshold <= percent && percent < threshold_end)
			filling.icon_state = "[fill_name][fill_icon_thresholds[i]]"

	filling.color = mix_color_from_reagents(reagents.reagent_list)
	. += filling

/// Opens a tgui_input_list and changes the transfer_amount of our container based on our selection.
/obj/item/reagent_containers/proc/change_transfer_amount(mob/living/user)
	if(!possible_transfer_amounts)
		return FALSE
	var/result = tgui_input_list(user, "Amount per transfer from this:", "[src]", possible_transfer_amounts)
	if(result)
		amount_per_transfer_from_this = result
	return TRUE

/// Verb that lets the user to change the transfer amount.
/obj/item/reagent_containers/verb/change_transfer_amount_verb()
	set name = "Set transfer amount"
	set category = "IC.Object"
	set src in view(1)

	change_transfer_amount(usr)

// Returns a text listing the reagents (and their volume) in the atom. Used by attack logs for reagents in pills.
/obj/item/reagent_containers/proc/get_reagent_list_text()
	if(!reagents.reagent_list ||!length(reagents.reagent_list))
		return "No reagents"

	var/datum/reagent/next_reagent = reagents.reagent_list[1]
	. = "[next_reagent.name]([next_reagent.volume]u)"

	if(length(reagents.reagent_list) < 2)
		return

	for(var/i = 2, i <= length(reagents.reagent_list), i++)
		next_reagent = reagents.reagent_list[i]
		if(!next_reagent)
			continue
		. += "; [next_reagent.name]([next_reagent.volume]u)"

/// Returns true if this object currently contains at least its starting reagents, false otherwise. Extra reagents are ignored.
/obj/item/reagent_containers/proc/has_initial_reagents()
	for(var/reagent_to_check in list_reagents)
		if(reagents.get_reagent_amount(reagent_to_check) != list_reagents[reagent_to_check])
			return FALSE
	return TRUE

/// Updates the icon of the container when the reagents change.
/obj/item/reagent_containers/proc/on_reagent_add(datum/source)
	SIGNAL_HANDLER
	update_appearance()

/*
 * Reagent containers are not necessarily things that you can refill, pour, drink, etc.
 * Because of this, these procs below are here to reduce copy and paste & maintain consistency across everything.
 *
 * These subtypes use them:
 * /obj/item/reagent_containers/cup
 * /obj/item/reagent_containers/glass
 * /obj/item/reagent_containers/cup/glass
 * /obj/item/reagent_containers/food/drinks
*/

/// Can the target drink at all from us?
/obj/item/reagent_containers/proc/can_consume(mob/living/carbon/target, mob/user)
	if(!istype(target))
		return FALSE
	if(!reagents || !reagents.total_volume)
		to_chat(user, span_warning("[src] is empty!"))
		return FALSE
	if(ishuman(target) && (target.species.species_flags & ROBOTIC_LIMBS))
		to_chat(user, span_warning("[user == target ? "You" : target.p_they()] have a monitor for a head. Where do you think you're going to put that?"))
		return FALSE
	return TRUE

/// Causes the target to drink from us.
/obj/item/reagent_containers/proc/try_consume(atom/target, mob/living/user, transfer_amount = amount_per_transfer_from_this)
	if(!can_consume(target)) // Will give varied chat message if it fails.
		return ITEM_INTERACT_BLOCKING
	if(!reagents || !reagents.total_volume)
		to_chat(user, span_warning("[src] is empty!"))
		return ITEM_INTERACT_BLOCKING

	user.changeNext_move(CLICK_CD_MELEE)
	if(target == user)
		user.visible_message(
			span_danger("[user] swallows some of the contents of \the [src]."),
			span_userdanger("You swallow some of the contents of \the [src]."),
		)
	else
		target.visible_message(
			span_danger("[user] attempts to feed [target] something from [src]."),
			span_userdanger("[user] attempts to feed you something from [src]."),
		)
		if(!do_after(user, 3 SECONDS, target))
			return ITEM_INTERACT_BLOCKING
		if(!reagents || !reagents.total_volume)
			return ITEM_INTERACT_BLOCKING
		target.visible_message(
			span_warning("[user] feeds [target] something from [src]."),
			span_warning("[user] feeds you something from [src]."),
		)
		if(is_blind(target))
			to_chat(target, span_notice("You feel someone feed you something."))
		log_combat(user, target, "fed", src, "Reagents: [get_reagent_list_text()]")

	SEND_SIGNAL(src, COMSIG_GLASS_DRANK, target, user)

	record_reagent_consumption(min(transfer_amount, reagents.total_volume), reagents.reagent_list, user, target)
	if(reagents.total_volume)
		reagents.reaction(target, INGEST)
		reagents.trans_to(target, transfer_amount)
	playsound(target, 'sound/items/drink.ogg', rand(10, 50), TRUE)
	target.update_appearance()

	return ITEM_INTERACT_SUCCESS

/// Tries to give/refill target's reagents from our reagents.
/obj/item/reagent_containers/proc/try_refill(atom/target, mob/living/user, transfer_amount = amount_per_transfer_from_this)
	if(!is_drainable())
		to_chat(user, span_notice("[src]'s lid isn't open!"))
		return ITEM_INTERACT_BLOCKING
	if(!reagents.total_volume)
		to_chat(user, span_warning("[src] is empty!"))
		return ITEM_INTERACT_BLOCKING
	if(target.reagents.holder_full())
		to_chat(user, span_warning("[target] is full."))
		return ITEM_INTERACT_BLOCKING

	var/trans = reagents.trans_to(target, transfer_amount)
	to_chat(user, span_notice("You transfer [trans] unit\s of the solution to [target]."))
	SEND_SIGNAL(src, COMSIG_REAGENTS_CUP_TRANSFER_TO, target)
	target.update_appearance()
	return ITEM_INTERACT_SUCCESS

/// Tries to pour/drain target's reagents into us.
/obj/item/reagent_containers/proc/try_drain(atom/target, mob/living/user, transfer_amount = amount_per_transfer_from_this)
	if(!is_refillable())
		to_chat(user, span_notice("[src]'s lid isn't open!"))
		return ITEM_INTERACT_BLOCKING
	if(!target.reagents.total_volume)
		to_chat(user, span_warning("[target] is empty and can't be refilled!"))
		return ITEM_INTERACT_BLOCKING
	if(reagents.holder_full())
		to_chat(user, span_warning("[src] is full."))
		return ITEM_INTERACT_BLOCKING

	var/trans = target.reagents.trans_to(src, transfer_amount)
	to_chat(user, span_notice("You fill [src] with [trans] unit\s of the contents of [target]."))
	SEND_SIGNAL(src, COMSIG_REAGENTS_CUP_TRANSFER_FROM, target)
	target.update_appearance()
	return ITEM_INTERACT_SUCCESS

/// Splashes atom/target with whatever reagents are contained
/obj/item/reagent_containers/proc/try_splash(atom/target, mob/living/user)
	if(!is_open_container()) //Can't splash stuff from a sealed container. I dare you to try.
		to_chat(user, span_warning("An airtight seal prevents you from splashing the solution!"))
		return

	if(ismob(target) && target.reagents && reagents.total_volume)
		to_chat(user, span_notice("You splash the solution onto [target]."))
		playsound(target, 'sound/effects/slosh.ogg', 25, 1, MEDIUM_SOUND_RANGE)

		var/mob/living/M = target
		var/list/injected = list()
		for(var/datum/reagent/R in src.reagents.reagent_list)
			injected += R.name
		var/contained = english_list(injected)
		log_combat(user, M, "splashed", src, "Reagents: [contained]")
		record_reagent_consumption(reagents.total_volume, injected, user, M)

		visible_message(span_warning("[target] has been splashed with something by [user]!"))
		reagents.reaction(target, TOUCH)
		addtimer(CALLBACK(reagents, TYPE_PROC_REF(/datum/reagents, clear_reagents)), 5)
		return


	else if(reagents.total_volume)
		to_chat(user, span_notice("You splash the solution onto [target]."))
		playsound(target, 'sound/effects/slosh.ogg', 25, 1, MEDIUM_SOUND_RANGE)
		reagents.reaction(target, TOUCH)
		addtimer(CALLBACK(reagents, TYPE_PROC_REF(/datum/reagents, clear_reagents)), 5)
		return
