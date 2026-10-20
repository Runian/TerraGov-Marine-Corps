/obj/item/tool/mop
	desc = "The world of janitalia wouldn't be complete without a mop."
	name = "mop"
	icon = 'icons/obj/janitor.dmi'
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/items/janitor_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/items/janitor_right.dmi',
	)
	icon_state = "mop"
	force = 3
	throwforce = 10
	throw_speed = 5
	throw_range = 10
	w_class = WEIGHT_CLASS_NORMAL
	attack_verb = list("mops", "bashes", "bludgeons", "whacks")

/obj/item/tool/mop/Initialize(mapload)
	. = ..()
	create_reagents(5)

/obj/item/tool/mop/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(reagents.total_volume < 1)
		balloon_alert(user, "mop is dry!")
		return ITEM_INTERACT_BLOCKING

	var/turf/turf_to_clean = get_turf(interacting_with)
	user.visible_message(
		span_notice("[user] begins to clean \the [turf_to_clean]."),
		span_notice("You begin to clean \the [turf_to_clean].")
	)
	if(!do_after(user, 4 SECONDS, NONE, turf_to_clean, BUSY_ICON_GENERIC))
		return ITEM_INTERACT_BLOCKING
	balloon_alert(user, "finished")

	if(reagents.has_reagent(/datum/reagent/water, 1))
		turf_to_clean.wash()
	reagents.reaction(turf_to_clean, TOUCH, 10) // 10 is the multiplier for the reaction effect. Probably needed to wet the floor properly.
	reagents.remove_any(1) // reaction() doesn't use up the reagents.
	return ITEM_INTERACT_SUCCESS

/obj/item/tool/wet_sign
	name = "wet floor sign"
	desc = "Caution! Wet Floor!"
	icon_state = "caution"
	icon = 'icons/obj/janitor.dmi'
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/items/janitor_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/items/janitor_right.dmi',
	)
	force = 1
	throwforce = 3
	throw_speed = 1
	throw_range = 5
	w_class = WEIGHT_CLASS_SMALL
	attack_verb = list("warns", "cautions", "smashes")

/obj/item/clothing/head/warning_cone
	name = "warning cone"
	desc = "This cone is trying to warn you of something!"
	icon_state = "cone"
	icon = 'icons/obj/janitor.dmi'
	worn_icon_list = list(slot_head_str = 'icons/mob/clothing/headwear/head_0.dmi')
	force = 1
	throwforce = 3
	throw_speed = 1
	throw_range = 5
	w_class = WEIGHT_CLASS_SMALL
	attack_verb = list("warns", "cautions", "smashes")
	soft_armor = list(MELEE = 30, BULLET = 30, LASER = 30, ENERGY = 30, BOMB = 15, BIO = 10, FIRE = 20, ACID = 20)

/obj/item/tool/soap
	name = "soap"
	desc = "A cheap bar of soap. Doesn't smell."
	gender = PLURAL
	icon = 'icons/obj/janitor.dmi'
	icon_state = "soap"
	w_class = WEIGHT_CLASS_TINY
	throw_speed = 4
	throw_range = 20

/obj/item/tool/soap/Initialize(mapload)
	. = ..()
	AddComponent(/datum/component/slippery, 0.3 SECONDS, 0.2 SECONDS)

/obj/item/tool/soap/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(isturf(interacting_with))
		balloon_alert(user, "scrubbed")
		var/turf/interacting_turf = interacting_with
		interacting_turf.wash()
		return ITEM_INTERACT_SUCCESS
	if(istype(interacting_with, /obj/effect/decal/cleanable))
		balloon_alert(user, "scrubbed!")
		qdel(interacting_with)
		return ITEM_INTERACT_SUCCESS
	if(ishuman(interacting_with) && user.zone_selected == BODY_ZONE_PRECISE_MOUTH)
		var/mob/living/carbon/human/interacting_human = interacting_with
		if(!(interacting_human.head?.inventory_flags & COVERMOUTH) && !(interacting_human.wear_mask?.inventory_flags & COVERMOUTH))
			balloon_alert_to_viewers("washes mouth out with soap!")
			return ITEM_INTERACT_SUCCESS
		balloon_alert(user, "mouth covered!")
		return ITEM_INTERACT_BLOCKING
	balloon_alert(user, "cleaned!")
	interacting_with.wash()
	return ITEM_INTERACT_SUCCESS

/obj/item/tool/soap/nanotrasen
	desc = "A Nanotrasen brand bar of soap. Smells of phoron."
	icon_state = "soapnt"

/obj/item/tool/soap/deluxe
	icon_state = "soapdeluxe"

/obj/item/tool/soap/deluxe/Initialize(mapload)
	. = ..()
	desc = "A deluxe Waffle Co. brand bar of soap. Smells of [pick("lavender", "vanilla", "strawberry", "chocolate" ,"space")]."

/obj/item/tool/soap/syndie
	desc = "An untrustworthy bar of soap. Smells of fear."
	icon_state = "soapsyndie"
