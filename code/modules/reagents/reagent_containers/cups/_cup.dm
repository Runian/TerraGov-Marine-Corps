/obj/item/reagent_containers/cup
	name = "open container"
	amount_per_transfer_from_this = 10
	possible_transfer_amounts = list(5, 10, 15, 20, 25, 30, 50)
	volume = 50
	reagent_flags = OPENCONTAINER
	resistance_flags = UNACIDABLE
	icon_state = "bottle"
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/items/drinks_lefthand.dmi',
		slot_r_hand_str = 'icons/mob/inhands/items/drinks_righthand.dmi',
	)

	///Like Edible's food type, what kind of drink is this?
	var/drink_type = NONE
	///The last time we have checked for taste.
	var/last_check_time
	///How much we drink at once, shot glasses drink more.
	var/gulp_size = 5
	///Whether the 'bottle' is made of glass or not so that milk cartons dont shatter when someone gets hit by it.
	var/can_shatter = FALSE

/obj/item/reagent_containers/cup/examine(mob/user)
	. = ..()
	if(drink_type)
		var/list/types = bitfield_to_list(drink_type, FOOD_FLAGS)
		. += span_notice("It is [LOWER_TEXT(english_list(types))].")

/obj/item/reagent_containers/cup/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!check_allowed_items(interacting_with, target_self = TRUE))
		return NONE

	user.changeNext_move(CLICK_CD_RAPID)
	if(ismob(interacting_with))
		return try_consume(interacting_with, user, gulp_size)
	if(interacting_with.is_refillable())
		return try_refill(interacting_with, user, amount_per_transfer_from_this)
	if(interacting_with.is_drainable())
		return try_drain(interacting_with, user, amount_per_transfer_from_this)

	return NONE

/obj/item/reagent_containers/cup/interact_with_atom_secondary(atom/interacting_with, mob/living/user, list/modifiers)
	if(!check_allowed_items(interacting_with, target_self = TRUE))
		return NONE

	user.changeNext_move(CLICK_CD_RAPID)
	if(interacting_with.is_drainable())
		return try_drain(interacting_with, user, amount_per_transfer_from_this)

/obj/item/reagent_containers/cup/item_interaction(mob/living/user, obj/item/tool, list/modifiers)
	if(!istype(tool, /obj/item/reagent_containers/food/snacks/egg))
		return NONE
	if(reagents.holder_full())
		to_chat(user, span_notice("[src] is full."))
		return ITEM_INTERACT_BLOCKING

	to_chat(user, span_notice("You break [tool] in [src]."))
	tool.reagents.trans_to(src, tool.reagents.total_volume)
	qdel(tool)
	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_containers/cup/afterattack(atom/target, mob/user, has_proximity, click_parameters)
	. = ..()
	if(user.a_intent == INTENT_HARM)
		try_splash(target, user)

/// Callback for [datum/component/takes_reagent_appearance] to inherent style footypes
/obj/item/reagent_containers/cup/proc/on_cup_change(datum/glass_style/has_foodtype/style)
	if(!istype(style))
		return
	drink_type = style.drink_type

/// Callback for [datum/component/takes_reagent_appearance] to reset to no foodtypes
/obj/item/reagent_containers/cup/proc/on_cup_reset()
	drink_type = NONE

//Coffeepots: for reference, a standard cup is 30u, to allow 20u for sugar/sweetener/milk/creamer
/obj/item/reagent_containers/cup/coffeepot
	name = "coffeepot"
	desc = "A large pot for dispensing that ambrosia of corporate life known to mortals only as coffee. Contains 4 standard cups."
	volume = 120
	icon_state = "coffeepot"
	fill_icon_state = "coffeepot"
	fill_icon_thresholds = list(0, 1, 30, 60, 100)

/obj/item/reagent_containers/cup/coffeepot/bluespace
	name = "bluespace coffeepot"
	desc = "The most advanced coffeepot the eggheads could cook up: sleek design; graduated lines; connection to a pocket dimension for coffee containment; yep, it's got it all. Contains 8 standard cups."
	volume = 240
	icon_state = "coffeepot_bluespace"
	fill_icon_thresholds = list(0)
