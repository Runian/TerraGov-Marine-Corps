////////////////////////////////////////////////////////////////////////////////
/// Droppers.
////////////////////////////////////////////////////////////////////////////////
/obj/item/reagent_containers/dropper
	name = "dropper"
	desc = "A dropper. Transfers 5 units."
	icon = 'icons/obj/items/chemistry.dmi'
	icon_state = "dropper0"
	amount_per_transfer_from_this = 5
	possible_transfer_amounts = list(1,2,3,4,5)
	w_class = WEIGHT_CLASS_TINY
	volume = 5
	reagent_flags = TRANSPARENT
	var/filled = FALSE

/obj/item/reagent_containers/dropper/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!interacting_with.reagents)
		return NONE

	var/amount_transferred = 0
	if(!filled)
		if(!interacting_with.is_open_container() && !istype(interacting_with, /obj/structure/reagent_dispensers))
			balloon_alert(user, "can't remove reagents!")
			return ITEM_INTERACT_BLOCKING
		if(!interacting_with.reagents.total_volume)
			balloon_alert(user, "empty!")
			return ITEM_INTERACT_BLOCKING

		amount_transferred = interacting_with.reagents.trans_to(src, amount_per_transfer_from_this)
		if(amount_transferred)
			balloon_alert(user, "filled [amount_transferred] units!")
		filled = TRUE
		icon_state = "dropper[filled]"
		return ITEM_INTERACT_SUCCESS

	if(interacting_with.reagents.total_volume >= interacting_with.reagents.maximum_volume)
		balloon_alert(user, "container full!")
		return ITEM_INTERACT_BLOCKING

	if(!interacting_with.is_injectable() && !ismob(interacting_with)) // You can inject humans and food, but you can't remove the shit.
		balloon_alert(user, "cannot transfer!")
		return ITEM_INTERACT_BLOCKING

	if(ismob(interacting_with))
		visible_message(span_danger("[user] is trying to squirt something into [interacting_with]'s eyes!"))

		var/time = 2 SECONDS // 2/3rds the time of a syringe.
		if(!do_after(user, time, NONE, interacting_with, BUSY_ICON_HOSTILE))
			return ITEM_INTERACT_BLOCKING

		if(ishuman(interacting_with))
			var/mob/living/carbon/human/interacting_human = interacting_with
			var/obj/item/safe_thing = null
			if(interacting_human.wear_mask?.inventory_flags & COVEREYES)
				safe_thing = interacting_human.wear_mask
			if(!safe_thing && interacting_human.head?.inventory_flags & COVEREYES)
				safe_thing = interacting_human.head
			if(!safe_thing && interacting_human.glasses?.inventory_flags & COVEREYES)
				safe_thing = interacting_human.glasses

			if(safe_thing)
				if(!safe_thing.reagents)
					safe_thing.create_reagents(100)
				amount_transferred = reagents.trans_to(safe_thing, amount_per_transfer_from_this)
				visible_message(span_danger("[user] tries to squirt something into [interacting_with]'s eyes, but fails!"))
				addtimer(CALLBACK(reagents, TYPE_PROC_REF(/datum/reagents, reaction), safe_thing, TOUCH), 5)

				if(amount_transferred)
					balloon_alert(user, "transferred [amount_transferred] units!")
				if(!reagents.total_volume)
					filled = FALSE
					icon_state = "dropper[filled]"
				return ITEM_INTERACT_SUCCESS

		visible_message(span_danger("[user] squirts something into [interacting_with]'s eyes!"))
		reagents.reaction(interacting_with, TOUCH)

		log_combat(user, interacting_with, "squirted", src, "Reagents: [get_reagent_list_text()]")
		record_reagent_consumption(min(amount_per_transfer_from_this, reagents.total_volume), reagents.reagent_list, user, interacting_with)

	amount_transferred = reagents.trans_to(interacting_with, amount_per_transfer_from_this)
	if(amount_transferred)
		balloon_alert(user, "transferred [amount_transferred] units!")
	if(!reagents.total_volume)
		filled = FALSE
		icon_state = "dropper[filled]"
	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_containers/dropper/afterattack(obj/target, mob/user , flag)
	if(target.reagents.total_volume >= target.reagents.maximum_volume)
		balloon_alert(user, "Can't, full")
		return

	if(!target.is_injectable() && !ismob(target)) //You can inject humans and food but you cant remove the shit.
		balloon_alert(user, "Cannot fill object")
		return

	var/trans = 0

	if(ismob(target))
		visible_message(span_danger("[user] is trying to squirt something into [target]'s eyes!"))

		var/time = 2 SECONDS // 2/3rds the time of a syringe.
		if(!do_after(user, time, NONE, target, BUSY_ICON_HOSTILE))
			return

		if(ishuman(target))
			var/mob/living/carbon/human/victim = target

			var/obj/item/safe_thing = null
			if( victim.wear_mask )
				if ( victim.wear_mask.inventory_flags & COVEREYES )
					safe_thing = victim.wear_mask
			if( victim.head )
				if ( victim.head.inventory_flags & COVEREYES )
					safe_thing = victim.head
			if(victim.glasses)
				if ( !safe_thing )
					safe_thing = victim.glasses

			if(safe_thing)
				if(!safe_thing.reagents)
					safe_thing.create_reagents(100)
				trans = src.reagents.trans_to(safe_thing, amount_per_transfer_from_this)

				visible_message(span_danger("[user] tries to squirt something into [target]s eyes, but fails!"))
				addtimer(CALLBACK(reagents, TYPE_PROC_REF(/datum/reagents, reaction), safe_thing, TOUCH), 5)

				balloon_alert(user, "transfers [trans] units")
				if (src.reagents.total_volume<=0)
					filled = 0
					icon_state = "dropper[filled]"
				return

		visible_message(span_danger("[user] squirts something into [target]'s eyes!"))
		src.reagents.reaction(target, TOUCH)

		var/mob/living/M = target

		var/list/injected = list()
		for(var/datum/reagent/R in src.reagents.reagent_list)
			injected += R.name
		var/contained = english_list(injected)
		log_combat(user, M, "squirted", src, "Reagents: [contained]")
		record_reagent_consumption(min(amount_per_transfer_from_this, reagents.total_volume), reagents.reagent_list, user, M)

	trans = src.reagents.trans_to(target, amount_per_transfer_from_this)
	balloon_alert(user, "transfers [trans] units")
	if (src.reagents.total_volume<=0)
		filled = 0
		icon_state = "dropper[filled]"

////////////////////////////////////////////////////////////////////////////////
/// Droppers. END
////////////////////////////////////////////////////////////////////////////////
