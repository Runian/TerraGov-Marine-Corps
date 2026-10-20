/obj/item/reagent_scanner
	name = "reagent scanner"
	desc = "A hand-held reagent scanner which identifies chemical agents."
	icon = 'icons/obj/device.dmi'
	icon_state = "spectrometer"
	worn_icon_state = "analyzer"
	w_class = WEIGHT_CLASS_SMALL
	atom_flags = CONDUCT
	equip_slot_flags = ITEM_SLOT_BELT
	throwforce = 5
	throw_speed = 4
	throw_range = 20
	/// Should we show what percentage of each reagent relative to the total amount?
	var/details = FALSE

/obj/item/reagent_scanner/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!user.stat)
		return NONE
	if(!isobj(interacting_with))
		return NONE
	if(!interacting_with.reagents || !length(interacting_with.reagents.reagent_list))
		to_chat(user, span_notice("No chemical agents found in [interacting_with]."))
		return ITEM_INTERACT_SUCCESS
	var/dat = ""
	var/one_percent = interacting_with.reagents.total_volume / 100
	for(var/datum/reagent/found_reagent in interacting_with.reagents.reagent_list)
		dat += "\n \t [span_notice(" [found_reagent.name][details ? ": [found_reagent.volume / one_percent]%" : ""]")]"
	to_chat(user, span_notice("Chemicals found: [dat]"))
	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_scanner/adv
	name = "advanced reagent scanner"
	icon_state = "adv_spectrometer"
	details = TRUE
