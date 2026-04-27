/datum/mutation_datum
	interaction_flags = INTERACT_UI_INTERACT
	/// A list of disk colors that have been fully printed.
	var/list/completed_disk_colors = list()
	/// List of all mutation upgrades.
	var/list/datum/mutation_upgrade/all_mutation_upgrades = list()

/datum/mutation_datum/proc/initialize_all_mutation_upgrades()
	if(length(all_mutation_upgrades))
		return
	// Only initializing non-base type mutations.
	for(var/mutation_upgrade_type AS in subtypesof(/datum/mutation_upgrade))
		var/datum/mutation_upgrade/mutation = new mutation_upgrade_type()
		if(!mutation.name || !mutation.category)
			continue
		if(!length(mutation.allowed_caste_names))
			continue
		all_mutation_upgrades += mutation

/datum/mutation_datum/New()
	. = ..()
	initialize_all_mutation_upgrades()
	RegisterSignal(SSdcs, COMSIG_GLOB_DISK_GENERATED, PROC_REF(on_disk_printed))

/datum/mutation_datum/ui_state(mob/user)
	return GLOB.hive_ui_state

/datum/mutation_datum/ui_interact(mob/user, datum/tgui/ui)
	ui = SStgui.try_update_ui(user, src, ui)
	if(!ui)
		ui = new(user, src, "MutationSelector", "Mutation Selector")
		ui.open()

/datum/mutation_datum/ui_data(mob/user)
	var/mob/living/carbon/xenomorph/xenomorph_user = user
	var/list/data = list()
	data["mutation_points_available"] = xenomorph_user.hive.mutation_points
	data["mutation_points_maximum"] = max(MUTATION_MAXIMUM_POINTS, data["mutation_points_available"])
	return data

/datum/mutation_datum/ui_static_data(mob/user)
	var/mob/living/carbon/xenomorph/xenomorph_user = user
	var/list/data = list()
	data["mutation_points_used"] = length(xenomorph_user.owned_mutations)
	data["mutations"] = list()
	data["mutation_categories"] = list()
	for(var/datum/mutation_upgrade/mutation AS in all_mutation_upgrades)
		if(!can_choose_mutation(xenomorph_user, mutation))
			continue
		if(!(mutation.category in data["mutation_categories"]))
			data["mutation_categories"] += mutation.category
		data["mutations"] += list(list(
			"category" = mutation.category,
			"name" = mutation.name,
			"description" = mutation.desc,
			"typepath" = mutation.type,
			"owned" = has_mutation_by_typepath(xenomorph_user, mutation.type)
		))
	return data

/datum/mutation_datum/ui_act(action, params)
	. = ..()
	if(.)
		return
	if(!isxeno(usr))
		return
	switch(action)
		if("purchase")
			try_purchase_mutation(usr, text2path(params["mutation_typepath"]))
	SStgui.close_user_uis(usr, src)

/// Returns the mutation from the global list if it can be found with its typepath.
/datum/mutation_datum/proc/find_mutation_by_typepath(datum/mutation_upgrade/mutation_typepath)
	for(var/datum/mutation_upgrade/mutation AS in all_mutation_upgrades)
		if(istype(mutation, mutation_typepath))
			return mutation

/// Returns TRUE if the xenomorph's caste has the option to choose this mutation.
/datum/mutation_datum/proc/can_choose_mutation(mob/living/carbon/xenomorph/xenomorph_target, datum/mutation_upgrade/mutation_target)
	return xenomorph_target.xeno_caste.caste_name in mutation_target.allowed_caste_names

/// Returns TRUE if the xenomorph already has a mutation.
/datum/mutation_datum/proc/has_mutation_by_typepath(mob/living/carbon/xenomorph/xenomorph_target, datum/mutation_upgrade/mutation_typepath)
	if(!length(xenomorph_target.owned_mutations))
		return FALSE
	for(var/datum/mutation_upgrade/owned_mutation AS in xenomorph_target.owned_mutations)
		if(!istype(owned_mutation, mutation_typepath))
			continue
		return TRUE

/// Tries to purchase a mutation based on its typepath. Returns TRUE if the mutation was successfully purchased.
/datum/mutation_datum/proc/try_purchase_mutation(mob/living/carbon/xenomorph/xenomorph_purchaser, datum/mutation_upgrade/mutation_typepath, silent = FALSE)
	if(!xenomorph_purchaser.hive || !mutation_typepath)
		return FALSE
	if(!(SSticker.mode?.round_type_flags & MODE_MUTATIONS_OBTAINABLE))
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("Mutations cannot be obtained."))
		return FALSE
	if(!(xenomorph_purchaser.xeno_caste.caste_flags & CASTE_MUTATIONS_ALLOWED))
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("Your caste is disallowed from getting mutations."))
		return FALSE
	if(xenomorph_purchaser.fortify)
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("You cannot get new mutations while fortified."))
		return FALSE
	var/mutation_points_available = xenomorph_purchaser.hive.mutation_points
	if(length(xenomorph_purchaser.owned_mutations) > mutation_points_available)
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("The hive can only support up to [mutation_points_available] mutations!"))
		return FALSE
	var/datum/mutation_upgrade/mutation = find_mutation_by_typepath(mutation_typepath)
	if(!mutation)
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("That mutation is invalid."))
		return FALSE
	if(has_mutation_by_typepath(xenomorph_purchaser, mutation.type))
		to_chat(xenomorph_purchaser, span_warning("You already have this mutation!"))
		return FALSE
	if(!can_choose_mutation(xenomorph_purchaser, mutation))
		if(!silent)
			to_chat(xenomorph_purchaser, span_warning("That mutation is not available for your caste."))
		return FALSE
	if(!mutation.can_gain(xenomorph_purchaser, silent))
		return FALSE
	to_chat(xenomorph_purchaser, span_xenonotice("Mutation gained."))
	xenomorph_purchaser.do_jitter_animation(500)
	new mutation_typepath(xenomorph_purchaser) // Everything else in handled during the mutation's New().
	return TRUE

/// Called when a disk is printed.
/datum/mutation_datum/proc/on_disk_printed(datum/source, obj/machinery/computer/code_generator/nuke/printing_computer)
	SIGNAL_HANDLER
	var/disk_color = printing_computer.key_color
	if(!disk_color || (disk_color in completed_disk_colors))
		return
	completed_disk_colors += disk_color

/mob/living/carbon/xenomorph/verb/open_mutation_menu()
	set name = "Mutate"
	set desc = "Opens the mutation selector menu."
	set category = "Alien"

	SStgui.close_user_uis(src, GLOB.mutation_selector)
	GLOB.mutation_selector.ui_interact(src)

