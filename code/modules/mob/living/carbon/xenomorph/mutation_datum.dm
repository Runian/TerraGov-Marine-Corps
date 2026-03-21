/datum/mutation_datum
	interaction_flags = INTERACT_UI_INTERACT
	/// A list of disk colors that have been fully printed.
	var/list/completed_disk_colors = list()
	/// List of all mutation upgrades.
	var/list/datum/mutation_upgrade/all_mutation_upgrades = list()

/datum/mutation_datum/proc/initialize_all_mutation_upgrades()
	if(length(all_mutation_upgrades))
		return
	for(var/mutation_upgrade_type AS in subtypesof(/datum/mutation_upgrade))
		var/datum/mutation_upgrade/mutation = new mutation_upgrade_type()
		// Only initializing mutations that have a name and are available for at least one caste.
		if(mutation.name && length(mutation.allowed_caste_names) > 0)
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
	. = ..()

	var/mob/living/carbon/xenomorph/xenomorph_user = user
	.["disks_completed"] = HAS_TRAIT(xenomorph_user, TRAIT_VALHALLA_XENO) ? 3 : length(completed_disk_colors)

/datum/mutation_datum/ui_static_data(mob/user)
	. = ..()
	var/mob/living/carbon/xenomorph/xenomorph_user = user
	.["shell_mutations"] = list()
	.["spur_mutations"] = list()
	.["veil_mutations"] = list()
	for(var/datum/mutation_upgrade/mutation AS in all_mutation_upgrades)
		if(!can_choose_mutation(xenomorph_user, mutation))
			continue
		var/list_name = "veil_mutations"
		if(mutation.category == MUTATION_SHELL)
			list_name = "shell_mutations"
		else if(mutation.category == MUTATION_SPUR)
			list_name = "spur_mutations"
		.[list_name] += list(list(
			"name" = mutation.name,
			"type" = mutation.type,
			"desc" = mutation.desc,
			"owned" = has_mutation_by_typepath(xenomorph_user, mutation.type)
		))
	.["points_available"] = HAS_TRAIT(xenomorph_user, TRAIT_VALHALLA_XENO) ? MUTATION_MAXIMUM_POINTS : GLOB.hive_datums[xenomorph_user.hivenumber].mutation_points
	.["points_used"] = length(xenomorph_user.owned_mutations)
	.["points_maximum"] = MUTATION_MAXIMUM_POINTS

/datum/mutation_datum/ui_act(action, params)
	. = ..()
	if(.)
		return
	if(!isxeno(usr))
		return

	switch(action)
		if("purchase")
			try_purchase_mutation(usr, text2path(params["upgrade_type"]))

	SStgui.close_user_uis(usr, src)

/// Returns the mutation from the global list if it can be found with its typepath.
/datum/mutation_datum/proc/find_mutation_by_typepath(datum/mutation_upgrade/mutation_typepath)
	for(var/datum/mutation_upgrade/mutation AS in all_mutation_upgrades)
		if(istype(mutation, mutation_typepath))
			return mutation

/// Returns TRUE if the xenomorph's caste has the option to choose this mutation.
/datum/mutation_datum/proc/can_choose_mutation(mob/living/carbon/xenomorph/xenomorph_target, datum/mutation_upgrade/mutation_target)
	return xenomorph_target.xeno_caste.caste_name in mutation_target.allowed_caste_names

/// Returns TRUE if the xenomorph already has a mutation by its typepath.
/datum/mutation_datum/proc/has_mutation_by_typepath(mob/living/carbon/xenomorph/xenomorph_target, datum/mutation_upgrade/mutation_typepath)
	if(!length(xenomorph_target.owned_mutations))
		return FALSE
	for(var/datum/mutation_upgrade/owned_mutation AS in xenomorph_target.owned_mutations)
		if(!istype(owned_mutation, mutation_typepath))
			continue
		return TRUE
	return FALSE

/// Tries to purchase a mutation based on its typepath. Returns TRUE if the mutation was successfully purchased.
/datum/mutation_datum/proc/try_purchase_mutation(mob/living/carbon/xenomorph/xenomorph_purchaser, datum/mutation_upgrade/mutation_typepath)
	if(!xenomorph_purchaser.hive || !mutation_typepath)
		return FALSE
	if(!(SSticker.mode?.round_type_flags & MODE_MUTATIONS_OBTAINABLE) && !HAS_TRAIT(xenomorph_purchaser, TRAIT_VALHALLA_XENO))
		return FALSE
	if(!(xenomorph_purchaser.xeno_caste.caste_flags & CASTE_MUTATIONS_ALLOWED))
		return FALSE
	var/datum/mutation_upgrade/mutation = find_mutation_by_typepath(mutation_typepath)
	if(!mutation)
		to_chat(xenomorph_purchaser, span_warning("That is not a valid mutation."))
		return FALSE
	if(!can_choose_mutation(xenomorph_purchaser, mutation))
		to_chat(xenomorph_purchaser, span_warning("Your caste cannot get this mutation."))
		return FALSE
	if(xenomorph_purchaser.fortify)
		to_chat(xenomorph_purchaser, span_warning("You cannot buy mutations while fortified!"))
		return FALSE
	if(length(xenomorph_purchaser.owned_mutations) > MUTATION_MAXIMUM_POINTS || (!HAS_TRAIT(xenomorph_purchaser, TRAIT_VALHALLA_XENO) && (length(xenomorph_purchaser.owned_mutations) >= length(completed_disk_colors))))
		to_chat(xenomorph_purchaser, span_warning("The hive hasn't developed enough to get another mutation..."))
		return FALSE
	if(has_mutation_by_typepath(xenomorph_purchaser, mutation_typepath))
		to_chat(xenomorph_purchaser, span_warning("You already own this mutation!"))
		return FALSE
	for(var/datum/mutation_upgrade/owned_mutation AS in xenomorph_purchaser.owned_mutations)
		if(!(mutation_typepath in owned_mutation.conflicting_mutation_types))
			continue
		to_chat(xenomorph_purchaser, span_warning("That mutation is not compatible with the mutation: [owned_mutation.name]"))
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

