/**
 * Handles any interactions of a tool on this atom before anything else.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
*/
/atom/proc/base_item_interaction(mob/user, obj/item/tool, list/modifiers)
	SHOULD_CALL_PARENT(TRUE)

	var/is_right_clicking = text2num(LAZYACCESS(modifiers, RIGHT_CLICK))
	var/is_left_clicking = !is_right_clicking
	var/early_sig_return
	if(is_left_clicking)
		/*
		 * This is intentionally using `||` instead of `|` to short-circuit the signal calls.
		 * This is because we want to return early if ANY of these signals return a value.
		 *
		 * This puts priority on the atom's signals, then the tool's signals, then the user's signals,
		 * so we can avoid doing two interactions at once.
		 */
		early_sig_return = SEND_SIGNAL(src, COMSIG_ATOM_ITEM_INTERACTION, user, tool, modifiers) \
			|| SEND_SIGNAL(tool, COMSIG_ITEM_INTERACTING_WITH_ATOM, user, src, modifiers) \
			|| SEND_SIGNAL(user, COMSIG_USER_ITEM_INTERACTION, src, tool, modifiers)
	else
		// See above
		early_sig_return = SEND_SIGNAL(src, COMSIG_ATOM_ITEM_INTERACTION_SECONDARY, user, tool, modifiers) \
			|| SEND_SIGNAL(tool, COMSIG_ITEM_INTERACTING_WITH_ATOM_SECONDARY, user, src, modifiers) \
			|| SEND_SIGNAL(user, COMSIG_USER_ITEM_INTERACTION_SECONDARY, src, tool, modifiers)
	if(early_sig_return)
		return early_sig_return

	var/self_interaction = is_left_clicking \
		? item_interaction(user, tool, modifiers) \
		: item_interaction_secondary(user, tool, modifiers)
	if(self_interaction)
		return self_interaction

	var/interact_return = is_left_clicking \
		? tool.interact_with_atom(src, user, modifiers) \
		: tool.interact_with_atom_secondary(src, user, modifiers)
	if(interact_return)
		return interact_return

	return NONE


/**
 * Called when this atom has an item used on it.
 * i.e., a mob is clicking on this atom with an item.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */
/atom/proc/item_interaction(mob/user, obj/item/tool, list/modifiers)
	return NONE

/**
 * Called when this atom has an item used on it WITH RIGHT CLICK,
 * i.e., a mob is right clicking on this atom with an item.
 *
 * Default behavior has it run the same code as left click.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */
/atom/proc/item_interaction_secondary(mob/user, obj/item/tool, list/modifiers)
	return item_interaction(user, tool, modifiers)

/**
 * Called when this item is being used to interact with an atom,
 * i.e., a mob is clicking on an atom with this item.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */
/obj/item/proc/interact_with_atom(atom/target, mob/user, list/modifiers)
	return

/**
 * Called when this item is being used to interact with an atom WITH RIGHT CLICK,
 * IE, a mob is right clicking on an atom with this item.
 *
 * Default behavior has it run the same code as left click.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */
/obj/item/proc/interact_with_atom_secondary(atom/target, mob/user, list/modifiers)
	return interact_with_atom(target, user, modifiers)

/**
 * Handles the usage of items with tool behaviors on this atom directly.
 * Handles the tool_acts in particular, such as wrenches and screwdrivers.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */
/atom/proc/tool_act(mob/user, obj/item/tool, list/modifiers)
	SHOULD_CALL_PARENT(TRUE)
	PROTECTED_PROC(TRUE)

	var/tool_type = tool.tool_behaviour
	if(!tool_type)
		return

	var/is_right_clicking = LAZYACCESS(modifiers, RIGHT_CLICK)
	var/is_left_clicking = !is_right_clicking

	var/signal_result = is_left_clicking \
		? SEND_SIGNAL(src, COMSIG_ATOM_TOOL_ACT(tool_type), user, tool) \
		: SEND_SIGNAL(src, COMSIG_ATOM_SECONDARY_TOOL_ACT(tool_type), user, tool)
	if(signal_result)
		return signal_result
	if(QDELETED(tool))
		return TRUE // Safe-ish to assume that if we deleted our item something succeeded.

	var/act_result
	switch(tool_type)
		if(TOOL_CROWBAR)
			act_result = is_left_clicking ? crowbar_act(user, tool) : crowbar_act_secondary(user, tool)
		if(TOOL_MULTITOOL)
			act_result = is_left_clicking ? multitool_act(user, tool) : multitool_act_secondary(user, tool)
		if(TOOL_SCREWDRIVER)
			act_result = is_left_clicking ? screwdriver_act(user, tool) : screwdriver_act_secondary(user, tool)
		if(TOOL_WRENCH)
			act_result = is_left_clicking ? wrench_act(user, tool) : wrench_act_secondary(user, tool)
		if(TOOL_WIRECUTTER)
			act_result = is_left_clicking ? wirecutter_act(user, tool) : wirecutter_act_secondary(user, tool)
		if(TOOL_WELDER)
			act_result = is_left_clicking ? welder_act(user, tool) : welder_act_secondary(user, tool)
		if(TOOL_ANALYZER)
			act_result = is_left_clicking ? analyzer_act(user, tool) : analyzer_act_secondary(user, tool)
		if(TOOL_FULTON)
			act_result = is_left_clicking ? fulton_act(user, tool) : fulton_act_secondary(user, tool)
		if(TOOL_PLASMACUTTER)
			act_result = is_left_clicking ? plasmacutter_act(user, tool) : plasmacutter_act_secondary(user, tool)

	if(!act_result)
		return

	// A tooltype_act has completed successfully.
	if(is_left_clicking)
		SEND_SIGNAL(tool, COMSIG_TOOL_ATOM_ACTED_PRIMARY(tool_type), src)
	else
		SEND_SIGNAL(tool, COMSIG_TOOL_ATOM_ACTED_SECONDARY(tool_type), src)
	SEND_SIGNAL(tool, COMSIG_ITEM_TOOL_ACTED, src, user, tool_type)
	return act_result

/**
 * Tool-specific behavior procs.
 *
 * Return TRUE to cancel the attack.
 * Return FALSE (or null) to continue the attack.
 */

/// Called on an object when a tool with crowbar capabilities is used to LEFT CLICK an atom.
/atom/proc/crowbar_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with crowbar capabilities is used to RIGHT CLICK an atom.
/atom/proc/crowbar_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with multitool capabilities is used to LEFT CLICK an atom.
/atom/proc/multitool_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with multitool capabilities is used to RIGHT CLICK an atom.
/atom/proc/multitool_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with screwdriver capabilities is used to LEFT CLICK an atom.
/atom/proc/screwdriver_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with screwdriver capabilities is used to RIGHT CLICK an atom.
/atom/proc/screwdriver_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with wrench capabilities is used to LEFT CLICK an atom.
/atom/proc/wrench_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with wrench capabilities is used to RIGHT CLICK an atom.
/atom/proc/wrench_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with wirecutter capabilities is used to LEFT CLICK an atom.
/atom/proc/wirecutter_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with wirecutter capabilities is used to RIGHT CLICK an atom.
/atom/proc/wirecutter_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with welder capabilities is used to LEFT CLICK an atom.
/atom/proc/welder_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with welder capabilities is used to RIGHT CLICK an atom.
/atom/proc/welder_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with analyzer capabilities is used to LEFT CLICK an atom.
/atom/proc/analyzer_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with analyzer capabilities is used to RIGHT CLICK an atom.
/atom/proc/analyzer_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with fulton capabilities is used to LEFT CLICK an atom.
/atom/proc/fulton_act(mob/user, obj/item/tool)
	if(!isturf(loc))
		return FALSE // Storage screens, worn containers, anything we want to be able to interact otherwise.
	to_chat(user, span_warning("Cannot extract [src]."))
	return TRUE

/// Called on an object when a tool with fulton capabilities is used to RIGHT CLICK an atom.
/atom/proc/fulton_act_secondary(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with plasma cutter capabilities is used to LEFT CLICK an atom.
/atom/proc/plasmacutter_act(mob/user, obj/item/tool)
	return

/// Called on an object when a tool with plasma cutter capabilities is used to RIGHT CLICK an atom.
/atom/proc/plasmacutter_act_secondary(mob/user, obj/item/tool)
	return
