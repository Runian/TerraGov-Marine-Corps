// Atom x_act() procs signals. Format:
// When the signal is called: (signal arguments)
// All signals send the source datum of the signal as the first argument

///from base of atom/CheckParts(): (list/parts_list, datum/crafting_recipe/R)
#define COMSIG_ATOM_CHECKPARTS "atom_checkparts"
///from base of atom/CheckParts(): (atom/movable/new_craft) - The atom has just been used in a crafting recipe and has been moved inside new_craft.
#define COMSIG_ATOM_USED_IN_CRAFT "atom_used_in_craft"
/// signal sent when a mouse is hovering over us, sent by atom/proc/on_mouse_entered
#define COMSIG_ATOM_MOUSE_ENTERED "mouse_entered"

/// When this atom gets interacted with by an mob with an active item with a specific tooltype via LEFT CLICK.
/// Sent from [atom/proc/item_interaction()] to the atom: [/mob/user, /obj/item/tool]
#define COMSIG_ATOM_TOOL_ACT(tooltype) "tool_act_[tooltype]"
/// When this atom gets interacted with by an mob with an active item with a specific tooltype via RIGHT CLICK.
/// Sent from [atom/proc/item_interaction()] to the atom: [/mob/user, /obj/item/tool]
#define COMSIG_ATOM_SECONDARY_TOOL_ACT(tooltype) "tool_secondary_act_[tooltype]"
/// When this item with a specific tooltype was successfully used by an mob on an atom via LEFT CLICK.
/// Sent from [atom/proc/tool_act()] to the item: [/atom/target]
#define COMSIG_TOOL_ATOM_ACTED_PRIMARY(tooltype) "tool_atom_acted_[tooltype]"
/// When this item with a specific tooltype was successfully used by an mob on an atom via RIGHT CLICK.
/// Sent from [atom/proc/tool_act()] to the item: [/atom/target]
#define COMSIG_TOOL_ATOM_ACTED_SECONDARY(tooltype) "tool_atom_acted_[tooltype]"
/// When this item with any tooltype successfully used by an mob on an atom.
/// Sent from [atom/proc/tool_act()] to the item: [/atom/target, /mob/user, tool_type, act_result]
#define COMSIG_ITEM_TOOL_ACTED "tool_item_acted"

/// When this atom is interacted with by a mob using an item via LEFT CLICK.
/// Sent from [atom/proc/item_interaction()] to the atom: [/mob/user, /obj/item/interacting_item,, list/modifiers]
#define COMSIG_ATOM_ITEM_INTERACTION "atom_item_interaction"
/// When this atom is interacted with by a mob using an item via RIGHT CLICK.
/// Sent from [atom/proc/item_interaction()] to the atom: [/mob/user, /obj/item/interacting_item, list/modifiers]
#define COMSIG_ATOM_ITEM_INTERACTION_SECONDARY "atom_item_interaction_secondary"
/// When this item is used by a mob to interact with an atom via LEFT CLICK.
/// Sent from [atom/proc/item_interaction()] to the item: [/mob/user, /atom/target, list/modifiers]
#define COMSIG_ITEM_INTERACTING_WITH_ATOM "item_interacting_with_atom"
/// When this item is used by a mob to interact with an atom via RIGHT CLICK.
/// Sent from [atom/proc/item_interaction()] to the item: [/mob/user, /atom/target, list/modifiers]
#define COMSIG_ITEM_INTERACTING_WITH_ATOM_SECONDARY "item_interacting_with_atom_secondary"
/// When this mob uses an item to interacts with an atom via LEFT CLICK.
/// Sent from [atom/proc/item_interaction()] to the user: [/atom/target, /obj/item/interacting_item, list/modifiers]
#define COMSIG_USER_ITEM_INTERACTION "user_item_interaction"
/// When this mob uses an item to interacts with an atom via RIGHT CLICK.
/// Sent from [atom/proc/item_interaction()] to the user: [/atom/target, /obj/item/interacting_item, list/modifiers]
#define COMSIG_USER_ITEM_INTERACTION_SECONDARY "user_item_interaction_secondary"

/// When this atom is interacted with by a non-adjacent mob using an item via LEFT CLICK.
/// Sent from [atom/proc/ranged_item_interaction()] to the atom: [/mob/user, /obj/item/interacting_item, list/modifiers]
#define COMSIG_ATOM_RANGED_ITEM_INTERACTION "atom_ranged_item_interaction"
/// When this atom is interacted with by a non-adjacent mob using an item via RIGHT CLICK.
/// Sent from [atom/proc/ranged_item_interaction()] to the atom: [/mob/user, /obj/item/interacting_item, list/modifiers]
#define COMSIG_ATOM_RANGED_ITEM_INTERACTION_SECONDARY "atom_ranged_item_interaction_secondary"
/// When this item is used by a mob on a non-adjacent atom via LEFT CLICK.
/// Sent from [atom/proc/ranged_item_interaction]: [/mob/user, /atom/target, list/modifiers]
#define COMSIG_RANGED_ITEM_INTERACTING_WITH_ATOM "ranged_item_interacting_with_atom"
/// When this item is used by a mob on a non-adjacent atom via RIGHT CLICK.
/// Sent from [atom/proc/ranged_item_interaction]: [/mob/user, /atom/target, list/modifiers]
#define COMSIG_RANGED_ITEM_INTERACTING_WITH_ATOM_SECONDARY "ranged_item_interacting_with_atom_secondary"

/// Occurs when an atom gets interacted with by any [/obj/item/powerloader_clamp].
/// From [/obj/item/powerloader_clamp/interact_with_atom]: (mob/living/user, obj/item/powerloader_clamp/clamp))
#define COMSIG_ATOM_POWERLOADER_ACT "atom_powerloader_act"
	/// Stops the rest of the act.
	#define COMPONENT_NO_POWERLOADER_ACT (1 << 0)

