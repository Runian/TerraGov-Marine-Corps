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
/// When this item with any tooltype successfully used by an mob on an atom via RIGHT CLICK.
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
	/// This cancels the rest of the chain entirely and indicates success.
	#define ITEM_INTERACT_SUCCESS (1<<0) // Same as TRUE, as most tool (legacy) tool acts return TRUE on success
	/// Return to prevent the rest of the attack chain from being executed / preventing the item user from thwacking the target.
	/// Similar to [ITEM_INTERACT_SUCCESS], but does not necessarily indicate success.
	#define ITEM_INTERACT_BLOCKING (1<<1)
	/// Return to skip the rest of the interaction chain, going straight to attack.
	#define ITEM_INTERACT_SKIP_TO_ATTACK (1<<2)
	/// Combination flag for any item interaction that blocks the rest of the attack chain
	#define ITEM_INTERACT_ANY_BLOCKER (ITEM_INTERACT_SUCCESS | ITEM_INTERACT_BLOCKING)


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


/// When an item used on itself by a mob via LEFT CLICK.
/// Sent from [obj/item/proc/attack_self()] to the item: (/mob/user)
#define COMSIG_ITEM_ATTACK_SELF "item_attack_self"
	/// Ends the entire attack chain.
	#define COMPONENT_CANCEL_ATTACK_CHAIN (1<<0)
	/// Skips this specific attack step, but continues with the rest of the attack chain.
	#define COMPONENT_CONTINUE_ATTACK_CHAIN (1<<1)
/// When an item used on itself by a mob via RIGHT CLICK.
/// Sent from [obj/item/proc/attack_self_secondary()] to the item: (/mob/user)
#define COMSIG_ITEM_ATTACK_SELF_SECONDARY "item_attack_self_secondary"
	/// Ends the entire attack chain.
	#define COMPONENT_SECONDARY_CANCEL_ATTACK_CHAIN (1<<0)
	/// Skips calling the primary pre-attack step, but continues with the rest of the attack chain.
	#define COMPONENT_SECONDARY_CONTINUE_ATTACK_CHAIN (1<<1)
	/// Calls the primary pre-attack step and contintues with the rest of the attack chain.
	#define COMPONENT_SECONDARY_CALL_NORMAL_ATTACK_CHAIN (1<<2)

/// When an item is used to attack an atom by a mob via LEFT CLICK.
/// Sent from [obj/item/proc/pre_attack()] to the item: (atom/target, mob/user, list/modifiers)
#define COMSIG_ITEM_PRE_ATTACK "item_pre_attack"
/// When an user attacks an atom with an item via LEFT CLICK.
/// Sent from [obj/item/proc/pre_attack()] to the item: (atom/item/weapon, mob/user, list/modifiers)
#define COMSIG_USER_PRE_ITEM_ATTACK "user_pre_item_attack"
/// When an item is used to attack an atom by a mob via RIGHT CLICK.
/// Sent from [obj/item/proc/pre_attack_secondary()] to the item: (atom/target, mob/user, list/modifiers)
#define COMSIG_ITEM_PRE_ATTACK_SECONDARY "item_pre_attack_secondary"
/// When an user attacks an atom with an item via RIGHT CLICK.
/// Sent from [obj/item/proc/pre_attack()] to the item: (atom/item/weapon, mob/user, list/modifiers)
#define COMSIG_USER_PRE_ITEM_ATTACK_SECONDARY "user_pre_item_attack_secondary"

/// When this atom is attacked by a mob with an item via LEFT CLICK.
// Sent from [/atom/proc/attackby(): (/obj/item/weapon, /mob/user,  list/modifiers)
#define COMSIG_ATOM_ATTACKBY "atom_attackby"
/// When this atom is attacked by a mob with an item via RIGHT CLICK.
/// From base of [atom/proc/attacby_secondary()] (/obj/item/weapon, /mob/user,  list/modifiers)
#define COMSIG_ATOM_ATTACKBY_SECONDARY "atom_attackby_secondary"
	/// Return this in response if you don't want afterattack to be called.
	#define COMPONENT_NO_AFTERATTACK (1<<0)

/// After this atom is attacked by a mob with an item, but before [/obj/item/proc/afterattack()] is called.
/// Sent from [/obj/item/proc/attack_atom()] to the atom: (/obj/item/weapon, /mob/user, list/modifiers)
#define COMSIG_ATOM_AFTER_ATTACKEDBY "atom_after_attackby"

/// When this item is being used to attack a living mob via LEFT CLICK.
/// Sent from [/obj/item/proc/attack()] to the item: (/mob/living/target_mob, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK "item_attack"
/// When this item is being used to attack a living mob via RIGHT CLICK.
/// Sent from [/obj/item/proc/attack()] to the item: (/mob/living/target_mob, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK_SECONDARY "item_attack_secondary"
/// When this item is being used to attack an atom.
/// Sent from [/obj/item/proc/attack_atom()] to the item: (/atom/target_atom, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK_ATOM "item_attack_atom"




#define COMSIG_ATOM_ATTACK_HAND "atom_attack_hand"				//from base of atom/attack_hand(mob/living/user)
#define COMSIG_ATOM_ATTACK_HAND_ALTERNATE "atom_attack_hand_alternate"	//from base of /atom/attack_hand_alternate(mob/living/user)
#define COMSIG_ATOM_ATTACK_GHOST "atom_attack_ghost"			//from base of atom/attack_ghost(): (mob/dead/observer/ghost)
	#define COMPONENT_NO_ATTACK_HAND (1<<0)						//works on all attack_hands.
#define COMSIG_ATOM_ATTACK_POWERLOADER "atom_attack_powerloader"//from base of atom/attack_powerloader: (mob/living/user, obj/item/powerloader_clamp/attached_clamp)

// TODO: These non-signals need to go into a different folder.

// Non-signal returns for any RIGHT CLICK/secondary attack chains.
// Return these at the end of any attack chain proc, e.g. [/obj/item/proc/pre_attack_secondary].
	/// Ends the rest of attack chain.
	#define SECONDARY_ATTACK_CANCEL_ATTACK_CHAIN 2
	/// Skips calling the primary attack step and continues with the rest of the attack chain.
	#define SECONDARY_ATTACK_CONTINUE_CHAIN 1
	/// Calls the primary attack step and continues with the rest of the attack chain.
	#define SECONDARY_ATTACK_CALL_NORMAL 3
