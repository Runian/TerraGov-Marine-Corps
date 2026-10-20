/// Sent from [obj/item/proc/attack_self()] to the item: (/mob/user)
#define COMSIG_ITEM_ATTACK_SELF "item_attack_self"
	/// Ends the entire attack chain.
	#define COMPONENT_CANCEL_ATTACK_CHAIN (1<<0)
	/// Skips this specific attack step, but continues with the rest of the attack chain.
	#define COMPONENT_CONTINUE_ATTACK_CHAIN (1<<1)

/// Sent from [obj/item/proc/attack_self_secondary()] to the item: (/mob/user)
#define COMSIG_ITEM_ATTACK_SELF_SECONDARY "item_attack_self_secondary"
	/// Ends the entire attack chain.
	#define COMPONENT_SECONDARY_CANCEL_ATTACK_CHAIN (1<<0)
	/// Skips calling the primary pre-attack step, but continues with the rest of the attack chain.
	#define COMPONENT_SECONDARY_CONTINUE_ATTACK_CHAIN (1<<1)
	/// Calls the primary pre-attack step and contintues with the rest of the attack chain.
	#define COMPONENT_SECONDARY_CALL_NORMAL_ATTACK_CHAIN (1<<2)

/// Sent from [obj/item/proc/pre_attack()] to the item: (atom/target, mob/user, list/modifiers)
#define COMSIG_ITEM_PRE_ATTACK "item_pre_attack"
/// Sent from [obj/item/proc/pre_attack_secondary()] to the item: (atom/target, mob/user, list/modifiers)
#define COMSIG_ITEM_PRE_ATTACK_SECONDARY "item_pre_attack_secondary"

/// Sent from [obj/item/proc/pre_attack()] to the user: (atom/item/weapon, atom/target, list/modifiers)
#define COMSIG_USER_PRE_ITEM_ATTACK "user_pre_item_attack"
/// Sent from [obj/item/proc/pre_attack_secondary()] to the user: (atom/item/weapon, atom/target, list/modifiers)
#define COMSIG_USER_PRE_ITEM_ATTACK_SECONDARY "user_pre_item_attack_secondary"


/// Sent from [/atom/proc/attackby() to the atom: (/obj/item/weapon, /mob/user,  list/modifiers)
#define COMSIG_ATOM_ATTACKBY "atom_attackby"
/// Sent from [atom/proc/attacby_secondary()] to the atom: (/obj/item/weapon, /mob/user,  list/modifiers)
#define COMSIG_ATOM_ATTACKBY_SECONDARY "atom_attackby_secondary"
	/// Return this in response if you don't want afterattack to be called.
	#define COMPONENT_NO_AFTERATTACK (1<<0)
/// Sent from [/obj/item/proc/melee_attack_chain()] to the atom: (/obj/item/weapon, /mob/user, list/modifiers)
#define COMSIG_ATOM_AFTER_ATTACKEDBY "atom_after_attackby"

/// Sent from [/obj/item/proc/attack_mob()] to the item: (/mob/living/target_mob, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK "item_attack"
/// Sent from [/obj/item/proc/attack_secondary()] to the item: (/mob/living/target_mob, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK_SECONDARY "item_attack_secondary"
/// Sent from [/obj/item/proc/attack_atom()] to the item: (/atom/target_atom, /mob/user, list/modifiers)
#define COMSIG_ITEM_ATTACK_ATOM "item_attack_atom"
/// Sent from [/obj/item/proc/melee_attack_chain()] to the item: (/atom/target_atom, /mob/user, list/modifiers)
#define COMSIG_ITEM_AFTERATTACK "item_afterattack"

/// Sent from [/obj/item/proc/attack_mob()] to the user: (/atom/target_atom, /obj/item/weapon, list/modifiers)
#define COMSIG_MOB_ITEM_ATTACK "mob_item_attack"
/// Sent from [/obj/item/proc/attack_atom()] to the user: (/obj/target_obj, /obj/item/weapon, list/modifiers)
#define COMSIG_LIVING_ATTACK_OBJ "living_attack_atom"

/// Sent from [/obj/item/proc/attack_turf()] to the item: (/turf/target_turf, /obj/item/weapon, list/modifiers)
#define COMSIG_ITEM_ATTACK_TURF "item_attack_turf"
/// Sent from [/obj/item/proc/attack_turf()] to the item: (/obj/target_obj, /obj/item/weapon, list/modifiers)
#define COMSIG_ITEM_ATTACK_OBJ "item_attack_obj"

/// Sent from [/atom/proc/attack_hand()] to the atom: (/mob/living/carbon/human/user)
#define COMSIG_ATOM_ATTACK_HAND "atom_attack_hand"
/// Sent from [/atom/proc/attack_hand_secondary()] to the atom: (/mob/living/carbon/human/user)
#define COMSIG_ATOM_ATTACK_HAND_SECONDARY "atom_attack_hand_secondary"
/// Sent from [/atom/proc/attack_ghost()] to the atom: (mob/dead/observer/user)
#define COMSIG_ATOM_ATTACK_GHOST "atom_attack_ghost"
