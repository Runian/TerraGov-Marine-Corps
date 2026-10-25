/datum/tech
	/// The name of the tech.
	var/name = "Unnamed Tech"
	/// The description of the tech.
	var/desc = "Placeholder description."
	/// The icon of the tech.
	var/icon = 'icons/effects/techtree/tech.dmi'
	/// The icon state of the tech.
	var/icon_state = "unknown"
	/// The background icon state for the primary (inner) icon.
	var/background_icon_primary = "background"
	/// The background icon state for the secondary (outer) icon.
	var/background_icon_secondary = "marine"
	/**
	 * The bitflags of the tech. for certain behavior.
	 *
	 * NONE will cause it to not appear as a tech.
	 */
	var/flags = NONE
	/// Has this been purchased at least once?
	var/purchased = FALSE
	/// The amount of points it costs to purchase this tech.
	var/purchase_cost = 0

	/// The tier value needed to buy this tech.
	var/datum/tech_tier/required_tier = /datum/tech_tier/zero
	/// The tech tree that this belongs to. This does not
	var/datum/tech_tree/tech_tree
	/// The physical representation within the tech tree.
	var/obj/effect/technode/node

/// Can we purchase this tech?
/datum/tech/proc/can_purchase()
	SHOULD_CALL_PARENT(TRUE)

	// Needs to meet the tier requirement!
	if(tech_tree.current_tier.value < required_tier.value)
		return FALSE

	// Needs to have enough points!
	if(tech_tree.points < purchase_cost)
		return FALSE

	return TRUE

/// The effects that happen upon purchasing this tech.
/datum/tech/proc/on_purchase()
	SHOULD_CALL_PARENT(TRUE)

	purchased = TRUE
	tech_tree.points -= purchase_cost

/datum/tech/proc/on_tree_insertion(datum/tech_tree/new_tech_tree)
	tech_tree = new_tech_tree
	background_icon_primary = new_tech_tree.background_icon

/datum/tech/proc/update_icon(obj/effect/technode)
	technode.overlays.Cut()
	if(!icon_state)
		technode.icon = 'icons/effects/techtree/tech.dmi'
		technode.icon_state = null
		return

	technode.icon = 'icons/effects/techtree/tech.dmi'
	technode.icon_state = "[background_icon_primary]"

	technode.overlays += get_tier_overlay()
	technode.overlays += image(icon, technode, icon_state)

	var/background_icons = 'icons/effects/techtree/tech_64.dmi'
	technode.overlays += image(background_icons, technode, "background[purchased ? "" : "_locked"]", pixel_x = -16, pixel_y = -16)
	technode.overlays += image(background_icons, technode, "[background_icon_secondary][purchased ? "" : "_locked"]", pixel_x = -16, pixel_y = -16)


/datum/tech/proc/get_tier_overlay()
	var/background_icons = 'icons/effects/techtree/tech_64.dmi'
	var/image/I = image(background_icons, node, "+lines", pixel_x = -16, pixel_y = -16)
	I.color = required_tier.color

	return I
