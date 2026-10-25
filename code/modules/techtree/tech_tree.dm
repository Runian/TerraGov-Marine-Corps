/datum/tech_tree
	/// The name that we display
	var/name = TECH_TREE_NONE
	/// The background icon state of the tech.
	var/background_icon = "background"
	/// The amount of stored points we have.
	var/points = 0

	/// The z-level where the techtree is physically stored at.
	/// This is set during within SStechtree.
	var/datum/space_level/zlevel = null
	/// The turf where anyone "entering" the techtree is moved to.
	var/turf/entrance = null

	/// List of all tech tiers (typepaths) that we can achieve.
	/// Order determines where each tier visually appears is in the tech tree (first = bottom, last = top).
	var/list/datum/tech_tier/all_tiers = TECHTIER_GAMEPLAY
	/// Associative list of every tech that we can purchase by tech tier. Associative list [/datum/tech_tier] = list(/datum/tech)
	var/list/all_techs = list()

	/// The tech tier that we are currently on.
	var/datum/tech_tier/current_tier = /datum/tech_tier/zero

/datum/tech_tree/proc/generate_tree()
	if(!zlevel)
		return

	// Got to figure out how wide to make the techtree.
	var/highest_tech_count = 0
	for(var/datum/tech_tier/techtier AS in all_techs)
		var/tech_count = length(all_techs[techtier])
		if(highest_tech_count >= tech_count)
			continue
		highest_tech_count = tech_count

	// Clear out and create the area.
	// (The `+ 2` on both of these is 1 for a buffer tile, and 1 for the outer `/turf/closed/void`.)
	var/area_max_x = highest_tech_count * 2 + 2
	var/area_max_y = length(all_techs) * 3 + 2
	for(var/turf/pos AS in block(1, 1, zlevel.z_value, area_max_x, area_max_y, zlevel.z_value))
		for(var/A AS in pos)
			qdel(A)

		// Since we are at the bottom left of the z-level, we need to box ourselves in.
		if(pos.x == area_max_x || pos.y == area_max_y)
			pos.ChangeTurf(/turf/closed/void)
		else
			pos.ChangeTurf(/turf/open/blank)
			pos.color = "#000000"
		new /area/techtree(pos)

	// Create the tech nodes.
	var/y_offset = 1
	for(var/datum/tech_tier/techtier AS in all_techs)
		var/tech_count = length(all_techs[techtier])
		var/x_offset = (highest_tech_count - tech_count) + 1

		for(var/turf/pos as anything in block(x_offset, y_offset, zlevel.z_value, x_offset + tech_count * 2, y_offset + 2, zlevel.z_value))
			pos.ChangeTurf(/turf/open/blank)
			pos.color = "#000000"

		var/tech_pos = x_offset + 1
		for(var/datum/tech/tech AS in all_techs[techtier])
			new /obj/effect/technode(locate(tech_pos, y_offset + 1, zlevel.z_value), all_techs[techtier][tech])
			tech_pos += 2

		y_offset += 3

	entrance = locate(ceil((highest_tech_count*2 + 1)*0.5), 2, zlevel.z_value)

	// TODO: debug only!! vvvv
	new /mob/living/simple_animal/corgi/ian(entrance) // to find this place via orbit menu.


/datum/tech_tree/proc/enter_mob(mob/living/user, force)
	if(!user.mind || user.stat == DEAD)
		return FALSE

	new /mob/tech_hologram(entrance, user)
	return TRUE

/datum/tech_tree/marine
	name = TECH_TREE_MARINE

/datum/tech_tree/xenomorph
	name = TECH_TREE_XENOMORPH
