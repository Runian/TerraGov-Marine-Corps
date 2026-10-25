SUBSYSTEM_DEF(techtree)
	name = "Tech Tree"
	flags = SS_NO_FIRE
	init_stage = INITSTAGE_LAST // Must load after SSmapping because we do some stupid z-level crap.

	/// List of every tech that exists.
	var/list/datum/tech/techs = list()
	/// List of every tech tree that exists.
	var/list/datum/tech_tree/techtrees = list()

/datum/controller/subsystem/techtree/Initialize()
	var/list/techtree_subtypes = subtypesof(/datum/tech_tree)
	if(!length(techtree_subtypes))
		log_admin("Error setting up tech trees; no subtypes found!")
		return SS_INIT_FAILURE

	var/list/tech_subtypes = subtypesof(/datum/tech)
	if(!length(tech_subtypes))
		log_admin("Error setting up tech nodes; no subtypes found!")
		return SS_INIT_FAILURE

	for(var/datum/tech_tree/techtree_typepath AS in techtree_subtypes)
		var/datum/tech_tree/techtree = new techtree_typepath()

		techtrees += list("[techtree.name]" = techtree)
		techtree.zlevel = SSmapping.add_new_zlevel(techtree.name, list(ZTRAIT_TECHTREE))
		for(var/datum/tech_tier/techtier_typepath AS in techtree.all_tiers)
			techtree.all_techs += techtier_typepath
			techtree.all_techs[techtier_typepath] = list()

		for(var/datum/tech/tech_typepath AS in tech_subtypes)
			if(!(tech_typepath::required_tier in techtree.all_techs) || (tech_typepath::flags == NONE))
				continue
			var/datum/tech/tech = new tech_typepath()
			var/datum/tech_tier/techtier_typepath = tech.required_tier

			techtree.all_techs[techtier_typepath] += list(tech.type = tech)
			techs += tech
			tech.on_tree_insertion(techtree)

		techtree.generate_tree()

	return SS_INIT_SUCCESS
