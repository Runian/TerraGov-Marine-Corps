SUBSYSTEM_DEF(advanced_pathfinding)
	name = "Advanced Pathfinding"
	priority = FIRE_PRIORITY_ADVANCED_PATHFINDING
	wait = 1 SECONDS
	///List of ai_behaviour datum asking for a tile pathfinding
	var/list/datum/ai_behavior/tile_pathfinding_to_do = list()
	///List of ai_behaviour datum asking for a tile pathfinding
	var/list/datum/ai_behavior/node_pathfinding_to_do = list()

/datum/controller/subsystem/advanced_pathfinding/Initialize()
	var/list/nodes = list()
	for(var/obj/effect/ai_node/ai_node AS in GLOB.all_nodes)
		nodes += list(ai_node.serialize())
	rustg_register_nodes_astar(json_encode(nodes))
	return SS_INIT_SUCCESS

#ifdef TESTING
#define BENCHMARK_LOOP while(world.timeofday < end_time)
#define BENCHMARK_RESET end_time = world.timeofday + duration

/// Run a benchmark comparing dm pathfinding with rust one. Will freeze server for 2 * run_number seconds
/datum/controller/subsystem/advanced_pathfinding/proc/benchmark(run_number)
	var/duration = 1 SECONDS
	var/end_time = world.timeofday + duration

	var/dm_iterations = 0
	var/rust_iterations = 0

	for(var/i in 1 to run_number)
		var/obj/effect/ai_node/start_node = pick(GLOB.all_nodes)
		var/obj/effect/ai_node/goal_node = pick(GLOB.all_nodes)

		while (start_node.z != goal_node.z || goal_node == start_node)
			goal_node = pick(GLOB.all_nodes)

		BENCHMARK_LOOP
			get_path(start_node, goal_node)
			dm_iterations++
		BENCHMARK_RESET

		BENCHMARK_LOOP
			rustg_generate_path_astar("[start_node.unique_id]", "[goal_node.unique_id]")
			rust_iterations++
		BENCHMARK_RESET

	message_admins("Average number of iterations for dm pathfinding in one sec : [dm_iterations/run_number]")
	message_admins("Average number of iterations for rust pathfinding in one sec : [rust_iterations/run_number]")
#endif //TESTING

/datum/controller/subsystem/advanced_pathfinding/fire()
	for(var/datum/ai_behavior/ai_behavior AS in tile_pathfinding_to_do)
		ai_behavior.look_for_tile_path()
		tile_pathfinding_to_do -= ai_behavior
		if (MC_TICK_CHECK)
			return
	for(var/datum/ai_behavior/ai_behavior AS in node_pathfinding_to_do)
		ai_behavior.look_for_node_path()
		node_pathfinding_to_do -= ai_behavior
		ai_behavior.registered_for_node_pathfinding = FALSE
		if (MC_TICK_CHECK)
			return

/datum/controller/subsystem/advanced_pathfinding/stat_entry(msg)
	msg = "Node pathfinding : [length(node_pathfinding_to_do)] || Tile pathfinding : [length(tile_pathfinding_to_do)]"
	return ..()


#define NODE_PATHING "node_pathing" //Looking through the network of nodes the best node path
#define TILE_PATHING "tile_pathing" //Looking the best tile path
GLOBAL_LIST_EMPTY(goal_nodes)

///Basic implementation of A* using atoms. Very cheap, at max it will do about 50-100 distance check for a whole path, but typically it will do 10-20
/datum/path_step
	///Euclidian distance to the goal atom
	var/distance_to_goal
	///Sum of euclidian distances to get from the starting atom to this atom, if you follow the current optimal path
	var/distance_walked
	///What atom this path reached
	var/atom/current_atom
	///What atom was right before current atom in the path
	var/atom/previous_atom

/datum/path_step/New(atom/previous_atom, atom/current_atom, atom/goal_atom, old_distance_walked)
	..()
	distance_to_goal = get_dist_euclidean_square(current_atom, goal_atom)
	distance_walked = old_distance_walked + get_dist_euclidean_square(current_atom, previous_atom)
	src.current_atom = current_atom
	src.previous_atom = previous_atom

///Returns the most optimal path to get from starting atom to goal atom
/proc/get_path(atom/starting_atom, atom/goal_atom, pathing_type = NODE_PATHING)
	if(starting_atom.z != goal_atom.z || starting_atom == goal_atom)
		return
	var/list/datum/path_step/paths_to_check = list()
	var/atom/current_atom = starting_atom
	var/list/datum/path_step/paths_checked = list()
	var/datum/path_step/current_path
	var/list/list_of_direction
	//Have we reached our goal atom yet?
	while(current_atom != goal_atom)
		//Check all possible next atoms, create an atom path for all of them
		switch(pathing_type)
			if(NODE_PATHING)
				var/obj/effect/ai_node/current_node = current_atom
				list_of_direction = current_node.adjacent_nodes
			if(TILE_PATHING)
				list_of_direction = GLOB.alldirs
		var/atom_to_check
		for(var/direction in list_of_direction)
			switch(pathing_type)
				if(NODE_PATHING)
					var/obj/effect/ai_node/current_node = current_atom
					atom_to_check = current_node.adjacent_nodes[direction]
				if(TILE_PATHING)
					var/turf/turf_to_check = get_step(current_atom, direction)
					if(turf_to_check.density || turf_to_check.atom_flags & AI_BLOCKED)
						continue
					atom_to_check = turf_to_check
			if(paths_to_check[atom_to_check] || paths_checked[atom_to_check] || !atom_to_check) //We already found a better path to get to this atom
				continue
			paths_to_check[atom_to_check] = new/datum/path_step(current_atom, atom_to_check, goal_atom, current_path?.distance_walked)
		paths_checked[current_atom] = current_path
		paths_to_check -= current_atom
		//We looked through all atoms, we didn't find a way to get to our end points
		if(!length(paths_to_check) || length(paths_checked) > PATHFINDER_MAX_TRIES)
			return
		//We created a atom path for each adjacent atom, we sort every atoms by their heuristic score
		sortTim(paths_to_check, GLOBAL_PROC_REF(cmp_path_step), TRUE) //Very cheap cause almost sorted
		current_path = paths_to_check[paths_to_check[1]] //We take the atom with the smaller heuristic score (distance to goal + distance already made)
		current_atom = current_path.current_atom
	paths_checked[current_atom] = current_path
	var/list/atom/atoms_path = list()
	//We can go back our track, making the path along the way
	while(current_atom != starting_atom)
		atoms_path += current_atom
		#ifdef TESTING
		new /obj/effect/temp_visual/telekinesis(current_atom)
		#endif
		current_atom = paths_checked[current_atom].previous_atom
	return atoms_path

/obj/effect/ai_node/goal
	name = "AI goal"
	invisibility = INVISIBILITY_OBSERVER
	faction = FACTION_XENO
	///Who made that ai_node
	var/mob/creator
	///The image added to the creator screen
	var/image/goal_image

/obj/effect/ai_node/goal/Initialize(mapload, mob/new_creator, new_faction)
	. = ..()
	if(new_faction)
		faction = new_faction
	SEND_GLOBAL_SIGNAL(COMSIG_GLOB_AI_GOAL_SET, src)
	RegisterSignal(SSdcs, COMSIG_GLOB_AI_GOAL_SET, PROC_REF(clean_goal_node))
	GLOB.goal_nodes[faction] = src
	if(!new_creator)
		return
	creator = new_creator
	RegisterSignal(creator, COMSIG_QDELETING, PROC_REF(clean_creator))
	goal_image = image('icons/Xeno/actions/leader.dmi', src, "minion_rendez_vous")
	goal_image.layer = HUD_PLANE
	goal_image.alpha = 180
	goal_image.pixel_y += 10
	animate(goal_image, pixel_y = pixel_y - 3, time = 7, loop = -1, easing = EASE_OUT)
	animate(pixel_y = pixel_y + 3, time = 7, loop = -1, easing = EASE_OUT)
	creator.client.images += goal_image

/obj/effect/ai_node/goal/LateInitialize()
	make_adjacents(TRUE)
	rustg_add_node_astar(json_encode(serialize()))

/obj/effect/ai_node/goal/Destroy()
	GLOB.goal_nodes -= faction
	if(creator)
		creator.client.images -= goal_image
	return ..()

///Null creator to prevent harddel
/obj/effect/ai_node/goal/proc/clean_creator()
	SIGNAL_HANDLER
	creator.client.images -= goal_image
	creator = null

///Delete this ai_node goal
/obj/effect/ai_node/goal/proc/clean_goal_node(datum/source, obj/effect/ai_node/goal/new_node)
	SIGNAL_HANDLER
	if(new_node.faction != faction)
		return
	qdel(src)

/obj/effect/ai_node/goal/zombie
	name = "Ai zombie goal"
	faction = FACTION_ZOMBIE

/obj/effect/ai_node/goal/neutral
	name = "Ai human goal"
	faction = FACTION_NEUTRAL

/obj/effect/ai_node/goal/tgmc
	faction = FACTION_TERRAGOV

/obj/effect/ai_node/goal/som
	faction = FACTION_SOM

/obj/effect/ai_node/goal/clf
	faction = FACTION_CLF

/obj/effect/ai_node/goal/freelancers
	faction = FACTION_FREELANCERS

/obj/effect/ai_node/goal/icc
	faction = FACTION_ICC

/obj/effect/ai_node/goal/alien
	faction = FACTION_ALIEN

/obj/effect/ai_node/goal/specops
	faction = FACTION_SPECFORCE

/obj/effect/ai_node/goal/vsd
	faction = FACTION_VSD

/obj/effect/ai_node/goal/hostile
	faction = FACTION_HOSTILE
