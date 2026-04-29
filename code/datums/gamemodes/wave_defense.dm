/datum/game_mode/wave_defense
	name = "Wave Defense"
	config_tag = "Wave Defense"
	round_type_flags = MODE_DISALLOW_RAILGUN
	xeno_abilities_flags = ABILITY_CRASH
	valid_job_types = list(
		/datum/job/terragov/squad/standard = -1,
		/datum/job/terragov/squad/engineer = 1,
		/datum/job/terragov/squad/corpsman = 1,
		/datum/job/terragov/squad/smartgunner = 1,
		/datum/job/terragov/squad/leader = 1,
		/datum/job/terragov/silicon/synthetic = 1,
		/datum/job/terragov/command/fieldcommander = 1,
	)
	respawn_time = 10 MINUTES
	whitelist_ground_maps = list(MAP_WAVE_DEFENSE)
	/// The amount of deciseconds before the first spawn wave occurs.
	var/grace_period = 8 MINUTES
	/// The amount of deciseconds between spawn waves.
	var/wave_time = 1 MINUTES
	/// The timer id of the callback proc that will create a new wave of xenomorphs.
	var/wave_timer_id
	/// List of all possible waves that could be spawned.
	var/list/waves_spawners = list()
	/// The multiplier of points that are used to calculated how many xenomorphs there will be in a wave.
	var/point_multiplier = 0.7
	/// The multiplier of health of created xenomorphs.
	var/health_multiplier = 0.7

/datum/game_mode/wave_defense/process()
	if(round_finished)
		return PROCESS_KILL

/datum/game_mode/wave_defense/pre_setup()
	. = ..()
	// Groundside spawn points only.
	for(var/job_type in GLOB.spawns_by_job)
		for(var/atom/loc in GLOB.spawns_by_job[job_type])
			if(is_ground_level(loc.z))
				continue
			GLOB.spawns_by_job[job_type] -= loc
	for(var/latejoin in GLOB.latejoin)
		var/atom/loc = latejoin
		if(is_ground_level(loc.z))
			continue
		GLOB.latejoin -= loc
	for(var/latejoin_cryo in GLOB.latejoin_cryo)
		var/atom/loc = latejoin_cryo
		if(is_ground_level(loc.z))
			continue
		GLOB.latejoin_cryo -= loc
	for(var/latejoin_gateway in GLOB.latejoin_gateway)
		var/atom/loc = latejoin_gateway
		if(is_ground_level(loc.z))
			continue
		GLOB.latejoin_gateway -= loc

/datum/game_mode/wave_defense/post_setup()
	. = ..()
	for(var/datum/wave_spawner/wave_spawner_typepath in subtypesof(/datum/wave_spawner))
		waves_spawner += new wave_spawner_typepath()
	wave_timer_id = addtimer(CALLBACK(src, PROC_REF(spawn_wave)), grace_period)

/datum/game_mode/wave_defense/announce()
	to_chat(world, "<b>The current game mode is - Wave Defense!</b>")
	priority_announce(
		title = "High Command Update",
		subtitle = "Good morning, marines.",
		message = "Unidentified organisms are approaching your location in [grace_period / 1 MINUTE] minutes! Hold the line!",
		color_override = "red"
	)

/datum/game_mode/wave_defense/check_finished()
	if(round_finished)
		return TRUE
	var/list/living_player_list = count_humans_and_xenos(count_flags = COUNT_IGNORE_HUMAN_SSD)
	if(!living_player_list[1])
		message_admins("Round finished: [MODE_INFESTATION_X_MAJOR]") // No one remains.
		round_finished = MODE_INFESTATION_X_MAJOR
		return TRUE
	return FALSE

/datum/game_mode/wave_defense/declare_completion()
	deltimer(wave_timer_id)
	wave_timer_id = null
	return ..()

/// Spawns a wave of xenomorphs and then scales future waves to be harder.
/datum/gamemode/wave_defense/proc/spawn_wave()
	var/list/datum/wave_spawner/acceptable_wave_spawners = list()
	for(var/datum/wave_spawner/possible_wave_spawner AS in waves_spawners)
		if(!possible_wave_spawner.can_spawn_wave())
			continue
		acceptable_wave_spawners += possible_wave_spawner
	if(!length(acceptable_wave_spawners))
		wave_timer_id = addtimer(CALLBACK(src, PROC_REF(spawn_wave)), wave_time)
		return
	var/datum/wave_spawner/chosen_wave_spawner = pick(acceptable_wave_spawners)
	var/list/living_player_list = count_humans_and_xenos(count_flags = COUNT_IGNORE_HUMAN_SSD)
	chosen_wave_spawner.spawn_wave(living_player_list[1] * point_multiplier, health_multiplier)
	point_multiplier += 0.05
	health_multiplier += 0.05

// TODO: Make unique groundside map (or rehash Fort Phobos). (use MAP_WAVE_DEFENSE).
// TODO: Figure out what jobs to allow.
// TODO: Req groundside?
// TODO: Random events?
// TODO: Additional end check if marines don't meaningfully defend, but are still alive.
// TODO: Make walls unbuildable.
// TODO: Create some wave_spawner(s)!

/datum/wave_spawner
	/// If the round has lasted this amount of deciseconds, then this wave can be spawned.
	var/minimum_spawn_time
	/// If the round has lasted this amount of deciseconds, then this wave cannot be spawned.
	var/maximum_spawn_time
	/// List of all typepaths to spawn.
	var/list/mob/living/carbon/xenomorph/xenomorph_typepaths = list()
	/// The point cost per spawn.
	var/point_cost = 1

/// Can a wave be spawned?
/datum/wave_spawner/proc/can_spawn_wave()
	var/time_since_round_start = world.time - SSticker.round_start_time
	if(minimum_spawn_time && minimum_spawn_time > time_since_round_start)
		return FALSE
	if(maximum_spawn_time && maximum_spawn_time <= time_since_round_start)
		return FALSE
	return TRUE

/// Spawns xenomorphs until points run out.
/datum/wave_spawner/proc/spawn_wave(points, health_multiplier)
	if(!points || !point_cost)
		return
	for(var/i in 1 to ROUND_UP(amount / point_cost))
		var/mob/living/carbon/xenomorph/xenomorph_typepath = pick(xenomorph_typepaths)
		var/mob/living/carbon/xenomorph/spawned_xenomorph = new xenomorph_typepath(pick(GLOB.waves_spawner_locs))
		spawned_xenomorph.maxHealth *= health_multiplier
		spawned_xenomorph.health = xenomorph.maxHealth

