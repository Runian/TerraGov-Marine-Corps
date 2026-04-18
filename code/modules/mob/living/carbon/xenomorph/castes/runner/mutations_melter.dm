
//*********************//
//        Base        //
//*********************//

/datum/mutation_upgrade/defense/melter
	allowed_caste_names = list(/datum/xeno_caste/runner/melter)

/datum/mutation_upgrade/offense/melter
	allowed_caste_names = list(/datum/xeno_caste/runner/melter)

/datum/mutation_upgrade/utility/melter
	allowed_caste_names = list(/datum/xeno_caste/runner/melter)

//*********************//
//       Defense       //
//*********************//

/datum/mutation_upgrade/defense/melter/acid_release
	name = "Acid Release"
	desc = "Upon getting staggered, create non-stunning acid in a 3x3 around you. This can only be activated once, but resets when you reach full health."
	/// Can this be activated?
	var/can_be_activated = FALSE

/datum/mutation_upgrade/defense/melter/acid_release/on_gain()
	RegisterSignals(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE), PROC_REF(on_damage))
	RegisterSignal(xenomorph_owner, COMSIG_LIVING_STATUS_STAGGER, PROC_REF(on_staggered))
	if(xenomorph_owner.health >= xenomorph_owner.maxHealth)
		can_be_activated = TRUE
	return ..()

/datum/mutation_upgrade/defense/melter/acid_release/on_loss()
	UnregisterSignal(xenomorph_owner, list(COMSIG_XENOMORPH_BRUTE_DAMAGE, COMSIG_XENOMORPH_BURN_DAMAGE, COMSIG_LIVING_STATUS_STAGGER))
	can_be_activated = FALSE
	return ..()

/// When available, creates non-stunning acid in a 3x3 radius upon stagger.
/datum/mutation_upgrade/defense/melter/acid_release/proc/on_staggered(datum/source, amount, ignore_canstun)
	if(!can_be_activated || !isturf(xenomorph_owner.loc))
		return
	var/turf/current_turf = xenomorph_owner.loc
	for(var/turf/acid_tile AS in RANGE_TURFS(1, current_turf))
		if(!line_of_sight(current_turf, acid_tile))
			continue
		xenomorph_spray(acid_tile, 6 SECONDS, xenomorph_owner.xeno_caste.acid_spray_damage, xenomorph_owner, TRUE, TRUE)

/// If they have full health, make it available.
/datum/mutation_upgrade/defense/melter/acid_release/proc/on_damage(datum/source, amount, list/amount_mod)
	SIGNAL_HANDLER
	if(can_be_activated || xenomorph_owner.health < xenomorph_owner.maxHealth)
		return
	can_be_activated = TRUE

/datum/mutation_upgrade/defense/melter/extinguishing_shroud
	name = "Extinguishing Shroud"
	desc = "Melter Shroud's gas is now light neurotoxin and larger."

/datum/mutation_upgrade/defense/melter/extinguishing_shroud/on_gain()
	var/datum/action/ability/activable/xeno/melter_shroud/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/melter_shroud]
	if(!ability)
		return
	ability.smoke_typepath = /datum/effect_system/smoke_spread/xeno/neuro/light
	ability.radius = initial(ability.radius) + 1

/datum/mutation_upgrade/defense/melter/extinguishing_shroud/on_loss()
	var/datum/action/ability/activable/xeno/melter_shroud/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/melter_shroud]
	if(!ability)
		return
	ability.smoke_typepath = initial(ability.smoke_typepath)
	ability.radius = initial(ability.radius)


/datum/mutation_upgrade/defense/melter/acidic_domain
	name = "Acidic Domain"
	desc = "While ontop of any acid puddle, you are immune to slowdown."
	var/turf/current_turf
	/// The acid puddle that we are relying on for the immunity.
	var/obj/effect/xenomorph/spray/acid_puddle

/datum/mutation_upgrade/defense/melter/acidic_domain/on_gain()
	current_turf = get_turf(xenomorph_owner)
	if(current_turf)
		RegisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON, PROC_REF(on_atom_initialize_on_turf))
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_movement))
	set_acid_puddle(locate(/obj/effect/xenomorph/spray) in xenomorph_owner.loc)

/datum/mutation_upgrade/defense/melter/acidic_domain/on_loss()
	if(current_turf)
		UnregisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON)
	UnregisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED)
	set_acid_puddle(null)

/datum/mutation_upgrade/defense/melter/acidic_domain/proc/set_acid_puddle(obj/effect/xenomorph/spray/found_puddle)
	if(acid_puddle == found_puddle)
		return
	if(acid_puddle)
		UnregisterSignal(acid_puddle, COMSIG_QDELETING)
	acid_puddle = found_puddle
	if(!acid_puddle)
		REMOVE_TRAIT(xenomorph_owner, TRAIT_SLOWDOWNIMMUNE, MUTATION_TRAIT)
		return
	RegisterSignal(acid_puddle, COMSIG_QDELETING, PROC_REF(on_puddle_qdel))
	ADD_TRAIT(xenomorph_owner, TRAIT_SLOWDOWNIMMUNE, MUTATION_TRAIT)

/// On movement, tries to find and set a acid puddle from our new location.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_movement(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	SIGNAL_HANDLER
	var/obj/effect/xenomorph/spray/found_puddle = locate(/obj/effect/xenomorph/spray) in xenomorph_owner.loc
	set_acid_puddle(found_puddle)

/// When our acid puddle is deleted, tries to find and set an acid puddle from our current location.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_puddle_qdel(datum/source)
	SIGNAL_HANDLER
	var/obj/effect/xenomorph/spray/new_found_puddle
	for(var/obj/effect/xenomorph/spray/next_puddle in current_turf.contents)
		if(next_puddle == source)
			continue
		new_found_puddle = next_puddle
		break
	set_acid_puddle(new_found_puddle)

/// If an acid puddle is created while we don't have one, set it as our current one.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_atom_initialize_on_turf(datum/source, atom/new_atom)
	SIGNAL_HANDLER
	if(acid_puddle || !istype(new_atom, /obj/effect/xenomorph/spray))
		return
	set_acid_puddle(new_atom)

//*********************//
//         Spur        //
//*********************//
/datum/mutation_upgrade/offense/fully_acid
	name = "Fully Acid"
	desc = "All of your stash damage is burn damage and is now checked against acid. You inflict 1/2/3 additional melting acid stacks."
	/// For each structure, the additional stacks of melting acid.
	var/stacks_per_structure = 1

/datum/mutation_upgrade/offense/fully_acid/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "All of your stash damage is burn damage and is now checked against acid. You inflict [get_stacks(new_amount)] additional melting acid stacks."

/datum/mutation_upgrade/offense/fully_acid/on_gain()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.second_damage_type = BURN
	melter_owner.second_damage_armor = ACID
	return ..()

/datum/mutation_upgrade/offense/fully_acid/on_loss()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.second_damage_type = initial(melter_owner.second_damage_type)
	melter_owner.second_damage_armor = initial(melter_owner.second_damage_armor)
	return ..()

/datum/mutation_upgrade/offense/fully_acid/on_structure_update(previous_amount, new_amount)
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.applied_acid_stacks += get_stacks(new_amount - previous_amount)
	return ..()

/// Returns the amount of stacks of melting acid that will be given.
/datum/mutation_upgrade/offense/fully_acid/proc/get_stacks(structure_count)
	return stacks_per_structure * structure_count

//*********************//
//         Veil        //
//*********************//
/datum/mutation_upgrade/utility/acid_reserves
	name = "Acid Reserves"
	desc = "Corrosive Acid is now applied 50/75/100% faster."
	/// For the first structure, the percentage to speed up Corrosive Acid by.
	var/speedup_initial = 0.25
	/// For each structure, the additional percentage to speed up Corrosive Acid by.
	var/speedup_per_structure = 0.25

/datum/mutation_upgrade/utility/acid_reserves/get_desc_for_alert(new_amount)
	if(!new_amount)
		return ..()
	return "Corrosive Acid is now applied [PERCENT(get_speedup(new_amount))]% faster."

/datum/mutation_upgrade/utility/acid_reserves/on_structure_update(previous_amount, new_amount)
	var/datum/action/ability/activable/xeno/corrosive_acid/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/corrosive_acid/melter]
	if(!ability)
		return
	if(previous_amount)
		ability.acid_speed_multiplier *= 1 + get_speedup(previous_amount) // First, we reverse...
	if(new_amount)
		ability.acid_speed_multiplier /= 1 + get_speedup(new_amount) // ... then we re-apply because math is hard!
	return ..()

/// Returns the percentage used to speed up Corrosive Acid by.
/datum/mutation_upgrade/utility/acid_reserves/proc/get_speedup(structure_count)
	return speedup_initial + (speedup_per_structure * structure_count)

