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
	/// The turf that we are watching for new acid puddles.
	var/turf/current_turf
	/// The acid puddle that we are relying on for the immunity.
	var/obj/effect/xenomorph/spray/immunity_puddle

/datum/mutation_upgrade/defense/melter/acidic_domain/on_gain()
	RegisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED, PROC_REF(on_movement))
	set_current_turf(get_turf(xenomorph_owner))
	set_immunity_puddle(locate(/obj/effect/xenomorph/spray) in xenomorph_owner.loc)

/datum/mutation_upgrade/defense/melter/acidic_domain/on_loss()
	UnregisterSignal(xenomorph_owner, COMSIG_MOVABLE_MOVED)
	set_current_turf(null)
	set_immunity_puddle(null)

/// Deals with all changes to the current_turf variable.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/set_current_turf(turf/new_turf)
	if(current_turf == new_turf)
		return
	if(current_turf)
		UnregisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON)
	if(new_turf)
		RegisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON, PROC_REF(on_atom_initialize_on_turf))
	current_turf = new_turf

/// Deals with all changes to the immunity_puddle variable.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/set_immunity_puddle(obj/effect/xenomorph/spray/new_puddle)
	if(immunity_puddle == new_puddle)
		return
	if(immunity_puddle)
		UnregisterSignal(immunity_puddle, COMSIG_QDELETING)
	if(new_puddle)
		RegisterSignal(immunity_puddle, COMSIG_QDELETING, PROC_REF(on_puddle_qdel))
	immunity_puddle = new_puddle
	if(!immunity_puddle)
		REMOVE_TRAIT(xenomorph_owner, TRAIT_SLOWDOWNIMMUNE, MUTATION_TRAIT)
		return
	ADD_TRAIT(xenomorph_owner, TRAIT_SLOWDOWNIMMUNE, MUTATION_TRAIT)

/// On movement, tries to find and set an acid puddle from our new location.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_movement(datum/source, atom/old_loc, movement_dir, forced, list/old_locs)
	SIGNAL_HANDLER
	if(current_turf)
		UnregisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON)
	current_turf = get_turf(xenomorph_owner)
	if(current_turf)
		RegisterSignal(current_turf, COMSIG_ATOM_INITIALIZED_ON, PROC_REF(on_atom_initialize_on_turf))
	var/obj/effect/xenomorph/spray/found_puddle = locate(/obj/effect/xenomorph/spray) in xenomorph_owner.loc
	set_immunity_puddle(found_puddle)

/// When our acid puddle is deleted, tries to find and set an acid puddle from our current location.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_puddle_qdel(datum/source)
	SIGNAL_HANDLER
	var/obj/effect/xenomorph/spray/new_found_puddle
	for(var/obj/effect/xenomorph/spray/next_puddle in current_turf.contents)
		if(next_puddle == source)
			continue
		new_found_puddle = next_puddle
		break
	set_immunity_puddle(new_found_puddle)

/// If an acid puddle is created while we don't have one, set it as our current one.
/datum/mutation_upgrade/defense/melter/acidic_domain/proc/on_atom_initialize_on_turf(datum/source, atom/new_atom)
	SIGNAL_HANDLER
	if(immunity_puddle || !istype(new_atom, /obj/effect/xenomorph/spray))
		return
	set_immunity_puddle(new_atom)

//*********************//
//       Offense       //
//*********************//

/datum/mutation_upgrade/offense/melter/corrosive_touch
	name = "Corrosive Touch"
	desc = "Your slash attacks now only deal burn damage and applies an additional two melting stacks."

/datum/mutation_upgrade/offense/melter/corrosive_touch/on_gain()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.second_damage_type = BURN
	melter_owner.second_damage_armor = ACID
	melter_owner.applied_acid_stacks = initial(melter_owner.applied_acid_stacks) + 2

/datum/mutation_upgrade/offense/melter/corrosive_touch/on_loss()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.second_damage_type = initial(melter_owner.second_damage_type)
	melter_owner.second_damage_armor = initial(melter_owner.second_damage_armor)
	melter_owner.applied_acid_stacks = initial(melter_owner.applied_acid_stacks)

/datum/mutation_upgrade/offense/melter/vaporous_touch
	name = "Vaporous Touch"
	desc = "Your slash attacks against targets with 6+ melting stacks will create a tiny cloud of acidic opaque gas ontop of them."

/datum/mutation_upgrade/offense/melter/vaporous_touch/on_gain()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.acid_stacks_gas_threshold = 6

/datum/mutation_upgrade/offense/melter/vaporous_touch/on_loss()
	if(!isxenomelter(xenomorph_owner))
		return
	var/mob/living/carbon/xenomorph/runner/melter/melter_owner = xenomorph_owner
	melter_owner.acid_stacks_gas_threshold = initial(melter_owner.acid_stacks_gas_threshold)

//*********************//
//       Utility       //
//*********************//

/datum/mutation_upgrade/utility/melter/acidic_splurge
	name = "Acidic Splurge"
	desc = "Corrosive Acid's cast time is reduced by one second."

/datum/mutation_upgrade/utility/melter/acidic_splurge/on_gain()
	var/datum/action/ability/activable/xeno/corrosive_acid/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/corrosive_acid/melter]
	if(!ability)
		return
	ability.acid_delay_increase = -1 SECONDS // In sum, anything that don't have an increased acid delay are now instant.

/datum/mutation_upgrade/utility/melter/acidic_splurge/on_loss()
	var/datum/action/ability/activable/xeno/corrosive_acid/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/corrosive_acid/melter]
	if(!ability)
		return
	ability.acid_delay_increase = initial(ability.acid_delay_increase)

/datum/mutation_upgrade/utility/melter/perpetual_dash
	name = "Perpetual Dash"
	desc = "Acid Dash can be recasted up to 3 times as long the plasma and recast prerequisites are met."

/datum/mutation_upgrade/utility/melter/perpetual_dash/on_gain()
	var/datum/action/ability/activable/xeno/charge/acid_dash/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/charge/acid_dash/melter]
	if(!ability)
		return
	ability.maximum_recasts = 3
	ability.available_recasts = !ability.available_recasts ? 0 : clamp(ability.available_recasts, ability.available_recasts + 3 - initial(ability.maximum_recasts), ability.maximum_recasts)

/datum/mutation_upgrade/utility/melter/perpetual_dash/on_loss()
	var/datum/action/ability/activable/xeno/charge/acid_dash/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/charge/acid_dash/melter]
	if(!ability)
		return
	ability.maximum_recasts = initial(ability.maximum_recasts)
	ability.available_recasts = min(ability.available_recasts, ability.maximum_recasts)

/datum/mutation_upgrade/utility/melter/quick_dash
	name = "Quick Dash"
	desc = "Acid Dash has no recast prerequisites."

/datum/mutation_upgrade/utility/melter/quick_dash/on_gain()
	var/datum/action/ability/activable/xeno/charge/acid_dash/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/charge/acid_dash/melter]
	if(!ability)
		return
	ability.recast_prerequisite = FALSE

/datum/mutation_upgrade/utility/melter/quick_dash/on_loss()
	var/datum/action/ability/activable/xeno/charge/acid_dash/melter/ability = xenomorph_owner.actions_by_path[/datum/action/ability/activable/xeno/charge/acid_dash/melter]
	if(!ability)
		return
	ability.recast_prerequisite = initial(ability.recast_prerequisite)
