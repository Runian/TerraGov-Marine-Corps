/mob/living/carbon/xenomorph/dragon
	caste_base_type = /datum/xeno_caste/dragon
	name = "Dragon"
	desc = "A primordial creature, evolved to smash the hardiest of defences and hunt the hardiest of prey."
	icon = 'icons/Xeno/castes/dragon.dmi'
	icon_state = "Dragon Walking"
	attacktext = "bites"
	attack_sound = null
	friendly = "nuzzles"
	health = 500
	maxHealth = 500
	plasma_stored = 300
	pixel_x = -16
	mob_size = MOB_SIZE_BIG
	drag_delay = 6
	tier = XENO_TIER_FOUR //King, like queen, doesn't count towards population limit.
	upgrade = XENO_UPGRADE_NORMAL
	bubble_icon = "alienroyal"
	inherent_verbs = list(
		/mob/living/carbon/xenomorph/proc/hijack,
	)

/mob/living/carbon/xenomorph/dragon/Initialize(mapload)
	. = ..()
	playsound(loc, 'sound/voice/alien/xenos_roaring.ogg', 75, 0)

/mob/living/carbon/xenomorph/dragon/generate_name()
	var/playtime_mins = client?.get_exp(xeno_caste.caste_name)
	var/prefix = (hive.prefix || xeno_caste.upgrade_name) ? "[hive.prefix][xeno_caste.upgrade_name] " : ""
	if(!client?.prefs.show_xeno_rank || !client)
		name = prefix + "Dragon ([nicknumber])"
		real_name = name
		if(mind)
			mind.name = name
		return
	switch(playtime_mins)
		if(0 to 600)
			name = prefix + "Young Dragon ([nicknumber])"
		if(601 to 1500)
			name = prefix + "Mature Dragon ([nicknumber])"
		if(1501 to 4200)
			name = prefix + "Elder Dragon ([nicknumber])"
		if(4201 to 10500)
			name = prefix + "Ancient Dragon ([nicknumber])"
		if(10501 to INFINITY)
			name = prefix + "Prime Dragon ([nicknumber])"
		else
			name = prefix + "Young Dragon ([nicknumber])"

	real_name = name
	if(mind)
		mind.name = name

/mob/living/carbon/xenomorph/dragon/death_cry()
	playsound(loc, 'sound/voice/alien/king_died.ogg', 75, 0)
