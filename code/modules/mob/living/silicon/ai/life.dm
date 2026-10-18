/mob/living/silicon/ai/Life(seconds_per_tick, times_fired)
	// Don't bother to process life if they're blocked from doing so.
	if(HAS_TRAIT(src, TRAIT_NO_TRANSFORM))
		return

	// Because AIs are not expected to be revived / live again, no more processing.
	if(stat == DEAD)
		SSmobs.stop_processing(src)
		return

	updatehealth()

	interactee?.check_eye(src)


/mob/living/silicon/ai/update_stat()
	. = ..()

	if(status_flags & GODMODE)
		return

	if(stat != DEAD)
		if(health <= get_death_threshold())
			death()
		else if(stat == UNCONSCIOUS)
			set_stat(CONSCIOUS)


/mob/living/silicon/ai/updatehealth()
	if(status_flags & GODMODE)
		return

	health = 100 - getOxyLoss() - getToxLoss() - getFireLoss() - getBruteLoss()

	update_stat()
