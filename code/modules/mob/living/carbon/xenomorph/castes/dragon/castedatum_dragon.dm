/* The TODO list in order: https://hackmd.io/@wlLEbadtQoCyE4iytTkonw/S10wubC1A
<<<<<<< HEAD
<<<<<<< HEAD
The Less Brain-Dead List:
✔️ Can Speak Common
✔️ Stamina Bar
✔️ Immunity to all forms of Crowd Control (Stagger/Stun/Slow/Flings)
✔️ Cannot be summoned by King.
❌ Regenerate Stamina Out of Combat
	- If they haven't taken damage in the last 10 seconds, they are considered out of combat.
	- Snowflaking: Any ability and slash attacks reset the timer.
❌ Does not heal from weeds with exceptions.
	- If they are non-groundside, they can rest to heal 0.5% of their max health (scales with regeneration power).
❌ Does not gain speed from weeds.
❌ Cannot be affected by various abilities that other xenos have.
	- A list includes, but is not limited to:
		Healing abilities (drone/hivelord/queen/hivemind)
		Lifelink and lifeshield (gorger)
		Flings (shrike/crusher/warrior)
		Fire-proof resin (drone/hivelord)
		Primo Fruits (drone/hivelord/queen)
❌ Cannot tunnel.
❌ Cannot swap places with / shuffle enemies or allies.
❌ Slash Attack
	- Needs to be telegraphed / delayed (1 second).
	- On attack completion, do the slash attack effect that Ravager has.
	- 80 damage to everything applicable; 160 damages for vehicles.
❌ Dragon's Breath (Ability)
	- 12 second cooldown.
	- Costs 1 stamina to use.
	- A copy of King's Roar except:
		Shorter radius/range.
		Deals burn damage.
		Creates Pyrogen fire in the affected area.
		Gives maximum pyrogen / melting fire stacks to every carbon in affected area.
❌ Tail Swipe (Ability)
	- 12 second cooldown.
	- Costs 1 stamina to use.
	- Selects all turfs in front and aside of them in a range of 2.
	- Turns them around (for flavor purposes as this ability swipes their tail behind them).
	- 75 damage to everything within the inner range (1). Knockdown for 0.5 seconds.
	- 37.5 damage to everything within the outer range (2). Knockdown for 1.0 seconds & 1.0 seconds of stagger.
	- Fling every carbon within both ranges away from them as a non-safe throw by 2 tiles.
	- Sound effect / emote for hitting anyone
	- Sound effect / emote for hitting nothing.
❌ Dragon's Flight / Landing (Ability)
	✔️ 60 second cooldown.
	✔️ Costs 0 stamina to use.
	❌ Flying:
		✔️ Takes 4 seconds to fully cast.
		❌ Cannot activate if inside of caves or not groundside.
		❌ If they change z-levels (e.g. Alamo/TAD) and it was not due to hijack, forcibly land / right-click this ability.
		✔️ Loses stagger immunity during casting. Regain it back after they're done.
		✔️ If staggered, cancels and adds 30 seconds of cooldown.
		❌ While flying, has godmode/immunity typical of Hivemind and can move anywhere they can.
		❌ Shadow sprite beneath them to indicate that they are flying.
		❌ Successfully casting should display a take-flight animation of sorts.
	❌ Landing:
		✔️ Takes 4 seconds to fully cast.
		❌ Cannot activate if inside of caves, within MARINE_BASE, or non-groundside.
		❌ Telegraphed with a range of 2.
		❌ Deals 75 damage to everything in range and knockdown for 1 second.
		❌ If landing right ontop of a carbon, deal double damage and cause them to emote: scream.
		❌ Restores sprite to normal (from shadow sprite).
		❌ Successfully casting should display a landing animation of sorts.
	- Right Click:
		If flying:
			If non-hijack:
				Teleport them to a random silo that is groundside.
				If no silo, teleport them to a random xeno groundside.
				If no xeno, teleport them to a random weed node groundside.
				If no weed node, teleport them to anything alive groundside (including the marines).
				If all else fails, send the message "There is no safe place to fly to. Forcibly landing." and forcibly land them.
					❓ This is meant to ensure that they are not permanently stuck flying.
			If hijack:
				Send the message "There is no safe place to fly to. Forcibly landing." and forcibly land them.
					❓ At this point, they should effectively lost their ability to fly as it is a groundside-only ability.
					❓ If they're stuck flying outside and get killed by the hijack, that is their fault.
		If not flying / landed:
			Nothing.
❌ Dark Orb (Ability/Spell)
	30 second cooldown.
	Costs 2 stamina to use.
	Creates a slow moving black-colored projectile that looks like the tesla ball from Marine's tesla gun, but even slower.
	Cast time of 2 seconds. This portion does not need to be telegraphed.
	Loses stagger immunity during casting and regained on cancel/success.
	If staggered or takes 250 damage (after armor), forcibly cancel the ability.
	Once the projectile hits something (including friendly xenos), it activates:
		- Gets all turfs in range of 1 (aka 3x3).
		- Telegraphed and delayed by 1 seconds.
			Telegraph sprite is Warlock's gravity crush (prepping verison).
			Completion should be Warlock's gravity crush (activation verison).
		- Deals 150 burn damage to carbons and does a lengthy (1.2) slowdown. No stagger.
		- Deals 270 burn damage to vehicles (for APCs/Multitile Vehicles, do 30 damage per tile).
❌ Miasma (Ability/Spell)
	30 second cooldown.
	Costs 2 stamina to use.
	Cast time of 2 seconds. This portion does not need to be telegraphed.
	Loses stagger immunity during casting and regained on cancel/success.
	If staggered or takes 250 damage (after armor), forcibly cancel the ability.
	Gets every turf visible. Applies an sprite effect similar to Queen's Balwark except black-colored.
	Every marine that stands in it loses stamina (-10) per tick and has their slowdown set to a minimum of 0.3 (effectively always slowed while in it)/
	Lasts 10 seconds.
❌ Grab (Ability/Spell)
	30 second cooldown.
	Costs 2 stamina to use.
	Cast time of 2 seconds.
	Loses stagger immunity during casting and regained on cancel/success.
	If staggered or takes 250 damage (after armor), forcibly cancel the ability.
	Gets up to 7 turfs in a straight line (no diagonal targetting) with line of sight. Telegraphed.
	Does a grab similar to Warrior's grab, except:
		0.1 second knockdown to victim
		Victim can STAND and fight back (either with a gun, their knife, or their fist).
		Victim is unshuffable (help intent)/unpushable (non-help intent) and must be GRABBED by a friendly marine to get out of the grab.
		Victim can still be thrown around (e.g. explosion knockback) and get out of the grab.
	While grabbing someone:
		If takes 200 damage, cancel/drop the grab.
		Can move slower similar to Warrior's grab.
		Automatically cancels after 20 seconds (to prevent dragging to omega xeno hell).
		If victim dies, stop grab
❌ Wind Current (Ability/Spell)
	30 second cooldown.
	Costs 2 stamina to use.
	Cast time of 2 seconds.
	Loses stagger immunity during casting and regained on cancel/success.
	If staggered or takes 250 damage (after armor), forcibly cancel the ability.
	Looks like and is telegraphed like Behemoth's charge except it is 3 tiles wide and 7 tiles long.
	After success, remove all gas and fling carbons 7 tiles as non-safe throw & knockdown of 1 second.
... TBA ...
=======
base xeno_caste, xeno, etc
Health (calm)
Soft Armor (calm)
Hard Armor (calm)
Speed (calm)
=======
base xeno_caste, xeno, etc ✔
Health ✔
Soft Armor ✔
Hard Armor ✔
Speed ✔
<<<<<<< HEAD
>>>>>>> 36bd1ad143 (health/armor/speed)
=======
can speak english ✔
stamina sprites :) ✔
15 xeno limit ✔
<<<<<<< HEAD
>>>>>>> 33b3744639 (stamina (plasma) sprites)
ms-paint level of Sprites
=======
ms-paint level of Sprites ✔
>>>>>>> 94544a2828 (ms level sprites)
cool descriptions!
Immunity to CC: slow / stun / fling / stagger
give caste_flag to prevent plasma/health regen on weeds
no pheromone given
no king summon allowed
no slash like Hivemind
no benefit from friendly xeno skills (like deathwing in HoTS!) :)
EVERYTHING ELSE WAHHHH
REAL Sprites
>>>>>>> 3b691bcd4d (initial garbage)
*/

/datum/xeno_caste/dragon
	caste_name = "Dragon"
	display_name = "Dragon"
	upgrade_name = ""
	caste_type_path = /mob/living/carbon/xenomorph/dragon
	caste_desc = "A big scary monster with wings!"

	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_BASETYPE

	// *** Melee Attacks *** //
	melee_damage = 80

	// *** Speed *** //
	speed = -0.6

	// *** Plasma *** //
	plasma_max = 10
	plasma_gain = 0
	plasma_icon_state = "stamina"

	// *** Health *** //
	max_health = 1650 // Calm (750) + Enraged (900)

	tier = XENO_TIER_FOUR
	upgrade = XENO_UPGRADE_BASETYPE
	wound_type = "dragon" //used to match appropriate wound overlays

	// *** Sunder *** //
	sunder_multiplier = 0.8

	// *** Evolution *** //
	upgrade_threshold = TIER_THREE_THRESHOLD
	maximum_active_caste = 1

	evolve_min_xenos = 15
	death_evolution_delay = 7 MINUTES

	// *** Flags *** //
	caste_flags = CASTE_NO_HEALING|CASTE_NO_PLASMA_REGEN|CASTE_IS_INTELLIGENT|CASTE_STAGGER_IMMUNE|CASTE_KNOCKBACK_IMMUNE|CASTE_SLOW_IMMUNE|CASTE_STUN_IMMUNE|CASTE_NO_PSYCHIC_SUMMON|CASTE_INSTANT_EVOLUTION|CASTE_LEADER_TYPE
	can_flags = CASTE_CAN_CORRUPT_GENERATOR
	caste_traits = null

	// *** Defense *** //
	soft_armor = list(MELEE = 75, BULLET = 75, LASER = 75, ENERGY = 75, BOMB = 50, BIO = 75, FIRE = 200, ACID = 75)
	hard_armor = list(MELEE = 10, BULLET = 10, LASER = 10, ENERGY = 10, BOMB = 10, BIO = 10, FIRE = 10, ACID = 10)

	minimap_icon = "xenoking"

	actions = list(
		// Actions given to all T0s:
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		// Dragon-specific actions:
		/datum/action/ability/xeno_action/dragon_flight,
		/datum/action/ability/activable/xeno/dragon_breath,
		/datum/action/ability/activable/xeno/tail_swipe,
		// Actions given to all T4s:
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)

/datum/xeno_caste/dragon/normal
	upgrade = XENO_UPGRADE_NORMAL

/datum/xeno_caste/dragon/primordial
	upgrade_name = "Primordial"
	caste_desc = "Ancient terror. Your end has come, and it bears my wings."
	primordial_message = "Destruction is my creed; none shall withstand my fury."
	upgrade = XENO_UPGRADE_PRIMO

	actions = list(
		// Actions given to all T0s:
		/datum/action/ability/xeno_action/xeno_resting,
		/datum/action/ability/xeno_action/watch_xeno,
		/datum/action/ability/activable/xeno/psydrain,
		// Dragon-specific actions:
		/datum/action/ability/xeno_action/dragon_flight,
		/datum/action/ability/activable/xeno/dragon_breath,
		/datum/action/ability/activable/xeno/tail_swipe,
		// Actions given to all T4s:
		/datum/action/ability/xeno_action/call_of_the_burrowed,
		/datum/action/ability/xeno_action/hive_message,
		/datum/action/ability/xeno_action/rally_hive,
		/datum/action/ability/xeno_action/rally_minion,
		/datum/action/ability/xeno_action/blessing_menu,
	)
