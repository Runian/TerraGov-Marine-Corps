/* changelog
	code is less shit
	process heals one-damage at a time instead of ALL of them at the same time
	code documented
	detects all implants (doesn't really matter in campaign since there are basically no autodocs)
*/

#define AUTODOC_NOTICE_SUCCESS 1
#define AUTODOC_NOTICE_DEATH 2
#define AUTODOC_NOTICE_NO_RECORD 3
#define AUTODOC_NOTICE_NO_POWER 4
#define AUTODOC_NOTICE_XENO_FUCKERY 5
#define AUTODOC_NOTICE_IDIOT_EJECT 6
#define AUTODOC_NOTICE_FORCE_EJECT 7

#define AUTODOC_SURGERY_TYPE_UNDEFINED 0
#define AUTODOC_SURGERY_TYPE_LIMB 1
#define AUTODOC_SURGERY_TYPE_ORGAN 2
#define AUTODOC_SURGERY_TYPE_EXTERNAL 3

#define AUTODOC_SURGERY_PROCEDURE_UNDEFINED 0
#define AUTODOC_SURGERY_PROCEDURE_INTERNAL_BLEEDING 1
#define AUTODOC_SURGERY_PROCEDURE_ORGAN_REPAIR 2
#define AUTODOC_SURGERY_PROCEDURE_DISFIGURED_FACE 3 // Also covers incomplete face surgery.
#define AUTODOC_SURGERY_PROCEDURE_FRACTURED_BONE 4
#define AUTODOC_SURGERY_PROCEDURE_MISSING_LIMB 5
#define AUTODOC_SURGERY_PROCEDURE_NECROTIZED_LIMB 6
#define AUTODOC_SURGERY_PROCEDURE_FOREIGN_BODIES 7 // Also covers schrapel removal.
#define AUTODOC_SURGERY_PROCEDURE_INFECTED_GERMS 8
#define AUTODOC_SURGERY_PROCEDURE_OPEN_INCISION 9
#define AUTODOC_SURGERY_PROCEDURE_EYE_DAMAGE 10
#define AUTODOC_SURGERY_PROCEDURE_BRUTE_DAMAGE 11
#define AUTODOC_SURGERY_PROCEDURE_BURN_DAMAGE 12
#define AUTODOC_SURGERY_PROCEDURE_TOXIN_DAMAGE 13
#define AUTODOC_SURGERY_PROCEDURE_DIALYSIS 14
#define AUTODOC_SURGERY_PROCEDURE_LOW_BLOOD 15

#define AUTODOC_SURGERY_UNNEEDED_DELAY 2 SECONDS // The amount of additional time caused by queuing an unnecessary surgery.

/datum/autodoc_surgery
	/// The limb that will be operated on.
	var/datum/limb/limb_ref = null
	/// The organ that will be operated on.
	var/datum/internal_organ/organ_ref = null
	/// The surgery type.
	var/surgery_type = AUTODOC_SURGERY_TYPE_UNDEFINED
	/// The surgery precedure.
	var/surgery_procedure = AUTODOC_SURGERY_PROCEDURE_UNDEFINED
	/// Was this surgery queued up despite not being required / expecting nothing to be done.
	var/surgery_unnecessary = FALSE
	/// Progress on this surgery (for autodoc purposes only).
	var/surgery_progress = 0

/proc/create_autodoc_surgery(limb_ref, surgery_type, surgery_procedure, surgery_unnecessary = FALSE, organ_ref = null)
	var/datum/autodoc_surgery/autodoc_surgery = new()
	autodoc_surgery.limb_ref = limb_ref
	autodoc_surgery.organ_ref = organ_ref
	autodoc_surgery.surgery_type = surgery_type
	autodoc_surgery.surgery_procedure = surgery_procedure
	autodoc_surgery.surgery_unnecessary = surgery_unnecessary
	return autodoc_surgery

/proc/generate_autodoc_surgery_list(mob/living/carbon/human/this_human)
	if(!ishuman(this_human))
		return list()

	var/surgery_list = list()

	// Going through every limb.
	for(var/datum/limb/this_limb in this_human.limbs)
		if(!this_limb)
			continue

		// Internal Bleeding
		if(length(this_limb.wounds))
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_INTERNAL_BLEEDING)

		// Organs
		var/organ_damage_surgery_added = 0
		for(var/datum/internal_organ/this_organ in this_limb.internal_organs)
			// Can't deal with non-organic organs.
			if(this_organ.robotic == ORGAN_ASSISTED || this_organ.robotic == ORGAN_ROBOT)
				continue
			// Eye surgery is handled seperately further along.
			if(this_organ.organ_id == ORGAN_EYES)
				continue
			if(!this_organ.damage)
				continue
			// Organ repair (generally) fixes all organs in that limb. No need to add mulitple surgeries.
			if(organ_damage_surgery_added)
				continue
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_ORGAN, AUTODOC_SURGERY_PROCEDURE_ORGAN_REPAIR, organ_ref = this_organ)
			organ_damage_surgery_added = TRUE

		// Head
		if(istype(this_limb,/datum/limb/head))
			var/datum/limb/head/head_limb = this_limb
			if(head_limb.disfigured || head_limb.face_surgery_stage > 0)
				surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_DISFIGURED_FACE)

		// Broken / Fractured
		if(this_limb.limb_status & LIMB_BROKEN)
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_FRACTURED_BONE)

		// Destroyed / Non-Existent
		if(this_limb.limb_status & LIMB_DESTROYED)
			if(!(this_limb.parent.limb_status & LIMB_DESTROYED) && this_limb.body_part != HEAD)
				surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_MISSING_LIMB)

		/// Necrotized
		if(this_limb.limb_status & LIMB_NECROTIZED)
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_NECROTIZED_LIMB)

		/// Sharpnel
		var/skip_embryo_check = FALSE
		if(length(this_limb.implants))
			for(var/obj/item/embedded AS in this_limb.implants)
				if(embedded.is_beneficial_implant()) // Only non-beneficial implants are from shrapnel and deathsquad's suicide implant.
					continue
				surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_FOREIGN_BODIES)
				if(this_limb.body_part == CHEST)
					skip_embryo_check = TRUE

		// Facehugged / Embryo
		var/obj/item/alien_embryo/alien_embryo = locate() in this_human
		if(alien_embryo && this_limb.body_part == CHEST && !skip_embryo_check)
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_FOREIGN_BODIES)

		// Too Much Germs / Infected
		if(this_limb.germ_level > INFECTION_LEVEL_ONE)
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_INFECTED_GERMS)

		// Opened Incision
		if(this_limb.surgery_open_stage)
			surgery_list += create_autodoc_surgery(this_limb, AUTODOC_SURGERY_TYPE_LIMB, AUTODOC_SURGERY_PROCEDURE_OPEN_INCISION)

	// Eyes
	var/datum/internal_organ/eyes_internal_organ = this_human.get_organ_slot(ORGAN_SLOT_EYES)
	if(eyes_internal_organ && (this_human.disabilities & NEARSIGHTED || this_human.disabilities & BLIND || eyes_internal_organ.damage > 0))
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_ORGAN, AUTODOC_SURGERY_PROCEDURE_EYE_DAMAGE, organ_ref = eyes_internal_organ)

	// Generic Damage
	if(this_human.getBruteLoss())
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_EXTERNAL, AUTODOC_SURGERY_PROCEDURE_BRUTE_DAMAGE)
	if(this_human.getFireLoss())
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_EXTERNAL, AUTODOC_SURGERY_PROCEDURE_BURN_DAMAGE)
	if(this_human.getToxLoss())
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_EXTERNAL, AUTODOC_SURGERY_PROCEDURE_TOXIN_DAMAGE)

	// Bad Reagents / Dialysis
	var/should_filter_reagents = FALSE
	for(var/datum/reagent/this_reagent in this_human.reagents.reagent_list)
		if(istype(this_reagent, /datum/reagent/toxin) || this_human.reagents.get_reagent_amount(this_reagent.type) > this_reagent.overdose_threshold)
			should_filter_reagents = TRUE
			break
	if(should_filter_reagents)
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_EXTERNAL, AUTODOC_SURGERY_PROCEDURE_DIALYSIS)

	// Low Blood
	if(this_human.blood_volume < BLOOD_VOLUME_NORMAL)
		surgery_list += create_autodoc_surgery(null, AUTODOC_SURGERY_TYPE_EXTERNAL, AUTODOC_SURGERY_PROCEDURE_LOW_BLOOD)
	return surgery_list



/obj/machinery/autodoc
	name = "\improper autodoc medical system"
	desc = "A fancy machine developed to be capable of operating on people with minimal human intervention. However, the interface is rather complex and most of it would only be useful to trained medical personnel."
	icon = 'icons/obj/machines/cryogenics.dmi'
	icon_state = "autodoc_open"
	density = TRUE
	anchored = TRUE
	coverage = 20
	req_one_access = list(ACCESS_MARINE_MEDBAY, ACCESS_MARINE_CHEMISTRY, ACCESS_MARINE_MEDPREP)
	light_range = 1
	light_power = 0.5
	light_color = LIGHT_COLOR_BLUE
	dir = EAST
	/// Should it be access-locked? Prevents ejection from those without access if so.
	var/locked = FALSE
	/// The human that it is currently inside.
	var/mob/living/carbon/human/occupant = null
	/// Is the occupant currently undergoing surgery?
	var/active_surgery = FALSE
	/// The list of surgeries we should do, in order.
	var/list/queued_surgeries = list()
	/// The current surgery that we are doing.
	var/datum/autodoc_surgery/current_surgery
	/// The timer id of the timer that callbacks the surgery loop.
	var/surgery_loop_timer_id



	/// Multiplies the entire surgery time by this amount.
	//var/surgery_time_mulitplier = 1
	/// Adds this amount of time to the surgery. Included as part of the surgery time and thus can be multiplied.
	//var/surgery_time_additive = 0 SECONDS

	var/list/surgery_todo_list = list() //a list of surgeries to do.
	var/surgery_mod = 1 //What multiple to increase the surgery timer? This is used for any non-WO maps or events that are done.
	var/filtering = 0
	var/blood_transfer = 0
	var/heal_brute = 0
	var/heal_burn = 0
	var/heal_toxin = 0
	var/automatic_mode = 0
	var/event = 0

	var/obj/machinery/computer/autodoc_console/connected

	//It uses power
	use_power = ACTIVE_POWER_USE
	idle_power_usage = 15
	active_power_usage = 120000 // It rebuilds you from nothing...

	var/stored_metal = 1000 // starts with 500 metal loaded
	var/stored_metal_max = 2000


/obj/machinery/autodoc/Initialize(mapload)
	. = ..()
	RegisterSignal(src, COMSIG_MOVABLE_SHUTTLE_CRUSH, PROC_REF(on_shuttle_crush))
	update_icon()

/obj/machinery/autodoc/Destroy()
	if(occupant)
		if(active_surgery)
			visible_message("\The [src] malfunctions as it is destroyed mid-surgery, ejecting [occupant] with surgical wounds and showering them in debris.")
			occupant.take_limb_damage(rand(30,50), rand(30,50))
		else
			visible_message("\The [src] is destroyed, ejecting [occupant] and showering them in debris.")
			occupant.take_limb_damage(rand(10,20), rand(10,20))
		handle_release_notice(AUTODOC_NOTICE_FORCE_EJECT)
		eject_occupant()
	if(connected)
		connected.connected = null
		connected = null
	return ..()

/obj/machinery/autodoc/power_change()
	. = ..()
	if(is_operational() || !occupant)
		return
	visible_message("[src] engages the safety override, ejecting the occupant.")
	handle_release_notice(AUTODOC_NOTICE_NO_POWER)
	eject_occupant()

/obj/machinery/autodoc/update_icon()
	. = ..()
	if(machine_stat & NOPOWER)
		set_light(0)
		return
	if(active_surgery || occupant)
		set_light(initial(light_range) + 1)
		return
	set_light(initial(light_range))

/obj/machinery/autodoc/update_icon_state()
	. = ..()
	if(machine_stat & NOPOWER)
		icon_state = "autodoc_off"
		return
	if(active_surgery)
		icon_state = "autodoc_operate"
		return
	if(occupant)
		icon_state = "autodoc_closed"
		return
	icon_state = "autodoc_open"

/obj/machinery/autodoc/update_overlays()
	. = ..()
	if(machine_stat & NOPOWER)
		return
	. += emissive_appearance(icon, "[icon_state]_emissive", alpha = src.alpha)

/obj/machinery/autodoc/process()
	if(!occupant)
		return

	if(occupant.stat == DEAD)
		say("Patient has expired.")
		handle_release_notice(AUTODOC_NOTICE_DEATH)
		eject_occupant()
		return

	if(!active_surgery)
		return

	// Carry over from previous code. Certain free healing to prevent certain types of death.
	occupant.adjustToxLoss(-0.5)
	occupant.adjustOxyLoss(-occupant.getOxyLoss())

	// Previously, everything was done at the same time. Now, it is done one at a time.
	if(filtering)
		var/filtered = 0
		for(var/datum/reagent/x in occupant.reagents.reagent_list)
			occupant.reagents.remove_reagent(x.type, 10) // same as sleeper, may need reducing
			filtered = TRUE
		if(!filtered)
			filtering = 0
			say("Blood filtering complete.")
		else if(prob(10))
			visible_message("[src] whirrs and gurgles as the dialysis module operates.")
			to_chat(occupant, span_info("You feel slightly better."))

	if(blood_transfer)
		// In the case when the autodoc console gets deleted somehow.
		if(!connected)
			blood_transfer = FALSE
			say("No blood bag detected. Ending blood transfer.")
			return
		// Blood levels are fine.
		if(occupant.blood_volume >= BLOOD_VOLUME_NORMAL)
			blood_transfer = FALSE
			say("Blood transfer complete.")
		// The autodoc console basically has infinite blood, but we love our flavortext!
		if(connected.blood_pack.reagents.get_reagent_amount(/datum/reagent/blood) < 4)
			connected.blood_pack.reagents.add_reagent(/datum/reagent/blood, 195, list("donor" = null, "blood_DNA" = null, "blood_type" = "O-"))
			say("Blood reserves depleted, switching to fresh bag.")
		occupant.inject_blood(connected.blood_pack, 8) // 2x IV stand rate (not that it matters).
		if(prob(10))
			visible_message("[src] whirrs and gurgles as it tranfuses blood.")
			to_chat(occupant, span_info("You feel slightly less faint."))
		return

	if(heal_brute)
		if(!occupant.getBruteLoss())
			heal_brute = FALSE
			say("Trauma repair surgery complete.")
			return
		occupant.heal_limb_damage(3, 0)
		occupant.updatehealth()
		if(prob(10))
			visible_message("[src] whirrs and clicks as it stitches flesh together.")
			to_chat(occupant, span_info("You feel your wounds being stitched and sealed shut."))
		return

	if(heal_burn)
		if(!occupant.getFireLoss())
			heal_burn = FALSE
			say("Skin grafts complete.")
			return
		occupant.heal_limb_damage(0, 3)
		occupant.updatehealth()
		if(prob(10))
			visible_message("[src] whirrs and clicks as it grafts synthetic skin.")
			to_chat(occupant, span_info("You feel your burned flesh being sliced away and replaced."))
		return

	if(heal_toxin)
		if(!occupant.getToxLoss())
			heal_toxin = FALSE
			say("Chelation complete.")
			return
		occupant.adjustToxLoss(-3)
		occupant.updatehealth()
		if(prob(10))
			visible_message("[src] whirrs and gurgles as it kelates the occupant.")
			to_chat(occupant, span_info("You feel slighly less ill."))
		return

/obj/machinery/autodoc/attack_alien(mob/living/carbon/xenomorph/xeno_attacker, damage_amount = xeno_attacker.xeno_caste.melee_damage, damage_type = BRUTE, armor_type = MELEE, effects = TRUE, armor_penetration = xeno_attacker.xeno_caste.melee_ap, isrightclick = FALSE)
	if(!occupant)
		to_chat(xeno_attacker, span_xenowarning("There is nothing of interest in there."))
		return
	if(xeno_attacker.status_flags & INCORPOREAL || xeno_attacker.do_actions)
		return
	eject_occupant_unexpected(xeno_attacker) // Xenomorphs get a special case here.






/obj/machinery/autodoc/verb/eject()
	set name = "Eject Med-Pod"
	set category = "Object"
	set src in oview(1)
	if(usr.incapacitated())
		return // nooooooooooo
	if(locked && !allowed(usr)) //Check access if locked.
		to_chat(usr, span_warning("Access denied."))
		playsound(loc,'sound/machines/buzz-two.ogg', 25, 1)
		return
	eject_occupant_unexpected(usr)


/obj/machinery/autodoc/MouseDrop_T(mob/M, mob/user)
	. = ..()
	insert_occupant(M, user)

/obj/machinery/autodoc/verb/move_inside()
	set name = "Enter Med-Pod"
	set category = "Object"
	set src in oview(1)

	insert_occupant(usr, usr)

/obj/machinery/autodoc/attackby(obj/item/I, mob/user, params)
	. = ..()
	if(.)
		return

	if(!ishuman(user))
		return // no

	if(istype(I, /obj/item/stack/sheet/metal))
		var/obj/item/stack/sheet/metal/M = I
		if(stored_metal >= stored_metal_max)
			to_chat(user, span_warning("[src]'s metal reservoir is full; it can't hold any more material!"))
			return
		stored_metal = min(stored_metal_max,stored_metal + M.amount * 100)
		to_chat(user, span_notice("[src] processes \the [I]. Its metal reservoir now contains [stored_metal] of [stored_metal_max] units."))
		user.drop_held_item()
		qdel(I)

	else if(istype(I, /obj/item/healthanalyzer) && occupant) //Allows us to use the analyzer on the occupant without taking him out.
		var/obj/item/healthanalyzer/J = I
		J.attack(occupant, user)
		return

/obj/machinery/autodoc/grab_interact(obj/item/grab/grab, mob/user, base_damage = BASE_OBJ_SLAM_DAMAGE, is_sharp = FALSE)
	. = ..()
	if(.)
		return
	if(!ishuman(user))
		return
	if(machine_stat & (NOPOWER|BROKEN))
		to_chat(user, span_notice("\ [src] is non-functional!"))
		return

	if(occupant)
		to_chat(user, span_notice("\ [src] is already occupied!"))
		return

	var/mob/grabbed_mob

	if(ismob(grab.grabbed_thing))
		grabbed_mob = grab.grabbed_thing

	else if(istype(grab.grabbed_thing,/obj/structure/closet/bodybag/cryobag))
		var/obj/structure/closet/bodybag/cryobag/cryobag = grab.grabbed_thing
		if(!cryobag.bodybag_occupant)
			to_chat(user, span_warning("The stasis bag is empty!"))
			return
		grabbed_mob = cryobag.bodybag_occupant
		cryobag.open()
		user.start_pulling(grabbed_mob)

	if(!ishuman(grabbed_mob))
		to_chat(user, span_notice("\ [src] is compatible with humanoid anatomies only!"))
		return

	if(grabbed_mob.abiotic())
		to_chat(user, span_warning("Subject cannot have abiotic items on."))
		return

	if(user.skills.getRating(SKILL_SURGERY) < SKILL_SURGERY_TRAINED && !event)
		user.visible_message(span_notice("[user] fumbles around figuring out how to put [grabbed_mob] into [src]."),
		span_notice("You fumble around figuring out how to put [grabbed_mob] into [src]."))
		var/fumbling_time = max(0 , SKILL_TASK_TOUGH - ( SKILL_TASK_EASY * user.skills.getRating(SKILL_SURGERY) ))// 8 secs non-trained, 5 amateur
		if(!do_after(user, fumbling_time, NONE, grabbed_mob, BUSY_ICON_UNSKILLED) || QDELETED(src))
			return

	visible_message("[user] starts putting [grabbed_mob] into [src].", 3)

	if(!do_after(user, 10, IGNORE_HELD_ITEM, grabbed_mob, BUSY_ICON_GENERIC) || QDELETED(src))
		return

	if(occupant)
		to_chat(user, span_notice("[src] is already occupied!"))
		return

	if(!grabbed_mob || !grab)
		return

	grabbed_mob.forceMove(src)
	occupant = grabbed_mob
	update_icon()
	var/implants = list(/obj/item/implant/neurostim)
	var/mob/living/carbon/human/H = occupant
	med_scan(H, null, implants, TRUE)
	start_processing()

	if(automaticmode)
		say("Automatic mode engaged, initialising procedures.")
		addtimer(CALLBACK(src, PROC_REF(auto_start)), 5 SECONDS)

	return TRUE

/////////////////////////////////////////////////////////////

//Auto Doc console that links up to it.
/obj/machinery/computer/autodoc_console
	name = "autodoc medical system control console"
	icon = 'icons/obj/machines/cryogenics.dmi'
	icon_state = "sleeperconsole"
	screen_overlay = "sleeperconsole_emissive"
	light_color = LIGHT_COLOR_EMISSIVE_RED
	req_one_access = list(ACCESS_MARINE_MEDBAY, ACCESS_MARINE_CHEMISTRY, ACCESS_MARINE_MEDPREP) //Valid access while locked
	density = FALSE
	idle_power_usage = 40
	dir = EAST
	var/obj/item/radio/headset/mainship/doc/radio
	var/obj/item/reagent_containers/blood/OMinus/blood_pack
	///connected autodoc
	var/obj/machinery/autodoc/connected = null
	///Are notifications for patient discharges turned on?
	var/release_notice = TRUE
	///Medics, Doctors and so on can lock this
	var/locked = FALSE

/obj/machinery/computer/autodoc_console/Initialize(mapload)
	. = ..()
	connected = locate(/obj/machinery/autodoc, get_step(src, REVERSE_DIR(dir)))
	if(connected)
		connected.connected = src
	radio = new(src)
	blood_pack = new(src)


/obj/machinery/computer/autodoc_console/Destroy()
	QDEL_NULL(radio)
	QDEL_NULL(blood_pack)
	if(connected)
		connected.connected = null
		connected = null
	return ..()

/obj/machinery/computer/autodoc_console/can_interact(mob/user)
	. = ..()
	if(!.)
		return FALSE

	if(!connected || !connected.is_operational())
		return FALSE

	if(locked && !allowed(user))
		return FALSE

	return TRUE



/obj/machinery/computer/autodoc_console/interact(mob/user)
	. = ..()
	if(.)
		return

	var/dat = ""

	if(locked)
		dat += "<hr>Lock Console</span> | <a href='?src=[text_ref(src)];locktoggle=1'>Unlock Console</a><BR>"
	else
		dat += "<hr><a href='?src=[text_ref(src)];locktoggle=1'>Lock Console</a> | Unlock Console<BR>"

	if(release_notice)
		dat += "<hr>Notifications On</span> | <a href='?src=[text_ref(src)];noticetoggle=1'>Notifications Off</a><BR>"
	else
		dat += "<hr><a href='?src=[text_ref(src)];noticetoggle=1'>Notifications On</a> | Notifications Off<BR>"

	if(connected.automatic_mode)
		dat += "<hr>[span_notice("Automatic Mode")] | <a href='?src=[text_ref(src)];automatictoggle=1'>Manual Mode</a>"
	else
		dat += "<hr><a href='?src=[text_ref(src)];automatictoggle=1'>Automatic Mode</a> | Manual Mode"

	dat += "<hr><font color='#487553'><B>Occupant Statistics:</B></FONT><BR>"
	if(!connected.occupant)
		dat += "No occupant detected."
		var/datum/browser/popup = new(user, "autodoc", "<div align='center'>Autodoc Console</div>", 600, 600)
		popup.set_content(dat)
		popup.open()
		return

	var/t1
	switch(connected.occupant.stat)
		if(CONSCIOUS)
			t1 = "Conscious"
		if(UNCONSCIOUS)
			t1 = "<font color='#487553'>Unconscious</font>"
		if(DEAD)
			t1 = "<font color='#b54646'>*Dead*</font>"
	var/operating
	switch(connected.active_surgery)
		if(0)
			operating = "Not in surgery"
		if(1)
			operating = "<font color='#b54646'><B>SURGERY IN PROGRESS: MANUAL EJECTION ONLY TO BE ATTEMPTED BY TRAINED OPERATORS!</B></FONT>"
	var/health_ratio = connected.occupant.health * 100 / connected.occupant.maxHealth
	dat += "[health_ratio > 50 ? "<font color='#487553'>" : "<font color='#b54646'>"]\tHealth %: [round(health_ratio)] ([t1])</FONT><BR>"
	var/pulse = connected.occupant.handle_pulse()
	dat += "[pulse == PULSE_NONE || pulse == PULSE_THREADY ? "<font color='#b54646'>" : "<font color='#487553'>"]\t-Pulse, bpm: [connected.occupant.get_pulse(GETPULSE_TOOL)]</FONT><BR>"
	dat += "[connected.occupant.getBruteLoss() < 60 ? "<font color='#487553'>" : "<font color='#b54646'>"]\t-Brute Damage %: [connected.occupant.getBruteLoss()]</FONT><BR>"
	dat += "[connected.occupant.getOxyLoss() < 60 ? "<font color='#487553'>" : "<font color='#b54646'>"]\t-Respiratory Damage %: [connected.occupant.getOxyLoss()]</FONT><BR>"
	dat += "[connected.occupant.getToxLoss() < 60 ? "<font color='#487553'>" : "<font color='#b54646'>"]\t-Toxin Content %: [connected.occupant.getToxLoss()]</FONT><BR>"
	dat += "[connected.occupant.getFireLoss() < 60 ? "<font color='#487553'>" : "<font color='#b54646'>"]\t-Burn Severity %: [connected.occupant.getFireLoss()]</FONT><BR>"

	dat += "<hr> Surgery Queue:<br>"

	var/list/surgeryqueue = list()

	var/datum/data/record/N = null
	for(var/datum/data/record/R in GLOB.datacore.medical)
		if (R.fields["name"] == connected.occupant.real_name)
			N = R
	if(isnull(N))
		N = create_medical_record(connected.occupant)

	if(connected.automaticmode)
		var/list/autosurgeries = N.fields["autodoc_data"]
		if(length(autosurgeries))
			dat += "[span_danger("Automatic Mode Ready.")]<br>"
		else

			dat += "[span_danger("Automatic Mode Unavaliable, Scan Patient First.")]<br>"
	else
		if(!isnull(N.fields["autodoc_manual"]))
			for(var/datum/autodoc_surgery/A in N.fields["autodoc_manual"])
				switch(A.type_of_surgery)
					if(EXTERNAL_SURGERY)
						switch(A.surgery_procedure)
							if(ADSURGERY_BRUTE)
								surgeryqueue["brute"] = 1
								dat += "Surgical Brute Damage Treatment"
							if(ADSURGERY_BURN)
								surgeryqueue["burn"] = 1
								dat += "Surgical Burn Damage Treatment"
							if(ADSURGERY_TOXIN)
								surgeryqueue["toxin"] = 1
								dat += "Toxin Damage Chelation"
							if(ADSURGERY_DIALYSIS)
								surgeryqueue["dialysis"] = 1
								dat += "Dialysis"
							if(ADSURGERY_BLOOD)
								surgeryqueue["blood"] = 1
								dat += "Blood Transfer"
					if(ORGAN_SURGERY)
						switch(A.surgery_procedure)
							if(ADSURGERY_GERMS)
								surgeryqueue["organgerms"] = 1
								dat += "Organ Infection Treatment"
							if(ADSURGERY_DAMAGE)
								surgeryqueue["organdamage"] = 1
								dat += "Surgical Organ Damage Treatment"
							if(ADSURGERY_EYES)
								surgeryqueue["eyes"] = 1
								dat += "Corrective Eye Surgery"
					if(LIMB_SURGERY)
						switch(A.surgery_procedure)
							if(ADSURGERY_INTERNAL)
								surgeryqueue["internal"] = 1
								dat += "Internal Bleeding Surgery"
							if(ADSURGERY_BROKEN)
								surgeryqueue["broken"] = 1
								dat += "Broken Bone Surgery"
							if(ADSURGERY_MISSING)
								surgeryqueue["missing"] = 1
								dat += "Limb Replacement Surgery"
							if(ADSURGERY_NECRO)
								surgeryqueue["necro"] = 1
								dat += "Necrosis Removal Surgery"
							if(ADSURGERY_SHRAPNEL)
								surgeryqueue["shrapnel"] = 1
								dat += "Foreign Body Removal Surgery"
							if(ADSURGERY_GERM)
								surgeryqueue["limbgerm"] = 1
								dat += "Limb Disinfection Procedure"
							if(ADSURGERY_FACIAL)
								surgeryqueue["facial"] = 1
								dat += "Facial Reconstruction Surgery"
							if(ADSURGERY_OPEN)
								surgeryqueue["open"] = 1
								dat += "Close Open Incision"
				dat += "<br>"

	dat += "<hr> Med-Pod Status: [operating] "
	dat += "<hr><a href='?src=[text_ref(src)];clear=1'>Clear Surgery Queue</a>"
	dat += "<hr><a href='?src=[text_ref(src)];refresh=1'>Refresh Menu</a>"
	dat += "<hr><a href='?src=[text_ref(src)];surgery=1'>Begin Surgery Queue</a>"
	dat += "<hr><a href='?src=[text_ref(src)];ejectify=1'>Eject Patient</a>"
	if(!connected.surgery)
		if(connected.automaticmode)
			dat += "<hr>Manual Surgery Interface Unavaliable, Automatic Mode Engaged."
		else
			dat += "<hr>Manual Surgery Interface<hr>"
			dat += "<b>Trauma Surgeries</b>"
			dat += "<br>"
			if(isnull(surgeryqueue["brute"]))
				dat += "<a href='?src=[text_ref(src)];brute=1'>Surgical Brute Damage Treatment</a><br>"
			if(isnull(surgeryqueue["burn"]))
				dat += "<a href='?src=[text_ref(src)];burn=1'>Surgical Burn Damage Treatment</a><br>"
			dat += "<b>Orthopedic Surgeries</b>"
			dat += "<br>"
			if(isnull(surgeryqueue["broken"]))
				dat += "<a href='?src=[text_ref(src)];broken=1'>Broken Bone Surgery</a><br>"
			if(isnull(surgeryqueue["internal"]))
				dat += "<a href='?src=[text_ref(src)];internal=1'>Internal Bleeding Surgery</a><br>"
			if(isnull(surgeryqueue["shrapnel"]))
				dat += "<a href='?src=[text_ref(src)];shrapnel=1'>Foreign Body Removal Surgery</a><br>"
			if(isnull(surgeryqueue["missing"]))
				dat += "<a href='?src=[text_ref(src)];missing=1'>Limb Replacement Surgery</a><br>"
			dat += "<b>Organ Surgeries</b>"
			dat += "<br>"
			if(isnull(surgeryqueue["organdamage"]))
				dat += "<a href='?src=[text_ref(src)];organdamage=1'>Surgical Organ Damage Treatment</a><br>"
			if(isnull(surgeryqueue["organgerms"]))
				dat += "<a href='?src=[text_ref(src)];organgerms=1'>Organ Infection Treatment</a><br>"
			if(isnull(surgeryqueue["eyes"]))
				dat += "<a href='?src=[text_ref(src)];eyes=1'>Corrective Eye Surgery</a><br>"
			dat += "<b>Hematology Treatments</b>"
			dat += "<br>"
			if(isnull(surgeryqueue["blood"]))
				dat += "<a href='?src=[text_ref(src)];blood=1'>Blood Transfer</a><br>"
			if(isnull(surgeryqueue["toxin"]))
				dat += "<a href='?src=[text_ref(src)];toxin=1'>Toxin Damage Chelation</a><br>"
			if(isnull(surgeryqueue["dialysis"]))
				dat += "<a href='?src=[text_ref(src)];dialysis=1'>Dialysis</a><br>"
			if(isnull(surgeryqueue["necro"]))
				dat += "<a href='?src=[text_ref(src)];necro=1'>Necrosis Removal Surgery</a><br>"
			if(isnull(surgeryqueue["limbgerm"]))
				dat += "<a href='?src=[text_ref(src)];limbgerm=1'>Limb Disinfection Procedure</a><br>"
			dat += "<b>Special Surgeries</b>"
			dat += "<br>"
			if(isnull(surgeryqueue["facial"]))
				dat += "<a href='?src=[text_ref(src)];facial=1'>Facial Reconstruction Surgery</a><br>"
			if(isnull(surgeryqueue["open"]))
				dat += "<a href='?src=[text_ref(src)];open=1'>Close Open Incision</a><br>"

	var/datum/browser/popup = new(user, "autodoc", "<div align='center'>Autodoc Console</div>", 600, 600)
	popup.set_content(dat)
	popup.open()


/obj/machinery/computer/autodoc_console/Topic(href, href_list)
	. = ..()
	if(.)
		return

	if(!connected)
		return

	if(ishuman(connected.occupant))
		// manual surgery handling
		var/datum/data/record/N = null
		for(var/i in GLOB.datacore.medical)
			var/datum/data/record/R = i
			if(R.fields["name"] == connected.occupant.real_name)
				N = R

		if(isnull(N))
			N = create_medical_record(connected.occupant)

		var/needed = 0 // this is to stop someone just choosing everything
		if(href_list["brute"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,EXTERNAL_SURGERY,ADSURGERY_BRUTE)

		if(href_list["burn"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,EXTERNAL_SURGERY,ADSURGERY_BURN)

		if(href_list["toxin"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,EXTERNAL_SURGERY,ADSURGERY_TOXIN)

		if(href_list["dialysis"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,EXTERNAL_SURGERY,ADSURGERY_DIALYSIS)

		if(href_list["blood"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,EXTERNAL_SURGERY,ADSURGERY_BLOOD)

		if(href_list["organgerms"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,ORGAN_SURGERY,ADSURGERY_GERMS)

		if(href_list["eyes"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null,ORGAN_SURGERY,ADSURGERY_EYES,0,connected.occupant.get_organ_slot(ORGAN_SLOT_EYES))

		if(href_list["organdamage"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				for(var/x in L.internal_organs)
					var/datum/internal_organ/I = x
					if(I.robotic == ORGAN_ASSISTED || I.robotic == ORGAN_ROBOT)
						continue
					if(I.damage > 0)
						N.fields["autodoc_manual"] += create_autodoc_surgery(L,ORGAN_SURGERY,ADSURGERY_DAMAGE,0,I)
						needed++
			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,ORGAN_SURGERY,ADSURGERY_DAMAGE,1)

		if(href_list["internal"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(length(L.wounds))
					N.fields["autodoc_manual"] += create_autodoc_surgery(L,LIMB_SURGERY,ADSURGERY_INTERNAL)
					needed++
			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,LIMB_SURGERY,ADSURGERY_INTERNAL,1)

		if(href_list["broken"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(L.limb_status & LIMB_BROKEN)
					N.fields["autodoc_manual"] += create_autodoc_surgery(L,LIMB_SURGERY,ADSURGERY_BROKEN)
					needed++
			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,LIMB_SURGERY,ADSURGERY_BROKEN,1)

		if(href_list["missing"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(L.limb_status & LIMB_DESTROYED && !(L.parent.limb_status & LIMB_DESTROYED) && L.body_part != HEAD)
					N.fields["autodoc_manual"] += create_autodoc_surgery(L,LIMB_SURGERY,ADSURGERY_MISSING)
					needed++
			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,LIMB_SURGERY,ADSURGERY_MISSING,1)

		if(href_list["necro"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(L.limb_status & LIMB_NECROTIZED)
					N.fields["autodoc_manual"] += create_autodoc_surgery(L,LIMB_SURGERY,ADSURGERY_NECRO)
					needed++
			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,LIMB_SURGERY,ADSURGERY_NECRO,1)


		if(href_list["shrapnel"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				var/skip_embryo_check = FALSE
				var/obj/item/alien_embryo/A = locate() in connected.occupant
				for(var/obj/item/embedded AS in L.implants)
					if(embedded.is_beneficial_implant())
						continue
					N.fields["autodoc_manual"] += create_autodoc_surgery(L, LIMB_SURGERY,ADSURGERY_SHRAPNEL)
					needed++
					if(L.body_part == CHEST)
						skip_embryo_check = TRUE
				if(A && L.body_part == CHEST && !skip_embryo_check) //If we're not already doing a shrapnel removal surgery of the chest proceed.
					N.fields["autodoc_manual"] += create_autodoc_surgery(L, LIMB_SURGERY,ADSURGERY_SHRAPNEL)
					needed++

			if(!needed)
				N.fields["autodoc_manual"] += create_autodoc_surgery(null, LIMB_SURGERY,ADSURGERY_SHRAPNEL, 1)

		if(href_list["limbgerm"])
			N.fields["autodoc_manual"] += create_autodoc_surgery(null, LIMB_SURGERY,ADSURGERY_GERM)

		if(href_list["facial"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(!istype(L, /datum/limb/head))
					continue
				var/datum/limb/head/J = L
				if(J.disfigured || J.face_surgery_stage)
					N.fields["autodoc_manual"] += create_autodoc_surgery(L, LIMB_SURGERY,ADSURGERY_FACIAL)
				else
					N.fields["autodoc_manual"] += create_autodoc_surgery(L, LIMB_SURGERY,ADSURGERY_FACIAL, 1)
				break

		if(href_list["open"])
			for(var/i in connected.occupant.limbs)
				var/datum/limb/L = i
				if(L.surgery_open_stage)
					N.fields["autodoc_manual"] += create_autodoc_surgery(L,LIMB_SURGERY,ADSURGERY_OPEN)
					needed++
			if(href_list["open"])
				N.fields["autodoc_manual"] += create_autodoc_surgery(null,LIMB_SURGERY,ADSURGERY_OPEN,1)

		// The rest
		if(href_list["clear"])
			N.fields["autodoc_manual"] = list()

	if(href_list["locktoggle"]) //Toggle the autodoc lock on/off if we have authorization.
		if(allowed(usr))
			locked = !locked
			connected.locked = !connected.locked
		else
			to_chat(usr, span_warning("Access denied."))
			playsound(loc,'sound/machines/buzz-two.ogg', 25, 1)

	if(href_list["noticetoggle"]) //Toggle notifications on/off if we have authorization.
		if(allowed(usr))
			release_notice = !release_notice
		else
			to_chat(usr, span_warning("Access denied."))
			playsound(loc,'sound/machines/buzz-two.ogg', 25, 1)

	if(href_list["automatictoggle"])
		connected.automaticmode = !connected.automaticmode

	if(href_list["surgery"])
		if(connected.occupant)
			connected.surgery_op()

	if(href_list["ejectify"])
		connected.eject()

	updateUsrDialog()


/obj/machinery/autodoc/event
	event = 1

/obj/machinery/computer/autodoc_console/examine(mob/living/user)
	. = ..()
	if(locked)
		. += span_warning("It's currently locked down!")
	if(release_notice)
		. += span_notice("Release notifications are turned on.")

/obj/machinery/autodoc/examine(mob/living/user)
	. = ..()
	to_chat(user, span_notice("Its metal reservoir contains [stored_metal] of [stored_metal_max] units."))
	if(!occupant) //Allows us to reference medical files/scan reports for cryo via examination.
		return
	if(!ishuman(occupant))
		return
	var/active = ""
	if(surgery)
		active += " Surgical procedures are in progress."
	if(!hasHUD(user,"medical"))
		. += span_notice("It contains: [occupant].[active]")
		return
	var/mob/living/carbon/human/H = occupant
	for(var/datum/data/record/R in GLOB.datacore.medical)
		if (!R.fields["name"] == H.real_name)
			continue
		if(!(R.fields["last_scan_time"]))
			. += span_deptradio("No scan report on record")
		else
			. += span_deptradio("<a href='?src=[text_ref(src)];scanreport=1'>It contains [occupant]: Scan from [R.fields["last_scan_time"]].[active]</a>")
		break

/obj/machinery/autodoc/Topic(href, href_list)
	. = ..()
	if(.)
		return
	if (!href_list["scanreport"])
		return
	if(!hasHUD(usr,"medical"))
		return
	if(!ishuman(occupant))
		return
	var/mob/living/carbon/human/H = occupant
	for(var/datum/data/record/R in GLOB.datacore.medical)
		if (!R.fields["name"] == H.real_name)
			continue
		if(R.fields["last_scan_time"] && R.fields["last_scan_result"])
			var/datum/browser/popup = new(usr, "scanresults", "<div align='center'>Last Scan Result</div>", 430, 600)
			popup.set_content(R.fields["last_scan_result"])
			popup.open(FALSE)
		break













/// Gibs the occupant as well if a shuttle crashes/crushes/qdels this machine.
/obj/machinery/autodoc/proc/on_shuttle_crush()
	SIGNAL_HANDLER
	if(!occupant)
		return

	var/mob/living/carbon/human/human_occupant = occupant
	eject_occupant()
	human_occupant.gib()

/// Handles ejection of the occupants when done in an unexpected matter.
/obj/machinery/autodoc/proc/eject_occupant_unexpected(mob/living/ejector)
	if(!occupant)
		return

	// Xenos can eject people who are sitting in the autodoc.
	if(isxeno(ejector))
		visible_message(span_warning("[ejector] begins to pry the [src]'s cover!"), 3)
		playsound(src,'sound/effects/metal_creaking.ogg', 25, 1)
		if(!do_after(ejector, 2 SECONDS))
			return
		if(!active_surgery)
			handle_release_notice(AUTODOC_NOTICE_XENO_FUCKERY)
			eject_occupant()
			return
		visible_message("\The [src] malfunctions as [ejector] aborts the surgery in progress.")
		occupant.take_limb_damage(rand(30, 50), rand(30, 50))
		handle_release_notice(AUTODOC_NOTICE_IDIOT_EJECT)
		eject_occupant()
		return

	if(!ishuman(ejector))
		return

	/// Occupants are stuck inside if they are in the middle of surgery.
	if(ejector == occupant)
		if(active_surgery)
			to_chat(ejector, span_warning("There's no way you're getting out while this thing is operating on you!"))
			return
		visible_message("[ejector] engages the internal release mechanism, and climbs out of \the [src].")
		eject_occupant()
		return

	var/is_trained = ejector.skills.getRating(SKILL_SURGERY) >= SKILL_SURGERY_TRAINED
	if(is_trained)
		eject_occupant()
		return

	ejector.visible_message(
		span_notice("[ejector] fumbles around figuring out how to use [src]."),
		span_notice("You fumble around figuring out how to use [src].")
	)
	// 8 seconds @ untrained (0); 5 seconds @ amateur (1).
	var/fumbling_time = max(0, SKILL_TASK_TOUGH - ( SKILL_TASK_EASY * ejector.skills.getRating(SKILL_SURGERY)))
	if(!do_after(ejector, fumbling_time, NONE, src, BUSY_ICON_UNSKILLED) || !occupant)
		return

	// They also damage the occupant if they were in the middle of the surgery.
	if(active_surgery)
		visible_message("\The [src] malfunctions as [ejector] aborts the surgery in progress.")
		occupant.take_limb_damage(rand(30, 50), rand(30, 50))
		log_game("[key_name(ejector)] ejected [key_name(occupant)] from the autodoc during surgery causing damage.")
		message_admins("[ADMIN_TPMONTY(ejector)] ejected [ADMIN_TPMONTY(occupant)] from the autodoc during surgery causing damage.")
		handle_release_notice(AUTODOC_NOTICE_IDIOT_EJECT)
	eject_occupant()

/// Handles all checks, insertion, and more regarding a new occupant.
/obj/machinery/autodoc/proc/insert_occupant(mob/living/future_occupant, mob/inserter)
	if(!ishuman(future_occupant) || !ishuman(inserter) || inserter.incapacitated(TRUE))
		return

	if(occupant)
		to_chat(inserter, span_notice("[src] is already occupied!"))
		return
	if(machine_stat & (NOPOWER|BROKEN))
		to_chat(inserter, span_notice("[src] is non-functional!"))
		return

	if(inserter.skills.getRating(SKILL_SURGERY) < SKILL_SURGERY_TRAINED)
		future_occupant.visible_message(
			span_notice("[future_occupant] fumbles around figuring out how to get into \the [src]."),
			span_notice("You fumble around figuring out how to get into \the [src].")
		)
		// 8 seconds @ untrained (0); 5 seconds @ amateur (1).
		var/fumbling_time = max(0, SKILL_TASK_TOUGH - ( SKILL_TASK_EASY * inserter.skills.getRating(SKILL_SURGERY)))
		if(!do_after(future_occupant, fumbling_time, NONE, src, BUSY_ICON_UNSKILLED))
			return

	future_occupant.visible_message(
		span_notice("[future_occupant] starts climbing into \the [src]."),
		span_notice("You start climbing into \the [src].")
	)

	if(!do_after(future_occupant, 1 SECONDS, IGNORE_HELD_ITEM, src, BUSY_ICON_GENERIC))
		return

	if(occupant) // Checking again since something could of changed since then.
		to_chat(inserter, span_notice("[src] is already occupied!"))
		return
	if(machine_stat & (NOPOWER|BROKEN))
		to_chat(inserter, span_notice("[src] is non-functional!"))
		return

	if(inserter.pulling == future_occupant) // In case they were pulling someone else instead.
		inserter.stop_pulling()
	future_occupant.forceMove(src)
	occupant = future_occupant
	update_icon()

	start_processing()
	if(automatic_mode)
		say("Automatic mode engaged, initialising procedures.")
		addtimer(CALLBACK(src, PROC_REF(begin_automatic_surgery)), 5 SECONDS)

/// Ejects the occupant and resets the autodoc to a normal state.
/obj/machinery/autodoc/proc/eject_occupant()
	for(var/atom_content in contents)
		var/atom/movable/atom_movable = atom_content
		atom_movable.forceMove(loc)

	occupant = null
	active_surgery = FALSE
	surgery_todo_list = list()
	if(surgery_loop_timer_id)
		deltimer(surgery_loop_timer_id)
		surgery_loop_timer_id = null
	update_icon()
	stop_processing()

/// Begins the surgery.
/obj/machinery/autodoc/proc/begin_surgery()
	if(active_surgery)
		return

	if(QDELETED(occupant) || occupant.stat == DEAD)
		if(!ishuman(occupant))
			stack_trace("Non-human occupant made its way into the autodoc: [occupant] | [occupant?.type].")
		visible_message("[src] buzzes.")
		handle_release_notice(AUTODOC_NOTICE_DEATH)
		eject_occupant()
		return

	med_scan(occupant, "", GLOB.known_implants)

	var/datum/data/record/occupant_record = null
	for(var/datum/data/record/record in GLOB.datacore.medical)
		if(record.fields["name"] == occupant.real_name)
			occupant_record = record
	if(isnull(occupant_record))
		visible_message("[src] buzzes: No records found for occupant.")
		handle_release_notice(AUTODOC_NOTICE_NO_RECORD)
		eject_occupant()
		return

	queued_surgeries = automatic_mode ? occupant_record.fields["autodoc_data"] : occupant_record.fields["autodoc_manual"]
	if(!length(queued_surgeries))
		visible_message("[src] buzzes, no surgical procedures were queued.")
		return

	// Surgeries that are handled in process() and doesn't need the surgery loop to function.
	for(var/datum/autodoc_surgery/next_surgery in queued_surgeries)
		if(next_surgery.surgery_type != AUTODOC_SURGERY_TYPE_EXTERNAL)
			continue
		switch(next_surgery.surgery_procedure)
			if(AUTODOC_SURGERY_PROCEDURE_BRUTE_DAMAGE)
				heal_brute = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_BURN_DAMAGE)
				heal_burn = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_TOXIN_DAMAGE)
				heal_toxin = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_DIALYSIS)
				filtering = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_LOW_BLOOD)
				blood_transfer = TRUE
		queued_surgeries -= next_surgery

	visible_message("[src] begins to operate, the pod locking shut with a loud click.")
	active_surgery = TRUE
	update_icon()
	surgery_loop()

/// Callback to start surgery when someone entered while automatic mode is on.
/obj/machinery/autodoc/proc/begin_automatic_surgery()
	if(active_surgery)
		return
	if(!occupant)
		say("Occupant missing, procedures canceled.")
	if(!automatic_mode)
		say("Automatic mode disengaged, awaiting manual inputs.")
		return
	begin_surgery()

/// Sets and begins the surgery loop timer.
/obj/machinery/autodoc/proc/set_surgery_loop_timer(miliseconds = 1 SECONDS)
	surgery_loop_timer_id = addtimer(CALLBACK(src, PROC_REF(surgery_loop)), miliseconds, TIMER_STOPPABLE)

/obj/machinery/autodoc/proc/try_incision_manager(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	// The 1st step of many surgeries.
	if(limb_ref.surgery_open_stage < 2)
		if(istype(limb_owner) && !(limb_owner.species.species_flags & NO_BLOOD))
			limb_ref.add_limb_flags(LIMB_BLEEDING)

		limb_ref.createwound(CUT, 1)
		limb_ref.clamp_bleeder()
		limb_ref.surgery_open_stage = 2
		limb_owner.updatehealth()
		return TRUE
	// The 2nd step for necro removal surgery.
	if(!limb_ref.necro_surgery_stage)
		limb_ref.necro_surgery_stage = 1
		return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_circular_saw(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	// The 2nd step of larva removal / organ damage repair.
	if(limb_ref.surgery_open_stage == 2)
		limb_ref.surgery_open_stage = 2.5
		return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_retractor(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	switch(limb_ref.surgery_open_stage)
		// The 3rd step of larva removal / organ damage repair.
		if(2.5)
			limb_ref.surgery_open_stage = 3
			return TRUE
		// The 5th step of larva removal / organ damage repair.
		if(3)
			limb_ref.surgery_open_stage = 2.5
			return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_bonegel(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	switch(limb_ref.surgery_open_stage)
		// The 2nd step of fracture repair.
		if(2)
			if(!limb_ref.bone_repair_stage)
				limb_ref.bone_repair_stage = 1
				return TRUE
		// The 6th step of larva removal / organ damage repair.
		if(2.5)
			limb_ref.surgery_open_stage = 2
			return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_bonesetter(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	// The 3rd step of fracture repair.
	if(limb_ref.surgery_open_stage == 2 && limb_ref.bone_repair_stage)
		// We do not want it to fracture immediately after it is fixed.
		if(limb_ref.brute_dam > 20)
			limb_ref.heal_limb_damage(limb_ref.brute_dam - 20)
			limb_owner.updatehealth()
		limb_ref.remove_limb_flags(LIMB_BROKEN | LIMB_SPLINTED | LIMB_STABILIZED)
		limb_ref.add_limb_flags(LIMB_REPAIRED)
		limb_ref.bone_repair_stage = 0
		return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_cautery(mob/living/carbon/human/limb_owner, datum/limb/limb_ref)
	if(!limb_ref.surgery_open_stage)
		return FALSE
	// The last step of many surgeries.
	if(limb_ref.surgery_open_stage <= 2)
		limb_ref.germ_level = 0
		limb_ref.remove_limb_flags(LIMB_BLEEDING)
		limb_ref.surgery_open_stage = 0
		limb_owner.updatehealth()
		return TRUE
	return FALSE

/obj/machinery/autodoc/proc/try_surgical_membrane(mob/living/carbon/human/limb_owner, datum/limb/limb_ref, datum/internal_organ/organ_ref)
	if(limb_ref.body_part != GROIN && limb_ref.surgery_open_stage == 3 && organ_ref)
		organ_ref.heal_organ_damage(organ_ref.damage)
		return TRUE
	return FALSE

surgical_membrane
/// Goes through the next surgery step infinitely (by calling itself with a timer) until there is nothing else to do.
/obj/machinery/autodoc/proc/surgery_loop()
	if(surgery_loop_timer_id)
		deltimer(surgery_loop_timer_id)
		surgery_loop_timer_id = null

	if(QDELETED(occupant) || occupant.stat == DEAD)
		return

	// Get and set the next queued surgery as our current one if we don't have one right now.
	if(!current_surgery && length(queued_surgeries))
		for(var/datum/autodoc_surgery/next_surgery in queued_surgeries)
			current_surgery = next_surgery
			queued_surgeries -= current_surgery
			break

	// No surgeries to do. End it if we're waiting on nothing else.
	if(!current_surgery)
		// Waiting on process() to tell us when everything is done.
		if(filtering || blood_transfer || heal_brute || heal_burn || heal_toxin)
			set_surgery_loop_timer(2 SECONDS)
			return
		visible_message("\The [src] clicks and opens up having finished the requested operations.")
		handle_release_notice(AUTODOC_NOTICE_SUCCESS)
		eject_occupant()
		return

	// External surgeries should of been handled in `begin_surgery()`.
	// This is here to prevent it from being stuck looping forever if something did go wrong.
	if(current_surgery.surgery_type == AUTODOC_SURGERY_TYPE_EXTERNAL)
		switch(current_surgery.surgery_procedure)
			if(AUTODOC_SURGERY_PROCEDURE_BRUTE_DAMAGE)
				heal_brute = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_BURN_DAMAGE)
				heal_burn = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_TOXIN_DAMAGE)
				heal_toxin = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_DIALYSIS)
				filtering = TRUE
			if(AUTODOC_SURGERY_PROCEDURE_LOW_BLOOD)
				blood_transfer = TRUE
		current_surgery = null
		surgery_loop()
		return

	var/datum/limb/surgeried_limb = current_surgery.limb_ref
	var/datum/internal_organ/surgeried_organ = current_surgery.organ_ref

	// Disinfecting limbs & organs
	if(current_surgery.surgery_procedure == AUTODOC_SURGERY_PROCEDURE_INFECTED_GERMS)
		say(current_surgery.surgery_type == AUTODOC_SURGERY_TYPE_ORGAN ? "Beginning organ disinfection." : "Beginning limb disinfection.")
		if(current_surgery.surgery_unnecessary)
			say("Procedure has been deemed unnecessary.")
			current_surgery = null
			set_surgery_loop_timer(AUTODOC_SURGERY_UNNEEDED_DELAY)
			return
		var/datum/reagent/spaceacillin_reagent = GLOB.chemical_reagents_list[/datum/reagent/medicine/spaceacillin]
		var/amount_remaining = (spaceacillin_reagent.overdose_threshold * 0.5) - occupant.reagents.get_reagent_amount(/datum/reagent/medicine/spaceacillin)
		var/inject_per_second = 3
		if(amount_remaining > inject_per_second)
			occupant.reagents.add_reagent(/datum/reagent/medicine/spaceacillin, inject_per_second)
			set_surgery_loop_timer(1 SECONDS)
			return
		occupant.reagents.add_reagent(/datum/reagent/medicine/spaceacillin, amount_remaining)
		current_surgery = null
		set_surgery_loop_timer(1 SECONDS)
		return

	// Organ surgeries
	if(current_surgery.surgery_type == AUTODOC_SURGERY_TYPE_ORGAN)
		switch(current_surgery.surgery_procedure)
			if(AUTODOC_SURGERY_PROCEDURE_EYE_DAMAGE)
				say("Beginning corrective eye surgery.")
				if(current_surgery.surgery_unnecessary)
					say("Procedure has been deemed unnecessary.")
					current_surgery = null
					set_surgery_loop_timer(AUTODOC_SURGERY_UNNEEDED_DELAY)
					return
				if(!istype(surgeried_organ, /datum/internal_organ/eyes))
					current_surgery = null
					set_surgery_loop_timer(1 SECONDS)
					return
				// This is the only surgery that doesn't use incision manager.
				var/datum/internal_organ/eyes/surgeried_eyes = surgeried_organ
				switch(surgeried_eyes.eye_surgery_stage)
					if(0)
						if(!surgeried_organ.damage)
							current_surgery = null
							set_surgery_loop_timer(1 SECONDS)
							return
						surgeried_eyes.eye_surgery_stage = 1
						occupant.disabilities |= NEARSIGHTED
						set_surgery_loop_timer(EYE_CUT_MAX_DURATION)
						return
					if(1)
						surgeried_eyes.eye_surgery_stage = 2
						set_surgery_loop_timer(EYE_LIFT_MAX_DURATION)
						return
					if(2)
						surgeried_eyes.eye_surgery_stage = 3
						set_surgery_loop_timer(EYE_MEND_MAX_DURATION)
						return
					if(3)
						occupant.disabilities &= ~NEARSIGHTED
						occupant.disabilities &= ~BLIND
						surgeried_eyes.heal_organ_damage(surgeried_eyes.damage)
						surgeried_eyes.eye_surgery_stage = 0
						set_surgery_loop_timer(EYE_CAUTERISE_MAX_DURATION)
						return
			if(AUTODOC_SURGERY_PROCEDURE_ORGAN_REPAIR)
				say("Beginning organ restoration.")
				if(current_surgery.surgery_unnecessary)
					say("Procedure has been deemed unnecessary.")
					current_surgery = null
					set_surgery_loop_timer(AUTODOC_SURGERY_UNNEEDED_DELAY)
					return
				if(!surgeried_organ)
					say("Organ is missing.")
					current_surgery = null
					set_surgery_loop_timer(1 SECONDS)
					return
				if(surgeried_organ.damage)
					if(try_incision_manager(occupant, surgeried_limb))
						set_surgery_loop_timer(INCISION_MANAGER_MAX_DURATION)
						return
					if(surgeried_limb.body_part != GROIN)
						if(try_circular_saw(occupant, surgeried_limb))
							set_surgery_loop_timer(SAW_OPEN_ENCASED_MAX_DURATION)
							return
						if(try_retractor(occupant, surgeried_limb))
							set_surgery_loop_timer(RETRACT_OPEN_ENCASED_MAX_DURATION)
							return
					if(try_retractor(occupant, surgeried_limb))
						set_surgery_loop_timer(RETRACT_OPEN_ENCASED_MAX_DURATION)
						return

					surgeried_organ.heal_organ_damage(surgeried_organ.damage)
					set_surgery_loop_timer(FIX_ORGAN_MAX_DURATION)
					return
				if(surgeried_limb.body_part != GROIN)
					if(try_retractor(occupant, surgeried_organ))
						set_surgery_loop_timer(RETRACT_OPEN_ENCASED_MAX_DURATION)
						return
					if(try_bonegel(occupant, surgeried_organ))
						set_surgery_loop_timer(BONEGEL_CLOSE_ENCASED_MAX_DURATION)
						return
				if(try_cautery(occupant, surgeried_organ))
					set_surgery_loop_timer(CAUTERY_MAX_DURATION)
					return
				current_surgery = null
				set_surgery_loop_timer(1 SECONDS)
				return

	// Limb surgeries
	if(current_surgery.surgery_type == AUTODOC_SURGERY_TYPE_LIMB)
		switch(current_surgery.surgery_procedure)
			if(AUTODOC_SURGERY_PROCEDURE_INTERNAL_BLEEDING)
				say("Beginning internal bleeding procedure.")
				if(current_surgery.surgery_unnecessary)
					say("Procedure has been deemed unnecessary.")
					current_surgery = null
					set_surgery_loop_timer(AUTODOC_SURGERY_UNNEEDED_DELAY)
					return
				var/list/datum/wound/limb_wounds = current_surgery.limb_ref.wounds
				if(length(limb_wounds))
					if(try_incision_manager(occupant, surgeried_limb))
						set_surgery_loop_timer(INCISION_MANAGER_MAX_DURATION)
						return
					for(var/datum/wound/limb_wound in limb_wounds)
						set_surgery_loop_timer(FIXVEIN_MAX_DURATION)
						qdel(limb_wound)
						return
				if(try_cautery(occupant, surgeried_organ))
					set_surgery_loop_timer(CAUTERY_MAX_DURATION)
					return
				current_surgery = null
				set_surgery_loop_timer(1 SECONDS)
				return
			if(AUTODOC_SURGERY_PROCEDURE_FRACTURED_BONE)
				say("Beginning broken bone procedure.")
				if(current_surgery.surgery_unnecessary)
					say("Procedure has been deemed unnecessary.")
					current_surgery = null
					set_surgery_loop_timer(AUTODOC_SURGERY_UNNEEDED_DELAY)
					return
				var/is_broken = surgeried_limb.limb_status & (LIMB_BROKEN|LIMB_SPLINTED|LIMB_STABILIZED)
				if(is_broken)
					if(try_incision_manager(occupant, surgeried_limb))
						set_surgery_loop_timer(INCISION_MANAGER_MAX_DURATION)
						return
					if(try_bonegel(occupant, surgeried_organ))
						set_surgery_loop_timer(BONEGEL_CLOSE_ENCASED_MAX_DURATION)
						return
					if(try_bonesetter(occupant, surgeried_organ))
						set_surgery_loop_timer(BONESETTER_MAX_DURATION)
						return
				if(try_cautery(occupant, surgeried_organ))
					set_surgery_loop_timer(CAUTERY_MAX_DURATION)
					return
				current_surgery = null
				set_surgery_loop_timer(1 SECONDS)
				return

	// Debugging:
	to_chat(world, span_boldannounce("A surgery was not accounted for! Surgery Type: [current_surgery.surgery_type], Surgery Procedure: [current_surgery.surgery_procedure]"))
	current_surgery = null
	set_surgery_loop_timer(1 SECONDS)
	return

/*
		switch(S.type_of_surgery)
			if(LIMB_SURGERY)
				switch(S.surgery_procedure)
					if(ADSURGERY_BROKEN)
						say("Beginning broken bone procedure.")
						if(S.unneeded)
							sleep(UNNEEDED_DELAY)
							say("Procedure has been deemed unnecessary.")
							surgery_todo_list -= S
							continue
						open_incision(occupant, S.limb_ref)
						sleep(BONEGEL_REPAIR_MAX_DURATION*surgery_mod)
						sleep(BONESETTER_MAX_DURATION*surgery_mod)
						if(S.limb_ref.brute_dam > 20)
							sleep(((S.limb_ref.brute_dam - 20)/2)*surgery_mod)
							if(!surgery)
								break
							S.limb_ref.heal_limb_damage(S.limb_ref.brute_dam - 20)
						if(!surgery)
							break
						S.limb_ref.remove_limb_flags(LIMB_BROKEN | LIMB_SPLINTED | LIMB_STABILIZED)
						S.limb_ref.add_limb_flags(LIMB_REPAIRED)
						close_incision(occupant, S.limb_ref)

					if(ADSURGERY_MISSING)
						say("Beginning limb replacement.")
						if(S.unneeded)
							sleep(UNNEEDED_DELAY)
							say("Procedure has been deemed unnecessary.")
							surgery_todo_list -= S
							continue

						sleep(ROBOLIMB_CUT_MAX_DURATION*surgery_mod)
						sleep(ROBOLIMB_MEND_MAX_DURATION*surgery_mod)
						sleep(ROBOLIMB_PREPARE_MAX_DURATION*surgery_mod)

						if(stored_metal < LIMB_METAL_AMOUNT)
							say("Metal reserves depleted.")
							playsound(loc, 'sound/machines/buzz-two.ogg', 15, TRUE)
							surgery_todo_list -= S
							continue // next surgery

						stored_metal -= LIMB_METAL_AMOUNT

						if(S.limb_ref.parent.limb_status & LIMB_DESTROYED) // there's nothing to attach to
							say("Limb attachment failed.")
							playsound(loc, 'sound/machines/buzz-two.ogg', 15, TRUE)
							surgery_todo_list -= S
							continue

						if(!surgery)
							break
						S.limb_ref.add_limb_flags(LIMB_AMPUTATED)
						S.limb_ref.setAmputatedTree()
						S.limb_ref.limb_replacement_stage = 0

						var/spillover = LIMB_PRINTING_TIME - (ROBOLIMB_PREPARE_MAX_DURATION+ROBOLIMB_MEND_MAX_DURATION+ROBOLIMB_CUT_MAX_DURATION)
						if(spillover > 0)
							sleep(spillover*surgery_mod)

						sleep(ROBOLIMB_ATTACH_MAX_DURATION*surgery_mod)
						if(!surgery)
							break
						S.limb_ref.robotize()
						occupant.update_body()
						occupant.updatehealth()
						occupant.UpdateDamageIcon()

					if(ADSURGERY_NECRO)
						say("Beginning necrotic tissue removal.")
						if(S.unneeded)
							sleep(UNNEEDED_DELAY)
							say("Procedure has been deemed unnecessary.")
							surgery_todo_list -= S
							continue

						open_incision(occupant, S.limb_ref)
						sleep(NECRO_REMOVE_MAX_DURATION*surgery_mod)
						sleep(NECRO_TREAT_MAX_DURATION*surgery_mod)
						S.limb_ref.remove_limb_flags(LIMB_NECROTIZED)
						occupant.update_body()

						close_incision(occupant, S.limb_ref)

					if(ADSURGERY_SHRAPNEL)
						say("Beginning foreign body removal.")
						if(S.unneeded)
							sleep(UNNEEDED_DELAY)
							say("Procedure has been deemed unnecessary.")
							surgery_todo_list -= S
							continue

						open_incision(occupant, S.limb_ref)
						if(S.limb_ref.body_part == CHEST || S.limb_ref.body_part == HEAD)
							open_encased(occupant, S.limb_ref)
						if(S.limb_ref.body_part == CHEST) //if it's the chest check for gross parasites
							var/obj/item/alien_embryo/A = locate() in occupant
							if(A)
								for(A in occupant)
									sleep(HEMOSTAT_REMOVE_MAX_DURATION*surgery_mod)
									occupant.visible_message(span_warning(" [src] defty extracts a wriggling parasite from [occupant]'s ribcage!"))
									var/mob/living/carbon/xenomorph/larva/L = locate() in occupant //the larva was fully grown, ready to burst.
									if(L)
										L.forceMove(get_turf(src))
									else
										A.forceMove(occupant.loc)
										occupant.status_flags &= ~XENO_HOST
									qdel(A)
						if(length(S.limb_ref.implants))
							for(var/obj/item/embedded AS in S.limb_ref.implants)
								if(!surgery)
									break
								if(embedded.is_beneficial_implant())
									continue
								sleep(HEMOSTAT_REMOVE_MAX_DURATION*surgery_mod)
								embedded.unembed_ourself(TRUE)
						if(S.limb_ref.body_part == CHEST || S.limb_ref.body_part == HEAD)
							close_encased(occupant, S.limb_ref)
						if(!surgery)
							break
						close_incision(occupant, S.limb_ref)

					if(ADSURGERY_FACIAL) // dumb but covers for incomplete facial surgery
						say("Beginning Facial Reconstruction Surgery.")
						if(S.unneeded)
							sleep(UNNEEDED_DELAY)
							say("Procedure has been deemed unnecessary.")
							surgery_todo_list -= S
							continue
						if(istype(S.limb_ref,/datum/limb/head))
							var/datum/limb/head/F = S.limb_ref
							if(F.face_surgery_stage == 0)
								sleep(FACIAL_CUT_MAX_DURATION)
								if(!surgery)
									break
								F.face_surgery_stage = 1
							if(F.face_surgery_stage == 1)
								sleep(FACIAL_MEND_MAX_DURATION)
								if(!surgery)
									break
								F.face_surgery_stage = 2
							if(F.face_surgery_stage == 2)
								sleep(FACIAL_FIX_MAX_DURATION)
								if(!surgery)
									break
								F.face_surgery_stage = 3
							if(F.face_surgery_stage == 3)
								sleep(FACIAL_CAUTERISE_MAX_DURATION)
								if(!surgery)
									break
								F.remove_limb_flags(LIMB_BLEEDING)
								F.disfigured = 0
								F.owner.name = F.owner.get_visible_name()
								F.face_surgery_stage = 0

					if(ADSURGERY_OPEN)
						say("Closing surgical incision.")
						close_encased(occupant, S.limb_ref)
						close_incision(occupant, S.limb_ref)

		say("Procedure complete.")
		surgery_todo_list -= S
		continue

	while(heal_brute||heal_burn||heal_toxin||filtering||blood_transfer)
		if(!surgery)
			break
		sleep(2 SECONDS)
		if(prob(5))
			visible_message("[src] beeps as it continues working.")

	visible_message("\The [src] clicks and opens up having finished the requested operations.")
	surgery = 0
	go_out(AUTODOC_NOTICE_SUCCESS)
	*/
	return

/// Notifies medical about occupant's success/failure if there is a connected computer with notices enabled.
/obj/machinery/autodoc/proc/handle_release_notice(notice_code)
	if(!connected?.release_notice || !occupant)
		return

	var/reason = "Reason for discharge: Procedural completion."
	var/sound = "sound/machines/ping.ogg"
	switch(notice_code)
		if(AUTODOC_NOTICE_DEATH)
			reason = "Reason for discharge: Patient death."
			sound = "sound/machines/warning-buzzer.ogg"
		if(AUTODOC_NOTICE_NO_RECORD)
			reason = "Reason for discharge: Medical records not detected. Alerting security advised."
			sound = "sound/machines/warning-buzzer.ogg"
		if(AUTODOC_NOTICE_NO_POWER)
			reason = "Reason for discharge: Power failure."
			sound = "sound/machines/warning-buzzer.ogg"
		if(AUTODOC_NOTICE_XENO_FUCKERY)
			reason = "Reason for discharge: Unauthorized manual release. Alerting security advised."
			sound = "sound/machines/warning-buzzer.ogg"
		if(AUTODOC_NOTICE_IDIOT_EJECT)
			reason = "Reason for discharge: Unauthorized manual release during surgery. Alerting security advised."
			sound = "sound/machines/warning-buzzer.ogg"
		if(AUTODOC_NOTICE_FORCE_EJECT)
			reason = "Reason for discharge: Destruction of linked Autodoc Medical System. Alerting security advised."
			sound = "sound/machines/warning-buzzer.ogg"
	connected.radio.talk_into(src, "<b>Patient: [occupant] has been released from [src] at: [get_area(src)]. [reason]</b>", RADIO_CHANNEL_MEDICAL)
