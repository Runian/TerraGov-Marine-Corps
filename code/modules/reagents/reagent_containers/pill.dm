////////////////////////////////////////////////////////////////////////////////
/// Pills.
////////////////////////////////////////////////////////////////////////////////

/obj/item/reagent_containers/pill
	name = "pill"
	icon = 'icons/obj/items/chemistry.dmi'
	icon_state = "pill1"
	worn_icon_state = "pill"
	possible_transfer_amounts = null
	amount_per_transfer_from_this = 15
	reagent_flags = AMOUNT_SKILLCHECK
	w_class = WEIGHT_CLASS_TINY
	volume = 60
	attack_speed = 1 //War against input locking while pill munching
	var/pill_desc = "An unknown pill." //the real description of the pill, shown when examined by a medically trained person
	var/pill_id

/obj/item/reagent_containers/pill/Initialize(mapload)
	. = ..()
	if(icon_state == "pill1")
		icon_state = pill_id ? GLOB.randomized_pill_icons[pill_id] : pick(GLOB.randomized_pill_icons)

/obj/item/reagent_containers/pill/attack_self(mob/user)
	. = ..()
	if(.)
		return TRUE
	user.changeNext_move(attack_speed)
	return try_consume(user, user, null)

/obj/item/reagent_containers/pill/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!check_allowed_items(interacting_with, target_self = TRUE))
		return NONE

	if(ismob(interacting_with))
		return try_consume(interacting_with, user, null)
	if(interacting_with.is_refillable())
		return try_refill(interacting_with, user, null)

	return NONE

/obj/item/reagent_containers/pill/try_consume(atom/target, mob/living/user, transfer_amount = null)
	if(!can_consume(target)) // Will give varied chat message if it fails.
		return ITEM_INTERACT_BLOCKING

	user.changeNext_move(CLICK_CD_MELEE)
	if(target == user)
		to_chat(user, span_green("You swallow [src]."))
	else
		target.visible_message(
			span_danger("[user] attempts to force [target] to swallow [src]."),
			span_userdanger("[user] attempts to force you to swallow [src]."),
		)
		var/ingestion_time = max(1 SECONDS, 3 SECONDS - 1 SECONDS * user.skills.getRating(SKILL_MEDICAL))
		if(!do_after(user, ingestion_time, NONE, target, BUSY_ICON_FRIENDLY, BUSY_ICON_MEDICAL))
			return ITEM_INTERACT_BLOCKING
		if(!reagents || !reagents.total_volume)
			return ITEM_INTERACT_BLOCKING

		target.visible_message(
			span_danger("[user] forces [target] to swallow [src]."),
			span_userdanger("[user] forces you to swallow [src]."),
		)
		if(is_blind(target))
			to_chat(target, span_notice("You feel someone feed you something."))
		log_combat(user, target, "fed", src, "Reagents: [get_reagent_list_text()]")

	record_reagent_consumption(min(transfer_amount, reagents.total_volume), reagents.reagent_list, user, target)
	if(reagents.total_volume)
		reagents.reaction(target, INGEST)
		reagents.trans_to(target, reagents.total_volume)
	qdel(src)

	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_containers/pill/try_refill(atom/target, mob/living/user, transfer_amount = amount_per_transfer_from_this)
	if(!reagents.total_volume)
		to_chat(user, span_warning("[src] is empty!"))
		return ITEM_INTERACT_BLOCKING
	if(target.reagents.holder_full())
		to_chat(user, span_warning("[target] is full."))
		return ITEM_INTERACT_BLOCKING

	var/can_freely_liquidate = FALSE
	if(istype(target, /obj/item/reagent_containers))
		var/obj/item/reagent_containers/reagent_liquiater = target
		if(reagent_liquiater.liquifier)
			can_freely_liquidate = TRUE

	if(target.is_drainable() && !target.reagents.total_volume)
		if(!can_freely_liquidate)
			to_chat(user, span_warning("[target] is empty! There's nothing to dissolve [src] in."))
			return
		to_chat(user, span_notice("[target]'s liquifier instantly reprocesses [src] upon insertion."))
	to_chat(user, span_notice("You dissolve the pill in [target]."))

	var/rgt_list_text = get_reagent_list_text()
	log_combat(user, target, "spiked", src, "Reagents: [rgt_list_text]")
	reagents.trans_to(target, reagents.total_volume)
	visible_message(
		span_warning("[user] puts something in \the [target]."),
		vision_distance = 2
	)
	qdel(src)
	return ITEM_INTERACT_SUCCESS

/obj/item/reagent_containers/pill/is_refuelable()
	return FALSE

////////////////////////////////////////////////////////////////////////////////
/// Pills. END
////////////////////////////////////////////////////////////////////////////////

//Pills
/obj/item/reagent_containers/pill/dylovene
	pill_desc = "A dylovene pill. It neutralizes many common toxins."
	list_reagents = list(/datum/reagent/medicine/dylovene = 15)
	pill_id = 1

/obj/item/reagent_containers/pill/tox
	pill_desc = "A toxins pill. It's highly toxic."
	list_reagents = list(/datum/reagent/toxin = 50)
	pill_id = 2

/obj/item/reagent_containers/pill/cyanide
	desc = "A cyanide pill. Don't swallow this!"
	pill_desc = null//so even non medics can see what this pill is.
	list_reagents = list(/datum/reagent/toxin/cyanide = 50)
	pill_id = 2

/obj/item/reagent_containers/pill/adminordrazine
	pill_desc = "An adminordrazine pill. It's magic. We don't have to explain it."
	list_reagents = list(/datum/reagent/medicine/adminordrazine = 50)
	pill_id = 3

/obj/item/reagent_containers/pill/sleeptoxin
	pill_desc = "A sleeping pill commonly used to treat insomnia."
	list_reagents = list(/datum/reagent/toxin/sleeptoxin = 15)
	pill_id = 4

/obj/item/reagent_containers/pill/kelotane
	pill_desc = "A kelotane pill. Used to treat burns."
	list_reagents = list(/datum/reagent/medicine/kelotane = 15)
	pill_id = 5

/obj/item/reagent_containers/pill/dermaline
	pill_desc = "A dermaline pill. Heals burn damage at a higher rate than kelotane."
	list_reagents = list(/datum/reagent/medicine/dermaline = 7.5)
	pill_id = 5

/obj/item/reagent_containers/pill/paracetamol
	pill_desc = "A paracetamol pill. Painkiller for the ages."
	list_reagents = list(/datum/reagent/medicine/paracetamol = 15)
	pill_id = 6

/obj/item/reagent_containers/pill/tramadol
	pill_desc = "A tramadol pill. A simple painkiller."
	list_reagents = list(/datum/reagent/medicine/tramadol = 15)
	pill_id = 7

/obj/item/reagent_containers/pill/isotonic
	pill_desc = "A pill with an isotonic solution inside. Used to stimulate blood regeneration."
	list_reagents = list(/datum/reagent/medicine/saline_glucose = 15)
	pill_id = 4

/obj/item/reagent_containers/pill/inaprovaline
	pill_desc = "An inaprovaline pill. Used to stabilize patients."
	list_reagents = list(/datum/reagent/medicine/inaprovaline = 15)
	pill_id = 10

/obj/item/reagent_containers/pill/dexalin
	pill_desc = "A dexalin pill. Used to treat oxygen deprivation."
	list_reagents = list(/datum/reagent/medicine/dexalin = 15)
	pill_id = 11

/obj/item/reagent_containers/pill/spaceacillin
	pill_desc = "A spaceacillin pill. Used to treat infected wounds and slow down viral infections."
	list_reagents = list(/datum/reagent/medicine/spaceacillin = 10)
	pill_id = 12

/obj/item/reagent_containers/pill/happy
	pill_desc = "A Happy Pill! Happy happy joy joy!"
	list_reagents = list(/datum/reagent/space_drugs = 15, /datum/reagent/consumable/sugar = 15, /datum/reagent/consumable/laughter = 5)
	pill_id = 13

/obj/item/reagent_containers/pill/zoom
	pill_desc = "A Zoom pill! Gotta go fast!"
	list_reagents = list(/datum/reagent/medicine/synaptizine = 3, /datum/reagent/medicine/hyronalin = 5)
	pill_id = 14

/obj/item/reagent_containers/pill/russian_red
	pill_desc = "A Russian Red pill. A last-resort revival drug."
	list_reagents = list(/datum/reagent/medicine/russian_red = 10)
	pill_id = 15

/obj/item/reagent_containers/pill/ryetalyn
	pill_desc = "A Ryetalyn pill. A long-duration shield against toxic chemicals."
	list_reagents = list(/datum/reagent/medicine/ryetalyn = 15)
	pill_id = 14

/obj/item/reagent_containers/pill/imidazoline
	pill_desc = "An imidazoline pill. Heals eye damage."
	list_reagents = list(/datum/reagent/medicine/imidazoline = 10)
	pill_id = 17

/obj/item/reagent_containers/pill/alkysine
	pill_desc = "An Alkysine pill. Heals brain and ear damage."
	list_reagents = list(/datum/reagent/medicine/alkysine = 10)
	pill_id = 18

/obj/item/reagent_containers/pill/bicaridine
	pill_desc = "A bicaridine pill. Heals brute damage."
	list_reagents = list(/datum/reagent/medicine/bicaridine = 15)
	pill_id = 19

/obj/item/reagent_containers/pill/meralyne
	pill_desc = "A meralyne pill. Heals brute damage at a higher rate than bicaridine."
	list_reagents = list(/datum/reagent/medicine/meralyne = 7.5)
	pill_id = 19

/obj/item/reagent_containers/pill/quickclot
	pill_desc = "A quick-clot pill. Stabilizes internal bleeding temporarily."
	list_reagents = list(/datum/reagent/medicine/quickclot = 10)
	pill_id = 20

/obj/item/reagent_containers/pill/tricordrazine
	pill_desc = "A tricordrazine pill. Broad spectrum medication that slowly heals all damage types."
	list_reagents = list(/datum/reagent/medicine/tricordrazine = 15)
	pill_id = 9

/obj/item/reagent_containers/pill/leporazine
	pill_desc = "A leporazine pill. Helps handle extreme body temperatures rapidly."
	list_reagents = list(/datum/reagent/medicine/leporazine = 10)
	pill_id = 21

/obj/item/reagent_containers/pill/hypervene
	pill_desc = "A hypervene pill. A purge medication used to treat overdoses and rapidly remove toxins. Causes pain and vomiting."
	list_reagents = list(/datum/reagent/hypervene = 3)
	pill_id = 14

/obj/item/reagent_containers/pill/ultrazine
	//pill_desc = "An ultrazine pill. A highly-potent, long-lasting combination CNS and muscle stimulant. Extremely addictive."
	list_reagents = list(/datum/reagent/medicine/ultrazine = 5)
	pill_id = 21
