GLOBAL_LIST_EMPTY(tagger_locations)

// todo replace me with tg code and uncamelcase me
/obj/structure/bigDelivery
	desc = "A big wrapped package."
	name = "large parcel"
	icon = 'icons/obj/items/storage/misc.dmi'
	icon_state = "deliverycloset"
	var/obj/wrapped = null
	density = TRUE
	var/sortTag = null
	var/examtext = null
	var/nameset = 0
	var/label_y
	var/label_x
	var/tag_x

/obj/structure/bigDelivery/attack_hand(mob/user as mob)
	if(wrapped) //sometimes items can disappear. For example, bombs. --rastaf0
		wrapped.loc = (get_turf(src.loc))
		if(istype(wrapped, /obj/structure/closet))
			var/obj/structure/closet/O = wrapped
			O.welded = 0
	qdel(src)
	return

/obj/structure/bigDelivery/update_overlays()
	. = ..()
	if(nameset || examtext)
		var/image/I = new/image('icons/obj/items/storage/misc.dmi', "delivery_label")
		if(icon_state == "deliverycloset")
			I.pixel_w = 2
			if(label_y == null)
				label_y = rand(-6, 11)
			I.pixel_z = label_y
		else if(icon_state == "deliverycrate")
			if(label_x == null)
				label_x = rand(-8, 6)
			I.pixel_w = label_x
			I.pixel_z = -3
		. += I
	if(src.sortTag)
		var/image/I = new/image('icons/obj/items/storage/misc.dmi', "delivery_tag")
		if(icon_state == "deliverycloset")
			if(tag_x == null)
				tag_x = rand(-2, 3)
			I.pixel_w = tag_x
			I.pixel_z = 9
		else if(icon_state == "deliverycrate")
			if(tag_x == null)
				tag_x = rand(-8, 6)
			I.pixel_w = tag_x
			I.pixel_z = -3
		. += I

/obj/structure/bigDelivery/examine(mob/user)
	..()
	if(get_dist(src, user) <= 4)
		if(sortTag)
			to_chat(user, span_notice("It is labeled \"[sortTag]\""))
		if(examtext)
			to_chat(user, span_notice("It has a note attached which reads, \"[examtext]\""))
	return

/obj/structure/bigDelivery/attack_alien(mob/living/carbon/xenomorph/xeno_attacker, damage_amount = xeno_attacker.xeno_caste.melee_damage, damage_type = BRUTE, armor_type = MELEE, effects = TRUE, armor_penetration = xeno_attacker.xeno_caste.melee_ap, isrightclick = FALSE)
	attack_hand(xeno_attacker)

/obj/structure/bigDelivery/attackby(obj/item/I, mob/user, params)
	. = ..()
	if(.)
		return

	if(istype(I, /obj/item/destTagger))
		var/obj/item/destTagger/O = I
		if(!O.currTag)
			to_chat(user, span_warning("You need to set a destination first!"))
			return

		if(sortTag == O.currTag)
			to_chat(user, span_warning("The package is already labeled for [O.currTag]."))
			return

		to_chat(user, span_notice("You have labeled the destination as [O.currTag]."))
		if(!sortTag)
			sortTag = O.currTag
			update_icon()
		else
			sortTag = O.currTag
		playsound(loc, 'sound/machines/twobeep.ogg', 25, 1, MEDIUM_SOUND_RANGE)

	else if(istype(I, /obj/item/tool/pen))
		switch(tgui_alert(user, "What would you like to alter?", "Label", list("Title", "Description", "Cancel")))
			if("Title")
				var/str = stripped_input(user, "Label text?", "Set label", max_length = MAX_NAME_LEN)
				if(!str)
					to_chat(user, span_warning("Invalid text."))
					return
				user.visible_message("\The [user] titles \the [src] with \a [I], marking down: \"[str]\"",\
				span_notice("You title \the [src]: \"[str]\""),\
				"You hear someone scribbling a note.")
				name = "[name] ([str])"
				if(!examtext && !nameset)
					nameset = TRUE
					update_icon()
				else
					nameset = TRUE
			if("Description")
				var/str = stripped_input(user, "Label text?", "Set label", max_length = MAX_NAME_LEN)
				if(!str)
					to_chat(user, span_warning("Invalid text."))
					return
				if(!examtext && !nameset)
					examtext = str
					update_icon()
				else
					examtext = str
				user.visible_message("\The [user] labels \the [src] with \a [I], scribbling down: \"[examtext]\"",\
				span_notice("You label \the [src]: \"[examtext]\""),\
				"You hear someone scribbling a note.")


/obj/item/smallDelivery
	desc = "A small wrapped package."
	name = "small parcel"
	icon = 'icons/obj/items/storage/misc.dmi'
	icon_state = "deliverycrate3"
	var/obj/item/wrapped = null
	var/sortTag = null
	var/examtext = null
	var/nameset = 0
	var/tag_x

/obj/item/smallDelivery/attack_self(mob/user as mob)
	if (src.wrapped) //sometimes items can disappear. For example, bombs. --rastaf0
		wrapped.loc = user.loc
		if(ishuman(user))
			user.put_in_hands(wrapped)
		else
			wrapped.loc = get_turf(src)

	qdel(src)
	return

/obj/item/smallDelivery/update_overlays()
	. = ..()
	if((nameset || examtext) && icon_state != "deliverycrate1")
		var/image/I = new/image('icons/obj/items/storage/misc.dmi', "delivery_label")
		if(icon_state == "deliverycrate5")
			I.pixel_y = -1
		. += I
	if(src.sortTag)
		var/image/I = new/image('icons/obj/items/storage/misc.dmi', "delivery_tag")
		switch(icon_state)
			if("deliverycrate1")
				I.pixel_y = -5
			if("deliverycrate2")
				I.pixel_y = -2
			if("deliverycrate3")
				I.pixel_y = 0
			if("deliverycrate4")
				if(tag_x == null)
					tag_x = rand(0,5)
				I.pixel_x = tag_x
				I.pixel_y = 3
			if("deliverycrate5")
				I.pixel_y = -3
		. += I

/obj/item/smallDelivery/examine(mob/user)
	..()
	if(get_dist(src, user) <= 4)
		if(sortTag)
			to_chat(user, span_notice("It is labeled \"[sortTag]\""))
		if(examtext)
			to_chat(user, span_notice("It has a note attached which reads, \"[examtext]\""))

/obj/item/smallDelivery/attackby(obj/item/I, mob/user, params)
	. = ..()
	if(.)
		return

	if(istype(I, /obj/item/destTagger))
		var/obj/item/destTagger/O = I
		if(!O.currTag)
			to_chat(user, span_warning("You need to set a destination first!"))
			return

		if(sortTag == O.currTag)
			to_chat(user, span_warning("The package is already labeled for [O.currTag]."))
			return

		to_chat(user, span_notice("You have labeled the destination as [O.currTag]."))
		if(!sortTag)
			sortTag = O.currTag
			update_icon()
		else
			sortTag = O.currTag
		playsound(loc, 'sound/machines/twobeep.ogg', 25, 1, MEDIUM_SOUND_RANGE)

	else if(istype(I, /obj/item/tool/pen))
		switch(tgui_alert(user, "What would you like to alter?", "Label", list("Title", "Description", "Cancel")))
			if("Title")
				var/str = stripped_input(user, "Label text?", "Set label", max_length = MAX_NAME_LEN)
				if(!str)
					to_chat(user, span_warning("Invalid text."))
					return
				user.visible_message("\The [user] titles \the [src] with \a [I], marking down: \"[str]\"",\
				span_notice("You title \the [src]: \"[str]\""),\
				"You hear someone scribbling a note.")
				name = "[name] ([str])"
				if(!examtext && !nameset)
					nameset = TRUE
					update_icon()
				else
					nameset = TRUE
			if("Description")
				var/str = stripped_input(user, "Label text?", "Set label", max_length = MAX_NAME_LEN)
				if(!str)
					to_chat(user, span_warning("Invalid text."))
					return
				if(!examtext && !nameset)
					examtext = str
					update_icon()
				else
					examtext = str
				user.visible_message("\The [user] labels \the [src] with \a [I], scribbling down: \"[examtext]\"",\
				span_notice("You label \the [src]: \"[examtext]\""),\
				"You hear someone scribbling a note.")


/obj/item/packageWrap
	name = "package wrapper"
	icon = 'icons/obj/stack_objects.dmi'
	icon_state = "deliveryPaper"
	w_class = WEIGHT_CLASS_NORMAL
	/// The amount left.
	var/amount = 25

/obj/item/packageWrap/examine(mob/user)
	. = ..()
	if(get_dist(src, user) < 2)
		. = span_notice("There are [amount] units of package wrap left!")

/obj/item/packageWrap/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	. = ..()
	if(.)
		return

	if(!isobj(interacting_with))
		return NONE
	var/obj/interacting_object = interacting_with
	if(interacting_object.anchored)
		return NONE
	if(interacting_object in user)
		return NONE
	if(user in interacting_object) // No wrapping closets that you are inside - it's not physically possible.
		return NONE

	var/static/list/disallowed_typepaths = list(
		/obj/item/smallDelivery,
		/obj/structure/bigDelivery,
		/obj/item/gift,
		/obj/item/evidencebag,
	)
	if(is_type_in_list(interacting_object, disallowed_typepaths))
		return NONE

	log_combat(user, interacting_object, "used", src)

	if(istype(interacting_object, /obj/structure/closet/crate))
		var/obj/structure/closet/crate/interacting_crate = interacting_object
		if(amount >= 3 && !interacting_crate.opened)
			var/obj/structure/bigDelivery/wrapped_thing = new /obj/structure/bigDelivery(get_turf(interacting_crate.loc))
			wrapped_thing.icon_state = "deliverycrate"
			wrapped_thing.wrapped = interacting_object
			interacting_crate.forceMove(wrapped_thing)
			amount -= 3
			user.visible_message(
				span_notice("[user] wraps \a [interacting_crate] with \a [src]."),
				span_notice("You wrap \the [interacting_crate], [amount <= 0 ? "using all of the" : "leaving [amount] units of"] paper on \the [src]."),
				span_notice("You hear someone taping paper around a large object.")
			)
		else if(amount < 3)
			to_chat(user, span_warning("You need more paper to wrap the closet!"))
			return ITEM_INTERACT_BLOCKING
	else if(istype(interacting_object, /obj/structure/closet))
		var/obj/structure/closet/interacting_closet = interacting_object
		if(amount >= 3 && !interacting_closet.opened)
			var/obj/structure/bigDelivery/wrapped_thing = new /obj/structure/bigDelivery(get_turf(interacting_closet.loc))
			wrapped_thing.wrapped = interacting_closet
			interacting_closet.welded = TRUE
			interacting_closet.forceMove(wrapped_thing)
			amount -= 3
			user.visible_message(
				span_notice("[user] wraps \a [interacting_closet] with \a [src]."),
				span_notice("You wrap \the [interacting_closet], [amount <= 0 ? "using all of the" : "leaving [amount] units of"] paper on \the [src]."),
				span_notice("You hear someone taping paper around a large object.")
			)
		else if(amount < 3)
			to_chat(user, span_warning("You need more paper to wrap the crate!"))
			return ITEM_INTERACT_BLOCKING
	else if(istype(interacting_object, /obj/item) && !(istype(interacting_object, /obj/item/storage) && !istype(interacting_object, /obj/item/storage/box)))
		var/obj/item/interacting_item = interacting_object
		if(amount >= 1)
			var/obj/item/smallDelivery/wrapped_thing = new /obj/item/smallDelivery(get_turf(interacting_item.loc))
			wrapped_thing.wrapped = interacting_item
			interacting_item.forceMove(wrapped_thing)

			var/fixed_weight_class = clamp(round(interacting_item.w_class), WEIGHT_CLASS_TINY, WEIGHT_CLASS_HUGE)
			wrapped_thing.icon_state = "deliverycrate[fixed_weight_class]"
			switch(fixed_weight_class)
				if(-INFINITY to WEIGHT_CLASS_TINY)
					wrapped_thing.name = "tiny parcel"
				if(WEIGHT_CLASS_SMALL)
					wrapped_thing.name = "small parcel"
				if(WEIGHT_CLASS_NORMAL)
					wrapped_thing.name = "normal-sized parcel"
				if(WEIGHT_CLASS_BULKY)
					wrapped_thing.name = "large parcel"
				if(WEIGHT_CLASS_HUGE)
					wrapped_thing.name = "huge parcel"
			amount -= 1
			user.visible_message(
				span_notice("[user] wraps \a [interacting_item] with \a [src]."),
				span_notice("You wrap \the [interacting_item], [amount <= 0 ? "using all of the" : "leaving [amount] units of"] paper on \the [src]."),
				span_notice("You hear someone taping paper around a small object.")
			)
	else
		to_chat(user, span_notice("The object you are trying to wrap is unsuitable for the sorting machinery!"))
		return ITEM_INTERACT_BLOCKING

	if(amount <= 0)
		new /obj/item/trash/c_tube(loc)
		qdel(src)
	return ITEM_INTERACT_SUCCESS

/obj/item/destTagger
	name = "destination tagger"
	desc = "Used to set the destination of properly wrapped packages."
	icon = 'icons/obj/device.dmi'
	icon_state = "dest_tagger"
	var/currTag = 0

	w_class = WEIGHT_CLASS_SMALL
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/equipment/engineering_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/equipment/engineering_right.dmi',
	)
	worn_icon_state = "electronic"
	atom_flags = CONDUCT
	equip_slot_flags = ITEM_SLOT_BELT

/obj/item/destTagger/proc/openwindow(mob/user as mob)
	var/dat

	dat += "<table style='width:100%; padding:4px;'><tr>"
	for(var/i in 1 to length(GLOB.tagger_locations))
		dat += "<td><a href='byond://?src=[text_ref(src)];nextTag=[GLOB.tagger_locations[i]]'>[GLOB.tagger_locations[i]]</a></td>"

		if (i%4==0)
			dat += "</tr><tr>"

	dat += "</tr></table><br>Current Selection: [currTag ? currTag : "None"]"

	var/datum/browser/popup = new(user, "destTagScreen", "<div align='center'>TagMaster 2.3</div>", 450, 350)
	popup.set_content(dat)
	popup.open(FALSE)
	onclose(user, "destTagScreen")

/obj/item/destTagger/attack_self(mob/user as mob)
	openwindow(user)
	return

/obj/item/destTagger/Topic(href, href_list)
	if(href_list["nextTag"] && (href_list["nextTag"] in GLOB.tagger_locations))
		src.currTag = href_list["nextTag"]
	openwindow(usr)

/obj/machinery/disposal/deliveryChute
	name = "Delivery chute"
	desc = "A chute for big and small packages alike!"
	density = TRUE
	icon_state = "intake"

	var/c_mode = 0

/obj/machinery/disposal/deliveryChute/Initialize(mapload)
	. = ..()
	set_trunk(locate(/obj/structure/disposalpipe/trunk) in loc)
	if(trunk)
		trunk.set_linked(src)	// link the pipe trunk to self

/obj/machinery/disposal/deliveryChute/interact()
	return

/obj/machinery/disposal/deliveryChute/attack_hand_secondary(mob/living/user)
	return

/obj/machinery/disposal/deliveryChute/update()
	return

/obj/machinery/disposal/deliveryChute/Bumped(atom/movable/AM) //Go straight into the chute
	if(istype(AM, /atom/movable/projectile) || istype(AM, /obj/effect))
		return
	switch(dir)
		if(NORTH)
			if(AM.loc.y != loc.y+1) return
		if(EAST)
			if(AM.loc.x != loc.x+1) return
		if(SOUTH)
			if(AM.loc.y != loc.y-1) return
		if(WEST)
			if(AM.loc.x != loc.x-1) return

	if(istype(AM, /obj))
		var/obj/O = AM
		O.loc = src
	else if(istype(AM, /mob))
		var/mob/M = AM
		M.loc = src
	flush()

/obj/machinery/disposal/deliveryChute/flush()
	flushing = 1
	flick("intake-closing", src)
	var/obj/structure/disposalholder/H = new()	// virtual holder object which actually
												// travels through the pipes.

	sleep(1 SECONDS)
	playsound(src, 'sound/machines/disposalflush.ogg', 25, 0, MEDIUM_SOUND_RANGE)
	sleep(0.5 SECONDS) // wait for animation to finish

	H.init(src)	// copy the contents of disposer to holder

	H.start(src) // start the holder processing movement
	flushing = 0
	// now reset disposal state
	flush = 0
	if(mode == 2)	// if was ready,
		mode = 1	// switch to charging
	update()
	return

/obj/machinery/disposal/deliveryChute/attackby(obj/item/I, mob/user, params)
	. = ..()
	if(.)
		return

	if(isscrewdriver(I))
		c_mode = !c_mode
		I.play_tool_sound(src)
		if(c_mode)
			to_chat(user, "You remove the screws around the power connection.")
		else
			to_chat(user, "You attach the screws around the power connection.")

	else if(istype(I, /obj/item/tool/weldingtool) && c_mode)
		var/obj/item/tool/weldingtool/W = I

		if(!W.isOn())
			to_chat(user, "Welder is not on.")
			return

		to_chat(user, "You start slicing the floorweld off the delivery chute.")

		if(!W.use_tool(src, user, 2 SECONDS, user_display = BUSY_ICON_BUILD))
			return

		to_chat(user, "You sliced the floorweld off the delivery chute.")
		var/obj/structure/disposalconstruct/C = new(loc)
		C.ptype = 8 // 8 = Delivery chute
		C.update()
		C.anchored = TRUE
		C.density = TRUE
		qdel(src)
