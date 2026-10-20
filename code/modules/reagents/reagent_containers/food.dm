////////////////////////////////////////////////////////////////////////////////
/// Food.
////////////////////////////////////////////////////////////////////////////////
/obj/item/reagent_containers/food
	worn_icon_list = list(
		slot_l_hand_str = 'icons/mob/inhands/items/food_left.dmi',
		slot_r_hand_str = 'icons/mob/inhands/items/food_right.dmi',
	)
	possible_transfer_amounts = null
	volume = 50 //Sets the default container amount for all food items.
	reagent_flags = INJECTABLE
	var/filling_color = "#FFFFFF" //Used by sandwiches.
	var/list/center_of_mass = newlist() //Center of mass

/obj/item/reagent_containers/food/Initialize(mapload)
	. = ..()
	if(!pixel_x && !pixel_y)
		pixel_x = rand(-6, 6) //Randomizes postion
		pixel_y = rand(-6, 6)

/obj/item/reagent_containers/food/interact_with_atom(atom/interacting_with, mob/living/user, list/modifiers)
	if(!istype(interacting_with, /obj/structure/table) || !length(center_of_mass))
		return NONE
	// Places the item on a grid.
	var/cellnumber = 4
	var/mouse_x = text2num(modifiers["icon-x"])
	var/mouse_y = text2num(modifiers["icon-y"])
	var/grid_x = round(mouse_x, 32 / cellnumber)
	var/grid_y = round(mouse_y, 32 /cellnumber)

	if(modifiers["icon-x"])
		var/sign = mouse_x - grid_x != 0 ? SIGN(mouse_x - grid_x) : -1 // Positive if rounded down, else negative.
		pixel_x = grid_x - center_of_mass["x"] + ((sign * 16) / cellnumber) // Center of the cell
	if(modifiers["icon-y"])
		var/sign = mouse_y - grid_y != 0 ? SIGN(mouse_y - grid_y) : -1
		pixel_y = grid_y - center_of_mass["y"] + ((sign * 16) / cellnumber)

/obj/item/reagent_containers/food/is_refuelable()
	return FALSE
