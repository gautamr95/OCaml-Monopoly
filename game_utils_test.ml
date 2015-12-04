open Game_utils

TEST_MODULE "board test" = struct
  let create_prop_list () =
    (create_property 1 Brown 300 20 "baltic") ::
    (create_property 3 Brown 300 20 "blah") ::
    (create_property 4 Brown 300 20 "boom") ::
    (create_property 5 Green 300 20 "park") ::
    (create_property 7 Green 300 20 "atlantic") ::
    (create_property 8 Green 300 20 "pacific") :: []

  let create_tile_list prop_lst =
    Go :: Prop(List.nth prop_lst 0) :: Chance :: Prop(List.nth prop_lst 1) ::
    Prop(List.nth prop_lst 2) :: Prop(List.nth prop_lst 3) :: Chest
    :: Prop(List.nth prop_lst 4) :: Prop(List.nth prop_lst 5) :: Jail(9) ::
    Go_jail :: []
  let create_chance_list () =
    [("boo", -50) ; ("shoo",30)]

  let create_community_chest_list () =
    [("foo", 100) ; ("dog",-300)]

  let property_list = create_prop_list ()
  let tile_list = create_tile_list property_list
  let chance_list = create_community_chest_list()
  let community_chest_list =  create_chance_list()

  let board = create_board [false;false;true;false] community_chest_list chance_list
                          property_list tile_list
  TEST "ai" = (is_ai board 2) = true
  TEST "ai_f" = (is_ai board 0) = false
  TEST "get_prop_from_name" = (get_property_from_name board "pacific")
                            = Some(create_property 8 Green 300 20 "pacific")
  TEST "get_prop_from_name_upper" = (get_property_from_name board "PacifIc")
                            = Some(create_property 8 Green 300 20 "pacific")

  TEST "get_prop_from name invalid" = (get_property_from_name board "blue")
                            = None

  TEST "get tile chance" = match get_tile board 2 with
                    | Chance -> true
                    |_ -> false

  TEST "get tile Go" = match get_tile board 0 with
                    | Go -> true
                    |_ -> false

  TEST "get tile property" = match get_tile board 1 with
                    | Prop x -> x = (create_property 1 Brown 300 20 "baltic")
                    | _ -> false
  TEST "get_property valid" = (get_property board 4)
                               = Some(create_property 4 Brown 300 20 "boom")

  TEST "get_property invalid" = (get_property board 2)
                               = None

  TEST "get pl start pos" = get_pl_position board 2  = 0

  TEST "move pl 2" = move_player board 2 3; get_pl_position board 2 = 3

  TEST "collect 200" = move_player board 2 7; (get_pl_position board 2 = 0) &&
                                              (get_money board 2 = 1700)

  TEST "lose all money" = change_money board 1 (-1500); get_money board 1 = 0

  TEST "is_bankrupt" = (is_bankrupt board 1) && not(is_bankrupt board 2)

  TEST "move pl 3" = move_player board 3 4; move_player board 3 7;
                     (get_money board 3 = 1700) && (get_pl_position board 3 = 1)

  TEST "get empty list from container" = !(get_pl_prop_of_color board 2
                                         (create_property 4 Brown 300 20 "boom")) = []
  TEST "get prop price and rent" = match (get_property board 4) with
                                  |Some x ->
                                   (get_prop_price x = 300)&& (get_rent x = 20)
                                   && (get_prop_name x = "boom")
                                  |None -> false

 TEST "move to jail" = move_to_jail board 0; (get_pl_position board 0 = 9) &&
                                             (in_jail board 0)
 TEST "leave jail" = leave_jail board 0; not(in_jail board 0)

 TEST "buy property" = let x = get_property_from_name board "boom" in
          match x with
          | Some i ->
          (move_property board 2 None i;
          !(get_pl_prop_of_color board 2 i) = [i])
           &&
           (match get_holder i with
            | Some j -> j = 2
            | None -> print_string "fuck"; false)
          | None -> false

TEST "holder correct" = let x = get_property_from_name board "boom" in
                        match x with
                        | Some j ->
                          (match get_holder j with
                          | Some i -> print_string (string_of_int i); i = 2
                          | None -> print_string "fuck"; false)
                        | None  -> false
end