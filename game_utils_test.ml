open Game_utils

TEST_MODULE "board test" = struct
  let board = create_board [false;false;true;false]
  TEST "ai" = (is_ai board 2) = true
  TEST "ai_f" = (is_ai board 0) = false
  TEST "get_prop_from_name" = (get_property_from_name board "pacific")
                            = Some(create_property 8 Green 300 20 "pacific")
  TEST "get_prop_from_name_upper" = (get_property_from_name board "PacifIc")
                            = Some(create_property 8 Green 300 20 "pacific")
  TEST "get tile chance" = match get_tile board 2 with
                    | Chance -> true
                    |_ -> false

  TEST "get tile Go" = match get_tile board 0 with
                    | Go -> true
                    |_ -> false

  TEST "get tile property" = match get_tile board 1 with
                    | Prop x -> x = (create_property 1 Brown 300 20 "baltic")
                    | _ -> false
end