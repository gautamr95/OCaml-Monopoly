open Game_utils
let create_prop_list () =
    (create_property 1 Brown 60 2"Mediterranean") ::
    (create_property 3 Brown 60 4 "Baltic") ::
    (create_property 4 Brown 70 5 "Caspian") ::
    (create_property 6 Green 100 6 "Oriental") ::
    (create_property 8 Green 100 6 "Vermont") ::
    (create_property 9 Green 120 8 "Connecticut") ::
    (create_property 11 Green 140 10 "St.Charles") ::
    (create_property 13 Green 140 10 "States") ::
    (create_property 14 Green 160 12 "Virginia") ::
    (create_property 16 Green 180 14 "St.James") ::
    (create_property 18 Green 180 14 "Tennessee") ::
    (create_property 19 Green 200 16 "New York") ::
    (create_property 21 Green 220 18 "Kentucky") ::
    (create_property 23 Green 220 18 "Indiana") ::
    (create_property 24 Green 240 20 "Illinois") ::
    (create_property 26 Green 260 22 "Atlantic") ::
    (create_property 27 Green 260 22 "Ventnor") ::
    (create_property 29 Green 280 24 "Marvin Gardens") ::
    (create_property 31 Green 300 26 "Pacific") ::
    (create_property 32 Green 300 26 "North Carolina") ::
    (create_property 34 Green 320 28 "Pennsylvania") ::
    (create_property 37 Green 350 35 "Park Place") ::
    (create_property 38 Green 350 35 "Wall") ::
    (create_property 39 Green 400 50 "Boardwalk") ::
    []

let create_tile_list prop_lst =
  Go ::
  Prop(List.nth prop_lst 0) ::
  Chest ::
  Prop(List.nth prop_lst 1) ::
  Prop(List.nth prop_lst 2) ::
  Chance ::
  Prop(List.nth prop_lst 3) ::
  Chance ::
  Prop(List.nth prop_lst 4) ::
  Prop(List.nth prop_lst 5) ::
  Jail(10) ::
  Prop(List.nth prop_lst 5) ::
  Chance ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Chest ::
  Prop(List.nth prop_lst 5) ::
  Chest ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Go_jail ::
  Prop(List.nth prop_lst 5) ::
  Chance ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Chance ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Chest ::
  Prop(List.nth prop_lst 5) ::
  Go_jail ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Chest ::
  Prop(List.nth prop_lst 5) ::
  Chest ::
  Chance ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  Prop(List.nth prop_lst 5) ::
  []
let create_chance_list () =
  [("boo", -50,0) ; ("shoo",30,-10)]

let create_community_chest_list () =
  [("foo", 100,0) ; ("dog",-300,10)]