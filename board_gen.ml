open Game_utils
let create_prop_list () =
    (create_property 1 Brown 60 2"Mediterranean") ::
    (create_property 3 Brown 60 4 "Baltic") ::
    (create_property 4 Brown 70 5 "Caspian") ::
    (create_property 6 Grey 100 6 "Oriental") ::
    (create_property 8 Grey 100 6 "Vermont") ::
    (create_property 9 Grey 120 8 "Connecticut") ::
    (create_property 11 Pink 140 10 "St.Charles") ::
    (create_property 13 Pink 140 10 "States") ::
    (create_property 14 Pink 160 12 "Virginia") ::
    (create_property 16 Orange 180 14 "St.James") ::
    (create_property 18 Orange 180 14 "Tennessee") ::
    (create_property 19 Orange 200 16 "New York") ::
    (create_property 21 Red 220 18 "Kentucky") ::
    (create_property 23 Red 220 18 "Indiana") ::
    (create_property 24 Red 240 20 "Illinois") ::
    (create_property 26 Yellow 260 22 "Atlantic") ::
    (create_property 27 Yellow 260 22 "Ventnor") ::
    (create_property 29 Yellow 280 24 "Marvin Gardens") ::
    (create_property 31 Green 300 26 "Pacific") ::
    (create_property 32 Green 300 26 "North Carolina") ::
    (create_property 34 Green 320 28 "Pennsylvania") ::
    (create_property 37 Blue 350 35 "Park Place") ::
    (create_property 38 Blue 350 35 "Wall") ::
    (create_property 39 Blue 400 50 "Boardwalk") ::
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
  Prop(List.nth prop_lst 6) ::
  Chance ::
  Prop(List.nth prop_lst 7) ::
  Prop(List.nth prop_lst 8) ::
  Chest ::
  Prop(List.nth prop_lst 9) ::
  Chest ::
  Prop(List.nth prop_lst 10) ::
  Prop(List.nth prop_lst 11) ::
  Go_jail ::
  Prop(List.nth prop_lst 12) ::
  Chance ::
  Prop(List.nth prop_lst 13) ::
  Prop(List.nth prop_lst 14) ::
  Chance ::
  Prop(List.nth prop_lst 15) ::
  Prop(List.nth prop_lst 16) ::
  Chest ::
  Prop(List.nth prop_lst 17) ::
  Go_jail ::
  Prop(List.nth prop_lst 18) ::
  Prop(List.nth prop_lst 19) ::
  Chest ::
  Prop(List.nth prop_lst 20) ::
  Chest ::
  Chance ::
  Prop(List.nth prop_lst 21) ::
  Prop(List.nth prop_lst 22) ::
  Prop(List.nth prop_lst 23) ::
  []
let create_chance_list () =
  [("What a terrible post! You just got mad downvotes", -50,0);
  ("You just did a dank 1v3 clutch. GG",30,-10);
  ("You just made the front page!",100,0);
  ("You wasted 5 hours browsing dank memes",-20,0);
  ("Senpai noticed your dank meme",50,0);
  ("You made a dank subreddit and everyone joined",100,20);
  ("There are no brakes on the feel train",-20,-20);]

let create_community_chest_list () =
  [("U WOT M8", -300,50);
  ("You got 10000 upvotes. Wow!",300,0);
  ("Dank fuel can't melt jet memes",150,50);
  ("Ayy lmao",200,0);
  ("You have surpassed 1337 link karma",200,0);]