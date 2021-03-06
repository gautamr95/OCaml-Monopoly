open Random
Random.self_init ();

exception TODO
(* Types used in the game *)

(* Constant values *)
let house_cost = -50
let house_return = 30

type color = Brown|Grey|Pink|Orange|Red|Yellow|Green|Blue (*to do*)
type property = { position: int;
                  color: color;
                  cost: int;
                  holder: int option ref;
                  rent: int;
                  name: string;
                  houses: int ref
                }

type property_container = { brown: property list ref;
                           grey: property list ref;
                           pink: property list ref;
                           orange: property list ref;
                           red: property list ref;
                           yellow: property list ref;
                           green: property list ref;
                           blue: property list ref;
                         }
type player = { id: int;
               position: int ref;
               properties: property_container;
               is_AI: bool;
               in_jail: bool ref;
               bankrupt:bool ref;
               is_done: bool ref;
               money: int ref;
             }

type community_chest = string * int * int

type chance = string * int * int

type tile = Prop of property | Chance | Chest |Jail of int | Go | Go_jail

type board = { player_list: player list;
               community_chest_list: community_chest list;
               chance_list: chance list;
               property_list: property list;
               tile_list : tile list;
               round : int ref;
               turn : int ref
             }


let get_player_list b =
  b.player_list

let get_property_list b =
  b.property_list

let get_round b =
  !(b.round)

let incr_round b =
  b.round := !(b.round) + 1

let get_player b pl_id =
  let pl_list = get_player_list b in
  List.nth pl_list (pl_id)

let get_property_from_name b p_name =
  let p_list = b.property_list in
  try Some(List.find (fun x -> (String.lowercase x.name) =
                     String.lowercase(p_name)) p_list) with
  | Not_found -> None

let get_tile b pos =
  List.nth b.tile_list pos

let get_player_id pl =
  pl.id

let get_property b pos =
  match get_tile b pos with
  |Prop x-> Some x
  |_     -> None

let get_pl_position b pl_id =
  let pl = get_player b pl_id in
  !(pl.position)

let get_prop_position (prop:property) =
  prop.position

let get_chest b =
  let num_chest = List.length b.community_chest_list in
  let rand_num = (Random.int num_chest) in
  List.nth b.community_chest_list rand_num

let get_chance b =
  let num_chance = List.length b.chance_list in
  let rand_num = Random.int num_chance in
  List.nth b.chance_list rand_num

let get_money b pl_id =
  let pl = get_player b pl_id in
  !(pl.money)

let change_money b pl_id amt =
  let pl = get_player b pl_id in
  pl.money := !(pl.money) + amt;
  if !(pl.money) <= 0 then
    pl.bankrupt:=true
  else ()

let move_player b pl_id i =
  let pl = get_player b pl_id in
  let new_pos = ((!(pl.position) + i) mod ((List.length b.tile_list))) in
  if new_pos < !(pl.position) then
    change_money b pl_id 200
  else ();
  pl.position := new_pos

let is_ai b pl_id =
  let pl = get_player b pl_id in
  pl.is_AI

let get_player_property b pl_id =
  let pl = get_player b pl_id in
  pl.properties

(* TODO, not tested *)
let get_player_property_val b pl_id =
  let props = get_player_property b pl_id in
  let tot_value = ref 0 in
  let add_values lst_ref =
    let prop_list = !lst_ref in
    let add_func prop =
      tot_value := !tot_value + prop.cost + house_cost * !(prop.houses) in
    List.iter add_func prop_list in
  (add_values props.brown);
  (add_values props.grey);
  (add_values props.pink);
  (add_values props.orange);
  (add_values props.red);
  (add_values props.yellow);
  (add_values props.green);
  (add_values props.blue);
  !tot_value

let get_pl_prop_from_color b pl_id col =
  let pl = get_player_property b pl_id in
  match col with
  |Brown ->pl.brown
  |Grey  ->pl.grey
  |Pink  ->pl.pink
  |Orange->pl.orange
  |Red   ->pl.red
  |Yellow->pl.yellow
  |Green ->pl.green
  |Blue  ->pl.blue

let get_pl_prop_of_color b pl_id prop =
  get_pl_prop_from_color b pl_id prop.color

let move_property b pl_id pl_id2 prop =
  prop.holder := Some pl_id;
  let prop_list = get_pl_prop_of_color b pl_id prop in
  prop_list:= prop :: !(prop_list);
  match pl_id2 with
  | None   -> ()
  | Some x -> let pl2_props = get_pl_prop_of_color b x prop in
    pl2_props := List.filter (fun x -> x <> prop) !(pl2_props)

let get_prop_price p = p.cost

let get_prop_name p = p.name

let in_jail b pl_id =
  let pl = get_player b pl_id in
  !(pl.in_jail)

let is_chance b pos =
  let tile = get_tile b pos in
  match tile with
  |Chance -> true
  | _ -> false

let is_chest b pos =
  let tile = get_tile b pos in
  match tile with
  |Chest -> true
  | _ -> false

let is_go_jail b pos =
  let tile = get_tile b pos in
  match tile with
  | Go_jail -> true
  | _ -> false

let is_bankrupt b pl_id =
  let pl = get_player b pl_id in
    !(pl.bankrupt)

let others_bankrupt b pl_id =
  let filtered = List.filter (fun x -> x.id <> pl_id) b.player_list in
  List.fold_left (fun x y -> (!(y.bankrupt) = true) && x) true filtered

let get_rent prop =
  prop.rent

let get_holder prop =
  !(prop.holder)

let get_tile b pos =
  List.nth b.tile_list pos

let get_houses prop =
  !(prop.houses)

let move_to_jail b pl_id =
  let pl = get_player b pl_id in
  let jail =
    List.find (fun x -> match x with |Jail _ -> true |_ -> false) b.tile_list in
  match jail with
  | Jail i -> pl.position := i; pl.in_jail := true
  | _ -> ()

let leave_jail b pl_id =
  let pl = get_player b pl_id in
  pl.in_jail := false

let print_prop_of_color p_list =
  let list_length = List.length p_list in
  if list_length = 0 then " None"
  else
    List.fold_left
      (fun x y -> x^(Printf.sprintf " %s(Houses:%i) " y.name !(y.houses)))
                   "" p_list

let print_players_properties b pl_id =
  let props = get_player_property b pl_id in
  "\n---------------------------\n"^
  "Brown:" ^ print_prop_of_color !(props.brown)^ "\n" ^
  "Grey:" ^ print_prop_of_color !(props.grey)^"\n" ^
  "Pink:" ^ print_prop_of_color !(props.pink)^"\n" ^
  "Orange:" ^ print_prop_of_color !(props.orange)^"\n" ^
  "Red:" ^ print_prop_of_color !(props.red)^"\n" ^
  "Yellow:" ^ print_prop_of_color !(props.yellow)^"\n" ^
  "Green:" ^ print_prop_of_color !(props.green)^"\n" ^
  "Blue:" ^ print_prop_of_color !(props.blue)^"\n" ^
  "---------------------------\n"

let can_buy_house b pl_id prop =
  let prop_list = !(get_pl_prop_of_color b pl_id prop) in
  ((List.length prop_list) = 3) && (!(prop.houses) <> 4)

let add_house b pl_id prop =
  prop.houses := !(prop.houses) + 1;
  change_money b pl_id house_cost

let remove_house b pl_id prop num =
  prop.houses := !(prop.houses) - num;
  change_money b pl_id (house_return*num)

let create_property position color cost rent name =
  {position;color;cost;holder=ref(None);rent;name;houses=ref(0)}

(* Creates an empty property container and returns it
   Inputs: None
   Output: empty property_container*)
let create_prop_cont () : property_container =
  {brown= ref []; grey= ref []; pink= ref []; orange= ref []; red= ref [];
   yellow= ref []; green= ref []; blue= ref []}

let create_player id ai =
  {id; position = ref(0); properties = create_prop_cont(); is_AI = ai;
   in_jail = ref(false); bankrupt = ref(false); money = ref(1500);
   is_done = ref false}

let create_player_list ai_lst =
  let id_ref = ref(-1) in
  List.map (fun x -> id_ref := !id_ref + 1; create_player !id_ref x) ai_lst

let create_board ai_lst community_chest_list
                 chance_list property_list tile_list =
  let player_list = create_player_list ai_lst in
  {player_list;community_chest_list;chance_list;property_list;
  tile_list;round = ref(1);turn = ref(0)}

(* Returns a number between 1 and 12 inclusive, simulating two dice rolled. *)
let roll_dice () : (int * int) = (1 + Random.int 6 , 1 + Random.int 6 )

let move_to_position b pl_id pos =
  let pl = get_player b pl_id in
  pl.position := pos

let change_others_money b pl_id amt =
  let pl_list = List.filter (fun x -> x <> pl_id) [0;1;2;3] in
  List.fold_left (fun _ x -> change_money b x amt) () pl_list

let get_done b pl_id =
  let pl = get_player b pl_id in
  !(pl.is_done)

let set_done b pl_id =
  let pl = get_player b pl_id in
  let done_ref = pl.is_done in
  done_ref := true

let return_pl_props b pl_id =
  List.iter (fun x ->
    match !(x.holder) with
    | None -> ()
    | Some i -> if i = pl_id then
                  let _ = x.holder := None in
                  x.houses := 0
                else ()) b.property_list

let get_turn b = !(b.turn)

let set_turn b tn = b.turn := tn


