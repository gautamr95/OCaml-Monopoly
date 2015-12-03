open Random
(* Constant values *)

(* Types used in the game *)
type color = Brown|Grey|Pink|Orange|Red|Yellow|Green|Blue (*to do*)
type property = { position: int;
                  color: color;
                  cost: int;
                  holder: int option ref;
                  rent: int;
                  name: string;
                  bankrupt:bool ref;
                  houses: int ref
                }


and player = { id: int;
               token: string;
               position: int ref;
               properties: property_container;
               is_AI: bool;
               in_jail: bool;
               money: int
             }

and property_container = { brown: property list ref;
                           grey: property list ref;
                           pink: property list ref;
                           orange: property list ref;
                           red: property list ref;
                           yellow: property list ref;
                           green: property list ref;
                           blue: property list ref;
                         }

type community_chest = string * int

type chance = string * int


type board = { player_list: player list;
               community_chest_list: community_chest list;
               chance_list: chance list;
               property_list: property list;
               tile_list : tile list;
             }

type tile = Prop of property | Chance | Chest |Jail | Go | Go_jail

let get_player b pl_id =
  let pl_list = get_player_list b in
  List.nth pl_list (pl_id)

let get_player_list b =
  b.player_list

let get_property_from_name b p_name =
  let p_list = b.property_list in
  try Some(List.find (fun x -> x.name = p_name) p_list) with
  | Not_Found -> None

let get_property b pos =
  match get_tile b pos with
  |Prop x-> Some x
  |_     -> None

let get_pl_position b pl_id =
  let pl = get_player b pl_id in
  pl.position

let get_prop_position b p =
  p.position

let get_chest b =
  let num_chest = List.length b.community_chest_list in
  let rand_num = Random.int num_chest in
  List.nth b.community_chest_list

let get_chance b =
  let num_chance = List.length b.chance_list in
  let rand_num = Random.int num_chance in
  List.nth b.chance_list

let move_player b pl_id i =
  let pl = get_player b pl_id in
  let new_pos = ((!pl.position + i) mod ((List.length b.tile_list) - 1)) in
  if new_pos < pl.position then change_money b pl_id 200 else ();
  pl.position := new_pos

let change_money b pl_id amt =
  let pl = get_player b pl_id in
  pl.money := !pl.money + amt;
  if !pl.money < 0 then pl.bankrupt:=true;
  print_endline "Collected $200 for passing go" else ()

let is_ai b pl_id =
  let pl = get_player b pl_id in
  pl.is_AI

let move_property b pl_id pl_id2 prop =
  let prop_list = get_pl_prop_of_color b pl_id prop in
  prop_list:= prop :: !prop_list;
  match pl_id2 with
  | None   -> ();
  | Some x -> let pl2_props = get_pl_prop_of_color b x prop in
    pl2_props := List.filter (fun x -> x = prop) !pl2_props;
  prop.holder := some pl

let get_prop_price p = p.cost

let get_prop_name p = p.name

let in_jail b pl_id =
  let pl = get_player b pl_id in
  let pos = List.nth b.tile_list pl.position in
  match pos with
  | Jail _ -> true
  | _      -> false

let is_chance b pos =
  let tile = List.nth b.tile_list pos in
  match tile with
  |Chance _ -> true
  | _ -> false

let is_chest b pos =
  let tile = List.nth b.tile_list pos in
  match tile with
  |Chest _ -> true
  | _ -> false

let is_go_jail b pos =
  let tile = List.nth b.tile_list pos in
  match tile with
  |Go_jail _ -> true
  | _ -> false

let get_player_property b pl_id =
  let pl = get_player b pl_id in
  pl.properties

let get_pl_prop_of_color b pl_id prop =
  let pl = get_player_property b pl_id in
  match prop.color with
  |Brown ->pl.brown
  |Grey  ->pl.grey
  |Pink  ->pl.pink
  |Orange->pl.orange
  |Red   ->pl.red
  |Yellow->pl.yellow
  |Green ->pl.green
  |Blue  ->pl.blue

let get_money b pl_id =
  let pl = get_player b pl_id in
  pl.money

let get_tile b pos =
  List.nth b.tile_list pos

let is_bankrupt b pl_id =
  let pl = get_player b pl_id in
    !pl.bankrupt

let others_bankrupt b pl_id =
  let filtered = List.filter (fun x -> x.id <> pl_id) b.player_list in
  List.fold_left (fun x y -> (!y.bankrupt = true) && x) true filtered

let get_holder prop =
  !prop.holder

let get_rent prop =
  prop.rent

let get_tile b pos =
  List.nth b.tile_list pos

let move_to_jail

let get_rent

let is_property b prop_name =
  let prop_list = b.property_list in
  List.exists (fun x -> x.name = prop_name) prop_list
(*
(* Creates an empty property container and returns it
   Inputs: None
   Output: empty property_container *)
let create_empty_prop_cont () : property_container =
  {blue= ref []; green= ref []; yellow= ref []; orange= ref []; black= ref []}

let create_board human_players player_names =
  let temp_player_list = ref [] in
  let id_counter = ref 0 in

  for i = 1 to human_players do
    let temp_player = { id= !id_counter; name= player_names.(!id_counter);
                        token= token_names.(!id_counter); position= ref 0;
                        properties= create_empty_prop_cont (); is_AI= false;
                        in_jail= false
                      } in
    id_counter := !id_counter + 1;
    temp_player_list := !temp_player_list@[temp_player]
  done;

  if human_players <> 4 then
    for i = 1 to 4 - human_players do
      let temp_player = { id= !id_counter; name= player_names.(!id_counter);
                        token= token_names.(!id_counter); position= ref 0;
                        properties= create_empty_prop_cont (); is_AI= false;
                        in_jail= false
                      } in
      id_counter := !id_counter + 1;
      temp_player_list := !temp_player_list@[temp_player]
    done

  {player_list= !temp_player_list; community_chest_list= ;
  chance_list= ; property_list= }
*)




















(*these 2 functions are dumb*)










