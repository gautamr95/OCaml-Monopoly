open Random
(* Constant values *)
let jail_position = 20

(* Types used in the game *)

type property = { position: int;
                  color: string;
                  cost: int;
                  holder: player option;
                  rent: int;
                  name: string;
                }


and player = { id: int;
               token: string;
               position: int ref;
               properties: property_container;
               is_AI: bool;
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

type tile = Prop of property | Chance of int | Chest of int |Jail of int
            | Go of int |Tax of int | Go_jail of int

let in_jail p = p.position = jail_position

let is_property b pos =
  let property_list = b.property_list in
  List.fold_left
    (fun acc prop -> if acc || prop.position = pos then true else false )
    false property_list

let get_prop_name p = p.name

let get_prop_price p = p.cost

let get_position b p =
  List.nth b.tile_list p.position

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
                        properties= create_empty_prop_cont (); is_AI= false
                      } in
    id_counter := !id_counter + 1;
    temp_player_list := !temp_player_list@[temp_player]
  done;

  if human_players <> 4 then
    for i = 1 to 4 - human_players do
      let temp_player = { id= !id_counter; name= player_names.(!id_counter);
                        token= token_names.(!id_counter); position= ref 0;
                        properties= create_empty_prop_cont (); is_AI= false
                      } in
      id_counter := !id_counter + 1;
      temp_player_list := !temp_player_list@[temp_player]
    done

  {player_list= !temp_player_list; community_chest_list= ;
  chance_list= ; property_list= }

let get_player_list b =
  b.player_list

let get_player b pl_id =
  let pl_list = get_player_list b in
  List.nth pl_list (pl_id)

let get_property b p_name =
  let p_list = b.property_list in
  try Some(List.find (fun x -> x.name = p_name) p_list) with
  | Not_Found -> None

let get_chest b =
  let num_chest = List.length b.community_chest_list in
  let rand_num = Random.int num_chest in
  List.nth b.community_chest_list

let get_chance b =
  let num_chance = List.length b.chance_list in
  let rand_num = Random.int num_chance in
  List.nth b.chance_list

let change_money b pl_id amt =
  let pl = get_player b pl_id in
  pl.money := !pl.money + amt

let move_player b pl_id i =
  let pl = get_player b pl_id in
  let new_pos = !pl.position + i in
  if new_pos < pl.position then change_money b pl_id 200 else ();
  pl.position := () mod (List.length b.tile_list)

let move_property b pl_id prop =
  let pl = get_player b pl_id in
  match prop.color with
