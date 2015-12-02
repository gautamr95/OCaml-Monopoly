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
               name: string;
               token: string;
               position: int ref;
               properties: property_container;
               is_AI: bool
             }

and property_container = { blue: property list ref;
                           green: property list ref;
                           yellow: property list ref;
                           orange: property list ref;
                           black: property list ref
                         }

type community_chest = string * int

type chance = string * int

type board = { player_list: player list;
               community_chest_list: community_chest list ref;
               chance_list: chance list ref;
               property_list: property list
             }

let in_jail p = p.position = jail_position

let is_property b pos =
  let property_list = b.property_list in
  List.fold_left
    (fun acc prop -> if acc || prop.position = pos then true else false )
    false property_list

let get_prop_name p = p.name

let get_prop_price p = p.cost

let get_position p = p.position

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




