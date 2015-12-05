open GMain
open GdkKeysyms
open Async.Std
open Game_utils

(*USE THIS COMMAND TO BUILD THE GTK GUI*)
(*ocamlfind ocamlc -g -thread -package lablgtk2,async -linkpkg gtktest.ml -o gtktest*)

exception Gui_error of string;;

(*type property = { position: int;
                }
type player = { id: int;
               position: int ref;
             }

type board = { player_list: player list;
               property_list: property list;
             }*)

let locale = GtkMain.Main.init ()

let tilelocation = [(720,720);
                    (630,720);
                    (565,720);
                    (500,720);
                    (435,720);
                    (368,720);
                    (303,720);
                    (237,720);
                    (171,720);
                    (106,720);
                    (0  ,720);
                    (0  ,630);
                    (0  ,565);
                    (0  ,500);
                    (0  ,435);
                    (0  ,368);
                    (0  ,303);
                    (0  ,237);
                    (0  ,171);
                    (0  ,106);
                    (0  ,0  );
                    (106,0  );
                    (171,0  );
                    (237,0  );
                    (303,0  );
                    (368,0  );
                    (435,0  );
                    (500,0  );
                    (565,0  );
                    (630,0  );
                    (720,0  );
                    (720,106);
                    (720,171);
                    (720,237);
                    (720,303);
                    (720,368);
                    (720,435);
                    (720,500);
                    (720,565);
                    (720,630);]

(*let board_state = {
  player_list = [{id = 0; position = ref 5};{id = 1; position = ref 6};{id = 2; position = ref 7}];
  property_list = [{position = 8};{position = 8};{position = 8};{position = 8};
                {position = 13};{position = 13};{position = 13};{position = 13};
                {position = 26};{position = 26};{position = 26};{position = 26};
                {position = 37};{position = 37};{position = 37};{position = 37}];
}*)

(*--------------------------BEGINNING GUI FUNCTIONS---------------------------*)

let window = GWindow.window ~width:1200 ~height:830
                              ~title:"Monopoly" ()

let main_container = GPack.box `VERTICAL ~packing:window#add ()

(* Menu bar *)
let menubar = GMenu.menu_bar ~packing:main_container#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "Game"

(* Game menu *)
let factory = new GMenu.factory file_menu ~accel_group
let factory = new GMenu.factory file_menu ~accel_group

(*Set up the actual game area*)
let game_area = GPack.box `HORIZONTAL ~packing:main_container#add ()

let board = GPack.box `VERTICAL ~width:800
                                ~packing:game_area#add ()

let controls = GPack.box `VERTICAL ~width:400
                                   ~packing:game_area#add ()

let infoarea = GPack.box `VERTICAL ~packing:controls#add ()
                                  ~height:200

let commandarea = GPack.box `VERTICAL ~packing:controls#add ()
                                      ~height:600

let scrollingtext = GBin.scrolled_window  ~hpolicy:`NEVER
                                      ~vpolicy:`AUTOMATIC
                                      ~height:550
                                      ~packing:commandarea#add ()

let board_pixbuf = GdkPixbuf.from_file "assets/monopoly.jpg"
(*scaled_board_pixbuf is the static and constant board picture pixbuf*)
(*it's used as the base image to overlay stuff onto; houses, players, etc*)
let scaled_board_pixbuf = GdkPixbuf.create ~width:800
                                           ~height:800
                      ~bits:(GdkPixbuf.get_bits_per_sample board_pixbuf)
                      ~has_alpha:(GdkPixbuf.get_has_alpha board_pixbuf) ()

(*drawn_board_pixbuf is the actually drawn pixbuf*)
let drawn_board_pixbuf = GdkPixbuf.copy scaled_board_pixbuf
let board_image = GMisc.image ~pixbuf:drawn_board_pixbuf
                              ~width:800
                              ~height:800
                              ~packing:board#add ()

(*Load up the player avatars*)
let obama_pixbuf = GdkPixbuf.from_file "assets/obama.png"
let cena_pixbuf = GdkPixbuf.from_file "assets/cena.png"
let sanders_pixbuf = GdkPixbuf.from_file "assets/sanders.png"
let gaben_pixbuf = GdkPixbuf.from_file "assets/gaben.png"

(*Load up the property pictures*)
let house_pixbuf = GdkPixbuf.from_file "assets/black_house.png"

(* Buttons *)
(*let button = GButton.button ~label:"Push me!"
                            ~packing:buttons#add ()*)

(* Information display area *)
let moneydisplay = GText.view ~editable:false
                              ~cursor_visible:false
                              ~wrap_mode:`CHAR
                              ~show:true
                              ~packing:infoarea#add ()

(* Command input and display*)
let commanddisplay = GText.view ~editable:false
                              ~cursor_visible:false
                              ~wrap_mode:`CHAR
                              ~show:true
                              ~packing:scrollingtext#add ()

let commandinput = GEdit.entry ~editable:true
                              ~show:true
                              ~packing:commandarea#add ()

let print_to_cmd str =
  commanddisplay#buffer#insert ~iter:commanddisplay#buffer#end_iter str;
  scrollingtext#vadjustment#set_value
        (scrollingtext#vadjustment#upper -. scrollingtext#vadjustment#page_size +. 100.)

(*Helper variables and functions for readline, which is a blocking function*)
let waiting = ref (ref (Mutex.create ()))
let input_str = ref (ref "")

let readline waiting_ref string_ref =
  Mutex.lock (!waiting_ref);
  waiting := waiting_ref;
  input_str := string_ref

(*--------------------HELPER FUNCTIONS FOR UPDATING BOARD---------------------*)
(*Helper function for getting list of player at the given board pos*)
let players_at_pos b pos playerlst =
  List.fold_left
    (fun acc a -> if get_pl_position b (get_player_id a) = pos then a::acc else acc) [] playerlst

(*Helper function for drawing a list of players at a given physical pos*)
let draw_players physpos playerlst dest_pixbuf =
  let x = fst physpos in let y = snd physpos in
  List.iter (fun p ->
    GdkPixbuf.composite ~dest:dest_pixbuf
              ~alpha:200
              ~ofs_x: (float_of_int x)
              ~ofs_y: (float_of_int y)
              ~dest_x:x
              ~dest_y:y
              ~interp:`BILINEAR
              ~scale_x:0.5
              ~scale_y:0.5
              ~width:30
              ~height:30
              (if get_player_id p = 0 then obama_pixbuf
                else if get_player_id p = 1 then cena_pixbuf
                else if get_player_id p = 2 then sanders_pixbuf
                else if get_player_id p = 3 then gaben_pixbuf
                else raise (Gui_error "Invalid player ID"))) playerlst

(*Helper function for drawing a list of properties at a given physical pos*)
let draw_properties propertylst dest_pixbuf =
  (*Helper function to draw num_of_houses of houses at location physpos*)
  let drawhelper physpos owner_id num_of_houses =
    (*A pair that contains a physical pos pair and pos adjustment pair*)
    (*The pos adjustment pair is how much change in x and y position for
     *for each property added*)
    let physpos_and_adj =
      match physpos with
      | (x, y) when y = 720 -> ((x, y-20),(15,0))
      | (x, y) when x = 0   -> ((x+85, y),(0,15))
      | (x, y) when y = 0   -> ((x, y+85),(15,0))
      | (x, y) when x = 720 -> ((x-20, y),(0,15))
      | _ -> raise (Gui_error "draw property fail") in
    let prop_holder_pos =
      match physpos with
      | (x, y) when y = 720 -> (25,-20)
      | (x, y) when x = 0   -> (20,25)
      | (x, y) when y = 0   -> (25,20)
      | (x, y) when x = 720 -> (-20,25)
      | _ -> raise (Gui_error "draw property fail") in
    let x  = fst (fst physpos_and_adj) in
    let y  = snd (fst physpos_and_adj) in
    let dx = fst (snd physpos_and_adj) in
    let dy = snd (snd physpos_and_adj) in
    let xavatar = x + (fst prop_holder_pos) in
    let yavatar = y + (snd prop_holder_pos) in
    (GdkPixbuf.composite ~dest:dest_pixbuf
                ~alpha:200
                ~ofs_x: (float_of_int xavatar)
                ~ofs_y: (float_of_int yavatar)
                ~dest_x:xavatar
                ~dest_y:yavatar
                ~interp:`BILINEAR
                ~scale_x:0.25
                ~scale_y:0.25
                ~width:15
                ~height:15
                (if owner_id = 0 then obama_pixbuf
                  else if owner_id = 1 then cena_pixbuf
                  else if owner_id = 2 then sanders_pixbuf
                  else if owner_id = 3 then gaben_pixbuf
                  else raise (Gui_error "Invalid player ID")));
    let rec draw_houses pnum =
      (*Draw the houses*)
      if pnum = -1 then ()
      else
        (*Calculate the adjusted x and y positions*)
        let xhouse = x + pnum*dx in
        let yhouse = y + pnum*dy in
        (*Draw the properties onto the pixbuf*)
        (GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:255
                                              ~ofs_x: (float_of_int xhouse)
                                              ~ofs_y: (float_of_int yhouse)
                                              ~dest_x:xhouse
                                              ~dest_y:yhouse
                                              ~interp:`BILINEAR
                                              ~scale_x:0.25
                                              ~scale_y:0.25
                                              ~width:15
                                              ~height:15
                                              house_pixbuf);
        draw_houses (pnum - 1) in
    draw_houses (num_of_houses - 1) in

  let rec draw_prop_list tileloc proplst tile_num =
    match (tileloc, proplst) with
    | (l_hd::l_tl, p_hd::p_tl) ->
      (*Check if the current tile is a property*)
      if not (get_prop_position p_hd = tile_num) then
        draw_prop_list l_tl proplst (tile_num + 1)
      else
        (*Check if the current property is owned by anyone*)
        (match get_holder p_hd with
        | None -> draw_prop_list l_tl p_tl (tile_num + 1)
        | Some pid -> drawhelper l_hd pid (get_houses p_hd);
          draw_prop_list l_tl p_tl (tile_num + 1))
    | _ -> () in
  draw_prop_list tilelocation propertylst 0


let update_money curboard =
  let obama_mon = Printf.sprintf "Obama: $%d\n" (get_money curboard 0) in
  let cena_mon = Printf.sprintf "John Cena: $%d\n" (get_money curboard 1) in
  let sanders_mon = Printf.sprintf "Sanders: $%d\n" (get_money curboard 2) in
  let gaben_mon = Printf.sprintf "Gaben: $%d\n" (get_money curboard 3) in
  moneydisplay#buffer#set_text (obama_mon ^ cena_mon ^ sanders_mon ^ gaben_mon)

(*Callback function for updating the board pixbuf and drawing it in the GUI*)
let updateboard curboard =
  (*The pixbuf of the updated board*)
  let out_pixbuf = GdkPixbuf.copy scaled_board_pixbuf in
  draw_properties (get_property_list curboard) out_pixbuf;
  let rec draw_player_helper curpos poslist=
    match poslist with
    | hd::tl->
      let players = players_at_pos curboard curpos (get_player_list curboard) in
      (if players = [] then () else draw_players hd players out_pixbuf);
      draw_player_helper (curpos + 1) tl
    | [] -> () in
  (draw_player_helper 0 tilelocation); board_image#set_pixbuf out_pixbuf;
  update_money curboard

(*-----------------END OF HELPER FUNCTIONS FOR UPDATING BOARD-----------------*)

let main () =
  (*In main function, we connect the callback functions and finish setting up*)
  let _ = window#connect#destroy ~callback:Main.quit in

  (* Game menu set up*)
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in
  let _ = factory#add_item "Restart" ~key:_R ~callback: Main.quit in

  (*Create and scale the board image*)
  let _ = GdkPixbuf.scale ~dest:scaled_board_pixbuf ~width:800
                                            ~height:800
                                            ~interp:`BILINEAR
                                            board_pixbuf in
  (*Draw the board; we use scaled_board here because drawn_board has not been
   *properly initialized yet*)
  let _ = board_image#set_pixbuf scaled_board_pixbuf in

  (* Button *)
  (*let _ = button#connect#clicked ~callback: (
      fun () -> board_image#set_pixbuf scaled_board_pixbuf) in*)

  (* Command input and display*)
  let _ = commandinput#connect#activate ~callback: (
    fun () -> print_to_cmd (commandinput#text ^ "\n");
      (!input_str) := commandinput#text;
      commandinput#set_text "";
      Mutex.unlock !(!waiting)) in

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  GtkThread.busy_waiting := true;
  GtkThread.main ()

(*let () = main ()*)