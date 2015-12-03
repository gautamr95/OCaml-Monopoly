open GMain
open GdkKeysyms

type property = { position: int;
                }
type player = { id: int;
               position: int ref;
             }

type board = { player_list: player list;
               property_list: property list;
             }

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

let board_state = {
  player_list = [{id = 0; position = ref 5};{id = 1; position = ref 6}];
  property_list = [{position = 8};{position = 8};{position = 8};{position = 8}];
}

let main () =
  let window = GWindow.window ~width:1200 ~height:830
                              ~title:"Simple lablgtk program" () in
  let main_container = GPack.box `VERTICAL ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:main_container#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "Game" in

  (* Game menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Restart" ~key:_R ~callback: Main.quit;

  (*Set up the actual game area*)
  let game_area = GPack.box `HORIZONTAL ~packing:main_container#add () in
  window#connect#destroy ~callback:Main.quit;

  let board = GPack.box `VERTICAL ~width:800
                                  ~packing:game_area#add () in

  let controls = GPack.box `VERTICAL ~width:400
                                     ~packing:game_area#add () in

  let buttons = GPack.box `VERTICAL ~packing:controls#add () in

  let commandarea = GPack.box `VERTICAL ~packing:controls#add () in

  let scrollingtext = GBin.scrolled_window  ~hpolicy:`NEVER
                                        ~vpolicy:`AUTOMATIC
                                        ~packing:commandarea#add () in

  (*Create board image*)
  let board_pixbuf = GdkPixbuf.from_file "assets/monopoly.jpg" in
  (*scaled_board_pixbuf is the static and constant board picture pixbuf*)
  (*it's used as the base image to overlay stuff onto; houses, players, etc*)
  let scaled_board_pixbuf = GdkPixbuf.create ~width:800
                                             ~height:800
                        ~bits:(GdkPixbuf.get_bits_per_sample board_pixbuf)
                        ~has_alpha:(GdkPixbuf.get_has_alpha board_pixbuf) () in
  GdkPixbuf.scale ~dest:scaled_board_pixbuf ~width:800
                                            ~height:800
                                            ~interp:`BILINEAR
                                            board_pixbuf;
  (*drawn_board_pixbuf is the actually drawn pixbuf*)
  let drawn_board_pixbuf = GdkPixbuf.copy scaled_board_pixbuf in
  let board_image = GMisc.image ~pixbuf:drawn_board_pixbuf
                                ~width:800
                                ~height:800
                                ~packing:board#add () in

  let obama_pixbuf = GdkPixbuf.from_file "assets/obama.png" in
  let cena_pixbuf = GdkPixbuf.from_file "assets/cena.png" in
  let sanders_pixbuf = GdkPixbuf.from_file "assets/sanders.png" in
  let house_pixbuf = GdkPixbuf.from_file "assets/black_house.png" in

  (*-----------------HELPER FUNCTIONS FOR UPDATING BOARD-----------------*)
  (*Helper function for getting list of properties at the given board pos*)
  let properties_at_pos pos (proplst:property list) =
    List.fold_left
      (fun acc (a:property) -> if a.position = pos then a::acc else acc) [] proplst in

  (*Helper function for getting list of player at the given board pos*)
  let players_at_pos pos playerlst =
    List.fold_left
      (fun acc a -> if !(a.position) = pos then a::acc else acc) [] playerlst in

  (*Helper function for drawing a list of players at a given physical pos*)
  let draw_players physpos playerlst dest_pixbuf =
    let x = fst physpos in let y = snd physpos in
    List.iter (fun p ->
      if p.id = 0 then
        GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:200
                                              ~ofs_x: (float_of_int x)
                                              ~ofs_y: (float_of_int y)
                                              ~dest_x:x
                                              ~dest_y:y
                                              ~interp:`BILINEAR
                                              ~scale_x:0.5
                                              ~scale_y:0.5
                                              ~width:30
                                              ~height:30
                                              obama_pixbuf
      else
        GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:200
                                              ~ofs_x: (float_of_int x)
                                              ~ofs_y: (float_of_int y)
                                              ~dest_x:x
                                              ~dest_y:y
                                              ~interp:`BILINEAR
                                              ~scale_x:0.5
                                              ~scale_y:0.5
                                              ~width:30
                                              ~height:30
                                              cena_pixbuf) playerlst in

  (*Helper function for drawing a list of properties at a given physical pos*)
  let draw_properties physpos proplst dest_pixbuf =
    let x = fst physpos in let y = (snd physpos) - 20 in
    let rec propdraw_helper pnum plst =
      match plst with
      | [] -> ()
      | hd::tl ->
        let xadj = x + pnum*15 in
        (GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:255
                                              ~ofs_x: (float_of_int xadj)
                                              ~ofs_y: (float_of_int y)
                                              ~dest_x:xadj
                                              ~dest_y:y
                                              ~interp:`BILINEAR
                                              ~scale_x:0.25
                                              ~scale_y:0.25
                                              ~width:15
                                              ~height:15
                                              house_pixbuf); propdraw_helper (pnum + 1) tl in
    propdraw_helper 0 proplst in

  (*Callback function for updating the board pixbuf in the GUI*)
  let updateboard curboard: GdkPixbuf.pixbuf =
    (*The pixbuf of the updated board*)
    let out_pixbuf = GdkPixbuf.copy scaled_board_pixbuf in
    let rec drawhelper curpos poslist=
      match poslist with
      | hd::tl->
        let players = players_at_pos curpos curboard.player_list in
        let props = properties_at_pos curpos curboard.property_list in
        (if players = [] then () else draw_players hd players out_pixbuf);
        (if props = [] then () else draw_properties hd props out_pixbuf);
        drawhelper (curpos + 1) tl
      | [] -> () in
    (drawhelper 0 tilelocation); out_pixbuf in

  (* Button *)
  let button = GButton.button ~label:"Push me!"
                              ~packing:buttons#add () in
  button#connect#clicked ~callback: (
    fun () -> board_image#set_pixbuf scaled_board_pixbuf);

  (* Button2 *)
  let button2 = GButton.button ~label:"Push me2!"
                              ~packing:buttons#add () in
  button2#connect#clicked ~callback: (
    fun () -> let drawn_board_pixbuf = updateboard board_state in
      board_image#set_pixbuf drawn_board_pixbuf);

  (* Toggle Button *)
  let togglebutton = GButton.toggle_button ~label:"Toggle me!"
                              ~active: false
                              ~draw_indicator: true
                              ~packing:buttons#add () in
  togglebutton#connect#enter ~callback: (fun () -> prerr_endline "Entered!");
  togglebutton#connect#leave ~callback: (fun () -> prerr_endline "Left!");

  (* Command input and display*)
  let commanddisplay = GText.view ~editable:false
                                ~cursor_visible:false
                                ~wrap_mode:`CHAR
                                ~show:true
                                ~packing:scrollingtext#add () in

  let commandinput = GEdit.entry ~editable:true
                                ~show:true
                                ~packing:commandarea#add () in
  commandinput#connect#activate ~callback: (
    fun () -> commanddisplay#buffer#insert ~iter:commanddisplay#buffer#end_iter
                                          (commandinput#text ^ "\n");
      scrollingtext#vadjustment#set_value
        (scrollingtext#vadjustment#upper -. scrollingtext#vadjustment#page_size);
      commandinput#set_text "");


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()