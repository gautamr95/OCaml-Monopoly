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
                    (635,720);
                    (570,720);
                    (505,720);
                    (440,720);
                    (375,720);
                    (310,720);
                    (240,720);
                    (175,720);
                    (110,720);
                    (20 ,710);
                    (20 ,625);
                    (20 ,560);
                    (20 ,495);
                    (20 ,430);
                    (20 ,365);
                    (20 ,300);
                    (20 ,230);
                    (20 ,165);
                    (20 ,100);
                    (20 ,20 );
                    (110,20 );
                    (175,20 );
                    (240,20 );
                    (310,20 );
                    (375,20 );
                    (440,20 );
                    (505,20 );
                    (570,20 );
                    (635,20 );
                    (740,20 );
                    (740,100);
                    (740,165);
                    (740,230);
                    (740,300);
                    (740,365);
                    (740,430);
                    (740,495);
                    (740,560);
                    (740,625);]

let board_state = {
  player_list = [{id = 0; position = ref 5};{id = 1; position = ref 6}];
  property_list = [{position = 7}];
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
  let board_pixbuf = GdkPixbuf.from_file "monopoly.jpg" in
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

  let obama_pixbuf = GdkPixbuf.from_file "face.png" in
  let cena_pixbuf = GdkPixbuf.from_file "cena.png" in
  let green_house_pixbuf = GdkPixbuf.from_file "green_house.png" in

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
                                              ~scale_x:0.1
                                              ~scale_y:0.1
                                              ~width:28
                                              ~height:38
                                              obama_pixbuf
      else
        GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:200
                                              ~ofs_x: (float_of_int x)
                                              ~ofs_y: (float_of_int y)
                                              ~dest_x:x
                                              ~dest_y:y
                                              ~interp:`BILINEAR
                                              ~scale_x:0.075
                                              ~scale_y:0.075
                                              ~width:47
                                              ~height:60
                                              cena_pixbuf) playerlst in

  (*Helper function for drawing a list of properties at a given physical pos*)
  let draw_properties physpos proplst dest_pixbuf =
    let x = fst physpos in let y = snd physpos in
    List.iter (fun p ->
      GdkPixbuf.composite ~dest:dest_pixbuf ~alpha:200
                                            ~ofs_x: (float_of_int x)
                                            ~ofs_y: (float_of_int y)
                                            ~dest_x:x
                                            ~dest_y:y
                                            ~interp:`BILINEAR
                                            ~scale_x:0.015
                                            ~scale_y:0.015
                                            ~width:15
                                            ~height:12
                                            green_house_pixbuf) proplst in

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