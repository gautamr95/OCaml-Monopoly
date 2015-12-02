open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let listofcrap = [0;1;2;3;0;1;2;3;0;1;2;3;
                  0;1;2;3;0;1;2;3;0;1;2;3;
                  0;1;2;3;0;1;2;3;0;1;2;3;
                  0;1;2;3]

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
  let fuck_menu = factory#add_submenu "fuck" in

  (* Game menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Restart" ~key:_R ~callback: Main.quit;

  (* Fuck menu *)
  let factory = new GMenu.factory fuck_menu ~accel_group in
  factory#add_item "fuck it" ~key:_Q ~callback: Main.quit;

  (*Set up the actual game area*)

  let game_area = GPack.box `HORIZONTAL ~packing:main_container#add () in
  window#connect#destroy ~callback:Main.quit;

  let board = GPack.box `VERTICAL ~width:800
                                  ~packing:game_area#add () in
  window#connect#destroy ~callback:Main.quit;

  let controls = GPack.box `VERTICAL ~width:400
                                     ~packing:game_area#add () in
  window#connect#destroy ~callback:Main.quit;

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

  let drawspirites xpos ypos : GdkPixbuf.pixbuf =
    let out_pixbuf = GdkPixbuf.copy scaled_board_pixbuf in
    let rec drawhelper spiritlist=
      match spiritlist with
      | (shd::stl,lhd::ltl) ->
        let x = fst lhd in let y = snd lhd in
        (if shd = 0 then
          GdkPixbuf.composite ~dest:out_pixbuf ~alpha:200
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
          GdkPixbuf.composite ~dest:out_pixbuf ~alpha:200
                                                ~ofs_x: (float_of_int x)
                                                ~ofs_y: (float_of_int y)
                                                ~dest_x:x
                                                ~dest_y:y
                                                ~interp:`BILINEAR
                                                ~scale_x:0.075
                                                ~scale_y:0.075
                                                ~width:47
                                                ~height:60
                                                cena_pixbuf);
        drawhelper (stl,ltl)
      | _ -> () in
    (drawhelper (listofcrap, tilelocation)); out_pixbuf in

  (* Button *)
  let button = GButton.button ~label:"Push me!"
                              ~packing:controls#add () in
  button#connect#clicked ~callback: (fun () -> board_image#set_pixbuf scaled_board_pixbuf);

  Random.init 1738;

  (* Button2 *)
  let button2 = GButton.button ~label:"Push me2!"
                              ~packing:controls#add () in
  button2#connect#clicked ~callback: (
    fun () -> let drawn_board_pixbuf = drawspirites 0 0 in
      board_image#set_pixbuf drawn_board_pixbuf);

  (* Toggle Button *)
  let togglebutton = GButton.toggle_button ~label:"Toggle me!"
                              ~active: false
                              ~draw_indicator: true
                              ~packing:controls#add () in
  togglebutton#connect#enter ~callback: (fun () -> prerr_endline "Entered!");
  togglebutton#connect#leave ~callback: (fun () -> prerr_endline "Left!");

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()