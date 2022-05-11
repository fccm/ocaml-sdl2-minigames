(* A minimalist game inspired from Sgnu's 9x17hell game
   https://sgnu.itch.io/9x17hell

 Copyright (C) 2022 Florent Monnier
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to modify it and
 redistribute it freely.
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
*)
open Sdl

let (gx, gy) as grid = (9, 17)
let cell_size = 20
let width, height = (gx * cell_size, gy * cell_size)

let red   = (230, 0, 0)
let blue  = (60, 60, 255)
let dark  = (60, 60, 60)
let alpha = 255


let fill_rect renderer color (x, y) =
  let x, y = (x * cell_size, y * cell_size) in
  let rect = Rect.make4 x y cell_size cell_size in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let display renderer debris pos_player =
  Render.set_draw_color renderer dark alpha;
  Render.clear renderer;
  fill_rect renderer blue pos_player;
  List.iter (fill_rect renderer red) debris;
  Render.render_present renderer;
;;


let proc_events dir = function
  | Event.KeyDown { Event.keycode = Keycode.Left } -> `left
  | Event.KeyDown { Event.keycode = Keycode.Right } -> `right
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> dir


let rec event_loop dir =
  match Event.poll_event () with
  | None -> dir
  | Some ev ->
      let dir = proc_events dir ev in
      event_loop dir


let new_debris () =
  (Random.int 9, 0)


let new_debris_opt debris =
  if Random.int 3 = 0
  then (new_debris ()) :: debris
  else debris


let step_debris debris =
  let debris = List.map (fun (x, y) -> (x, y + 1)) debris in
  let debris = List.filter (fun (x, y) -> (y < 17)) debris in
  let debris = new_debris_opt debris in
  (debris)


let () =
  Random.self_init ();
  Sdl.init [`VIDEO; `TIMER];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  let pos_player = (4, 16) in
  let debris = [] in

  let rec main_loop debris pos_player =
    let dir_player = event_loop `none in
    let pos_player =
      let x, y = pos_player in
      match dir_player with
      | `left  -> (x - 1, y)
      | `right -> (x + 1, y)
      | `none  -> (x, y)
    in
    let debris = step_debris debris in
    display renderer debris pos_player;
    Timer.delay 160;
    main_loop debris pos_player
  in
  main_loop debris pos_player
