(* A Minimalist Snake Game.
 Inspired from Gareth Halfacree's Raspberry Snake.
 Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

let width, height = (640, 480)

let red   = (255, 0, 0)
let blue  = (0, 0, 255)
let black = (0, 0, 0)
let alpha = 255

let fill_rect renderer (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.fill_rect renderer rect;
;;

let display renderer seg_snake pos_fruit =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;
  Render.set_draw_color renderer red alpha;
  fill_rect renderer pos_fruit;
  Render.set_draw_color renderer blue alpha;
  List.iter (fill_rect renderer) seg_snake;
  Render.render_present renderer;
;;

let proc_events dir_snake = function
  | Event.KeyDown { Event.keycode = Keycode.Left } -> `left
  | Event.KeyDown { Event.keycode = Keycode.Right } -> `right
  | Event.KeyDown { Event.keycode = Keycode.Up } -> `up
  | Event.KeyDown { Event.keycode = Keycode.Down } -> `down
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> (dir_snake)

let rec event_loop dir_snake =
  match Event.poll_event () with
  | None -> (dir_snake)
  | Some ev ->
      let dir = proc_events dir_snake ev in
      event_loop dir

let rec pop = function
  | [_] -> []
  | hd :: tl -> hd :: (pop tl)
  | [] -> invalid_arg "pop"

let rec new_pos_fruit seg_snake =
  let new_pos =
    (20 * Random.int 32,
     20 * Random.int 24)
  in
  if List.mem new_pos seg_snake
  then new_pos_fruit seg_snake
  else (new_pos)

let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  let pos_snake = (100, 100) in
  let seg_snake = [
    (100, 100);
    ( 80, 100);
    ( 60, 100);
  ] in
  let pos_fruit = (200, 200) in
  let dir_snake = `right in

  let rec main_loop pos_snake dir_snake seg_snake pos_fruit =
    let req_dir = event_loop dir_snake in
    let dir_snake =
      match dir_snake, req_dir with
      | `left, `right -> dir_snake
      | `right, `left -> dir_snake
      | `up, `down -> dir_snake
      | `down, `up -> dir_snake
      | _ -> req_dir
    in
    let pos_snake =
      let x, y = pos_snake in
      match dir_snake with
      | `left  -> (x - 20, y)
      | `right -> (x + 20, y)
      | `up    -> (x, y - 20)
      | `down  -> (x, y + 20)
    in
    let seg_snake = pos_snake :: seg_snake in
    let seg_snake, pos_fruit =
      if pos_snake = pos_fruit
      then (seg_snake, new_pos_fruit seg_snake)
      else (pop seg_snake, pos_fruit)
    in
    display renderer seg_snake pos_fruit;
    Timer.delay 100;
    main_loop pos_snake dir_snake seg_snake pos_fruit
  in
  main_loop pos_snake dir_snake seg_snake pos_fruit
