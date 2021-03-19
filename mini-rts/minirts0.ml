(* Copyright (C) 2019 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

let width, height = (640, 480)

let green = (0, 255, 0)
let blue  = (40, 60, 255)
let bkgnd = (40, 40, 40)
let alpha = 255

module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
      | 0 -> Stdlib.compare y0 y1
      | c -> c
  end

module PairsMap = Map.Make(IntPairs)


let fill_rect renderer (x, y) (w, h) =
  let rect = Rect.make4 x y w h in
  Render.fill_rect renderer rect;
;;


let display renderer map entities =
  Render.set_draw_color renderer bkgnd alpha;
  Render.clear renderer;

  Render.set_draw_color renderer blue alpha;
  Array.iteri (fun y line ->
    Array.iteri (fun x cell ->
      if cell = 1 then
        let pos = (x * 16, y * 16) in
        fill_rect renderer pos (16, 16);
    ) line
  ) map;

  Render.set_draw_color renderer green alpha;
  PairsMap.iter (fun (x, y) entity ->
    let pos = (x * 16, y * 16) in
    fill_rect renderer pos (16, 16);
  ) entities;

  Render.render_present renderer;
;;


let proc_events = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> ()


let rec event_loop () =
  match Event.poll_event () with
  | None -> ()
  | Some ev ->
      let () = proc_events ev in
      event_loop ()


let () =
  Random.self_init ();
  Sdl.init [`VIDEO; `TIMER];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  let map_size = (40, 30) in
  let map =
    let x, y = map_size in
    Array.init y (fun y ->
      Array.init x (fun x ->
        match Random.int 40 with
        | 0 -> 1
        | _ -> 0
      )
    )
  in
  let is_full ((x, y) as pos) entities =
    (PairsMap.mem pos entities)
  in
  let is_obstacle (x, y) map =
    try (map.(y).(x) = 1)
    with _ -> true
  in

  let put_entities n =
    let w, h = map_size in
    let rec aux i entities =
      if i <= 0 then entities else
        let pos = Random.int w, Random.int h in
        if is_full pos entities
        || is_obstacle pos map
        then aux i entities
        else aux (i - 1) (PairsMap.add pos `Entity entities)
    in
    aux n PairsMap.empty
  in
  let entities = put_entities 50 in

  let step_entities entities =
    let moves = [| (1, 0); (-1, 0); (0, 1); (0, -1) |] in
    PairsMap.fold (fun ((x, y) as pos1) entity updated_entities ->
      let dx, dy = moves.(Random.int 4) in
      let pos2 = (x + dx, y + dy) in
      if is_obstacle pos2 map
      || is_full pos2 updated_entities
      || is_full pos2 entities
      then PairsMap.add pos1 entity updated_entities
      else PairsMap.add pos2 entity updated_entities
    ) entities PairsMap.empty
  in

  let rec main_loop entities =
    let () = event_loop () in
    let entities = step_entities entities in
    display renderer map entities;
    Timer.delay 200;
    main_loop entities
  in
  main_loop entities
