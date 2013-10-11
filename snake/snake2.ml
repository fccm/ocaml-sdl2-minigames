(* A Simple Snake Game.
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

type pos = int * int
type dir = [`left | `right | `up | `down]

type snake_state = {
  snake_pos: pos;
  snake_seg: pos list;
  snake_dir: dir;
  req_dir: dir option;
  dir_keys: (Keycode.t * dir) list;
  alive: bool;
}

type game_state = {
  snakes: snake_state list;
  pos_fruit: pos;
  sleep_time: int;
  game_over: bool;
}


let new_dir = function
  | `left  -> [| `up; `down |].(Random.int 2)
  | `right -> [| `up; `down |].(Random.int 2)
  | `up    -> [| `left; `right |].(Random.int 2)
  | `down  -> [| `left; `right |].(Random.int 2)


let red   = (255, 0, 0)
let blue  = (0, 0, 255)
let green = (0, 255, 0)
let black = (0, 0, 0)
let grey  = (32, 32, 32)
let alpha = 255

let fill_rect renderer (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.fill_rect renderer rect;
;;


let display_game renderer state =
  let bg_color, snake_color, fruit_color =
    if state.game_over
    then (red, black, green)
    else (black, blue, red)
  in
  Render.set_draw_color renderer bg_color alpha;
  Render.clear renderer;
  Render.set_draw_color renderer fruit_color alpha;
  fill_rect renderer state.pos_fruit;
  List.iter (fun snake ->
    if snake.alive
    then Render.set_draw_color renderer snake_color alpha
    else Render.set_draw_color renderer grey alpha;
    List.iter (fill_rect renderer) snake.snake_seg;
  ) state.snakes;
  Render.render_present renderer;
;;


let proc_events snakes = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | Event.KeyDown { Event.keycode } ->
      List.map (fun snake ->
        let req_dir =
          try Some (List.assoc keycode snake.dir_keys)
          with Not_found -> None
        in
        { snake with req_dir }
      ) snakes
  | _ -> (snakes)


let rec event_loop snakes =
  match Event.poll_event () with
  | None -> (snakes)
  | Some ev ->
      let snakes = proc_events snakes ev in
      event_loop snakes


let rec pop = function
  | [_] -> []
  | hd :: tl -> hd :: (pop tl)
  | [] -> invalid_arg "pop"


let rec new_pos_fruit segs =
  let new_pos =
    (20 * Random.int 32,
     20 * Random.int 24)
  in
  if List.mem new_pos segs
  then new_pos_fruit segs
  else (new_pos)


let fold_slide f v lst =
  let rec aux v acc = function
  | x::xs ->
      let v = f v x (List.rev_append acc xs) in
      aux v (x::acc) xs
  | [] -> v
  in
  aux v [] lst


let update_state ({
    snakes;
    pos_fruit;
    sleep_time;
    game_over;
  } as state) =
  if game_over then state else
  let snakes, pos_fruit, sleep_time =
    fold_slide (fun (acc, pos_fruit, sleep_time) ({
        snake_pos;
        snake_seg;
        snake_dir;
        req_dir;
        dir_keys;
        alive;
      } as snake)
        other_snakes ->
      if not alive
      then ((snake::acc), pos_fruit, sleep_time) else
      let snake_dir =
        match snake_dir, req_dir with
        | `left, Some `right -> snake_dir
        | `right, Some `left -> snake_dir
        | `up, Some `down -> snake_dir
        | `down, Some `up -> snake_dir
        | snake_dir, None -> snake_dir
        | _, Some req_dir -> req_dir
      in
      let snake_pos =
        let x, y = snake_pos in
        match snake_dir with
        | `left  -> (x - 20, y)
        | `right -> (x + 20, y)
        | `up    -> (x, y - 20)
        | `down  -> (x, y + 20)
      in
      let segs = List.map (fun snake -> snake.snake_seg) other_snakes in
      let segs = List.flatten (snake_seg::segs) in
      let alive =
        let x, y = snake_pos in
        not (
          List.mem snake_pos segs
          || x < 0 || y < 0
          || x >= width
          || y >= height)
      in
      if not alive
      then (({ snake with alive }::acc), pos_fruit, sleep_time) else
      let snake_seg = snake_pos :: snake_seg in
      let snake_seg, pos_fruit, sleep_time =
        if snake_pos <> pos_fruit
        then (pop snake_seg, pos_fruit, sleep_time)
        else
          let segs = List.map (fun snake -> snake.snake_seg) other_snakes in
          let segs = List.flatten (snake_seg::segs) in
          (snake_seg, new_pos_fruit segs, sleep_time - 1)
      in
      let snake = {
        snake_pos;
        snake_seg;
        snake_dir;
        req_dir;
        dir_keys;
        alive;
      } in
      ((snake::acc), pos_fruit, sleep_time)
    )
    ([], pos_fruit, sleep_time)
    snakes
  in
  let game_over =
    List.for_all (fun snake -> not snake.alive) snakes
  in
  { snakes;
    pos_fruit;
    sleep_time;
    game_over;
  }


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  Window.set_title ~window ~title:"Snake OCaml-SDL2";
  let snake_a = {
    snake_pos = (100, 100);
    snake_seg = [
      (100, 100);
      ( 80, 100);
      ( 60, 100);
    ];
    snake_dir = `right;
    alive = true;
    dir_keys = [
      (Keycode.Left, `left);
      (Keycode.Right, `right);
      (Keycode.Up, `up);
      (Keycode.Down, `down);
    ];
    req_dir = None;
  } in
  let snake_b = {
    snake_pos = (540, 380);
    snake_seg = [
      (540, 380);
      (560, 380);
      (580, 380);
    ];
    snake_dir = `left;
    alive = true;
    dir_keys = [
      (Keycode.D, `left);
      (Keycode.G, `right);
      (Keycode.R, `up);
      (Keycode.F, `down);
    ];
    req_dir = None;
  } in
  let initial_state = {
    snakes = [snake_a; snake_b];
    pos_fruit = (200, 200);
    sleep_time = 120;
    game_over = false;
  } in

  let rec main_loop state =
    let snakes = event_loop state.snakes in
    let state = { state with snakes } in
    let state = update_state state in
    display_game renderer state;
    Timer.delay state.sleep_time;
    main_loop state
  in
  main_loop initial_state
