(* An experimental Snake Game.
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

type keyboard_control =
  (Keycode.t * dir) list

type ai_script =
  (snake_state -> snake_state list -> game_state -> dir option)

and snake_control =
  | Human of keyboard_control
  | Script of ai_script

and snake_state = {
  snake_name: string;
  snake_pos: pos;
  snake_seg: pos list;
  snake_dir: dir;
  req_dir: dir option;
  alive: bool;
  control: snake_control;
}

and game_state = {
  snakes: snake_state list;
  fruit_pos: pos;
  sleep_time: int;
  game_over: bool;
}


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
  fill_rect renderer state.fruit_pos;
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
        match snake.control with
        | Human dir_keys ->
            let req_dir =
              try Some (List.assoc keycode dir_keys)
              with Not_found -> None
            in
            { snake with req_dir }
        | Script _ -> snake
      ) snakes
  | _ -> (snakes)


let rec event_loop snakes =
  match Event.poll_event () with
  | None -> (snakes)
  | Some ev ->
      let snakes = proc_events snakes ev in
      event_loop snakes


let snake_next_pos ~snake_pos:(x, y) ~snake_dir =
  match snake_dir with
  | `left  -> (x - 20, y)
  | `right -> (x + 20, y)
  | `up    -> (x, y - 20)
  | `down  -> (x, y + 20)


let new_dir = function
  | `left  -> [| `up; `down |].(Random.int 2)
  | `right -> [| `up; `down |].(Random.int 2)
  | `up    -> [| `left; `right |].(Random.int 2)
  | `down  -> [| `left; `right |].(Random.int 2)


let snake_ai change_dir_prob snake state =
  let x, y = snake.snake_pos in
  match x = 0, x >= width - 20,
        y = 0, y >= height - 20, snake.snake_dir with
  (* when in the corners *)
  | true, _, true, _, `left -> Some (`down)
  | true, _, true, _, `up -> Some (`right)
  | _, true, _, true, `right -> Some (`up)
  | _, true, _, true, `down -> Some (`left)
  | true, _, _, true, `down -> Some (`right)
  | true, _, _, true, `left -> Some (`up)
  | _, true, true, _, `right -> Some (`down)
  | _, true, true, _, `up -> Some (`left)
  (* when going to the borders *)
  | true, _, _, _, `left -> Some (new_dir snake.snake_dir)
  | _, true, _, _, `right -> Some (new_dir snake.snake_dir)
  | _, _, true, _, `up -> Some (new_dir snake.snake_dir)
  | _, _, _, true, `down -> Some (new_dir snake.snake_dir)
  (* when along the borders *)
  | true, _, _, _, `up
  | true, _, _, _, `down ->
      if Random.float 1.0 < change_dir_prob
      then Some (`right) else None
  | _, true, _, _, `up
  | _, true, _, _, `down ->
      if Random.float 1.0 < change_dir_prob
      then Some (`left) else None
  | _, _, true, _, `right
  | _, _, true, _, `left ->
      if Random.float 1.0 < change_dir_prob
      then Some (`down) else None
  | _, _, _, true, `right
  | _, _, _, true, `left ->
      if Random.float 1.0 < change_dir_prob
      then Some (`up) else None
  (* anywhere in the middle *)
  | _, _, _, _, `left
  | _, _, _, _, `right ->
      let _x, _y = state.fruit_pos in
      if y = _y then None else
      if y < _y then Some (`down) else Some (`up)
  | _, _, _, _, `up
  | _, _, _, _, `down ->
      let _x, _y = state.fruit_pos in
      if x = _x then None else
      if x < _x then Some (`right) else Some (`left)
    (*
  | _ ->
      if Random.float 1.0 < change_dir_prob
      then Some (new_dir snake.snake_dir)
      else None
    *)


let snake_ai change_dir_prob snake all_snakes state =
  let segs = List.map (fun snake -> snake.snake_seg) all_snakes in
  let segs = List.flatten segs in
  let next_pos = snake_next_pos snake.snake_pos snake.snake_dir in
  match
    snake.snake_dir,
    snake_ai change_dir_prob snake state,
    List.mem next_pos segs
  with
  | _, req_dir, false -> (req_dir)
  | dir, Some (`up), true -> (None)
  | dir, Some (`down), true -> (None)
  | dir, Some (`left), true -> (None)
  | dir, Some (`right), true -> (None)
  | `up, None, true -> Some (new_dir snake.snake_dir)
  | `down, None, true -> Some (new_dir snake.snake_dir)
  | `left, None, true -> Some (new_dir snake.snake_dir)
  | `right, None, true -> Some (new_dir snake.snake_dir)


let ai_control state all_snakes snake =
  match snake.control with
  | Human _ -> snake
  | Script f ->
      { snake with req_dir = f snake all_snakes state }


let ai_controls state snakes =
  List.map (ai_control state snakes) snakes


let rec pop = function
  | [_] -> []
  | hd :: tl -> hd :: (pop tl)
  | [] -> invalid_arg "pop"


let rec new_fruit_pos segs =
  let new_pos =
    (20 * Random.int 32,
     20 * Random.int 24)
  in
  if List.mem new_pos segs
  then new_fruit_pos segs
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
    fruit_pos;
    sleep_time;
    game_over;
  } as state) =
  if game_over then state else
  let snakes, fruit_pos, sleep_time =
    fold_slide (fun (acc, fruit_pos, sleep_time) ({
        snake_name;
        snake_pos;
        snake_seg;
        snake_dir;
        req_dir;
        control;
        alive;
      } as snake)
        other_snakes ->
      if not alive
      then ((snake::acc), fruit_pos, sleep_time) else
      let snake_dir =
        match snake_dir, req_dir with
        | `left, Some `right -> snake_dir
        | `right, Some `left -> snake_dir
        | `up, Some `down -> snake_dir
        | `down, Some `up -> snake_dir
        | snake_dir, None -> snake_dir
        | _, Some req_dir -> req_dir
      in
      let snake_pos = snake_next_pos snake_pos snake_dir in
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
      let () = if not alive then Printf.printf "%s died\n%!" snake_name in
      if not alive
      then (({ snake with alive }::acc), fruit_pos, sleep_time) else
      let snake_seg = snake_pos :: snake_seg in
      let snake_seg, fruit_pos, sleep_time =
        if snake_pos <> fruit_pos
        then (pop snake_seg, fruit_pos, sleep_time)
        else
          let segs = List.map (fun snake -> snake.snake_seg) other_snakes in
          let segs = List.flatten (snake_seg::segs) in
          (snake_seg, new_fruit_pos segs, sleep_time - 1)
      in
      let snake = {
        snake_name;
        snake_pos;
        snake_seg;
        snake_dir;
        req_dir;
        control;
        alive;
      } in
      ((snake::acc), fruit_pos, sleep_time)
    )
    ([], fruit_pos, sleep_time)
    snakes
  in
  let game_over =
    List.for_all (fun snake -> not snake.alive) snakes
  in
  { snakes;
    fruit_pos;
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
    snake_name = "A";
    snake_pos = (100, 100);
    snake_seg = [
      (100, 100);
      ( 80, 100);
      ( 60, 100);
    ];
    snake_dir = `right;
    req_dir = None;
    alive = true;
    control =
      (*
      Script (snake_ai 0.1);
      *)
      Human [
        (Keycode.Left, `left);
        (Keycode.Right, `right);
        (Keycode.Up, `up);
        (Keycode.Down, `down);
      ];
  } in
  let snake_b = {
    snake_name = "B 0.1";
    snake_pos = (540, 380);
    snake_seg = [
      (540, 380);
      (560, 380);
      (580, 380);
    ];
    snake_dir = `left;
    req_dir = None;
    alive = true;
    control =
      Script (snake_ai 0.1);
      (*
      Human [
        (Keycode.D, `left);
        (Keycode.G, `right);
        (Keycode.R, `up);
        (Keycode.F, `down);
      ];
      *)
  } in
  let snake_c = {
    snake_name = "C 0.2";
    snake_pos = (100, 380);
    snake_seg = [
      (100, 380);
      ( 80, 380);
      ( 60, 380);
    ];
    snake_dir = `up;
    req_dir = None;
    alive = true;
    control =
      Script (snake_ai 0.2);
  } in
  let snake_d = {
    snake_name = "D 0.3";
    snake_pos = (540, 100);
    snake_seg = [
      (540, 100);
      (560, 100);
      (580, 100);
    ];
    snake_dir = `down;
    req_dir = None;
    alive = true;
    control =
      Script (snake_ai 0.3);
  } in
  (*
  *)

  let snakes = [snake_a; snake_b; snake_c; snake_d] in
  (*
  let snakes = [snake_a] in
  let snakes = [snake_a; snake_b] in
  *)

  let segs = List.map (fun snake -> snake.snake_seg) snakes in
  let segs = List.flatten segs in
  let fruit_pos = new_fruit_pos segs in

  let initial_state = {
    snakes;
    fruit_pos;
    sleep_time = 120;
    game_over = false;
  } in

  let rec main_loop state =
    let snakes = event_loop state.snakes in
    let snakes = ai_controls state snakes in
    let state = { state with snakes } in
    let state = update_state state in
    display_game renderer state;
    Timer.delay state.sleep_time;
    main_loop state
  in
  main_loop initial_state
