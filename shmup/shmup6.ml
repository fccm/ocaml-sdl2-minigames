(* A Minimalist Shmup Game
 Copyright (C) 2019 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

type foe = {
  foe_pos: int * int;
  foe_last_shot: int;
  foe_shoot_freq: int;
}

type bullet = {
  bullet_pos: int * int;
  bullet_line: (int * int) * (int * int);
  bullet_birth: int;
}

type player_dir = {
  left: bool;
  right: bool;
  up: bool;
  down: bool;
}

type player = {
  p_pos: int * int;
  p_last_shot: int;
  p_shoot_freq: int;
  p_shooting: bool;
  p_dir: player_dir;
}

let width, height = (640, 480)

let red    = (255, 0, 0)
let blue   = (0, 0, 255)
let yellow = (255, 255, 0)
let orange = (255, 127, 0)
let black  = (0, 0, 0)
let alpha  = 255


let fill_rect renderer color (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let display renderer bg_color player f_bullets foes =
  Render.set_draw_color renderer bg_color alpha;
  Render.clear renderer;
  List.iter (fun bullet -> fill_rect renderer yellow bullet.bullet_pos) f_bullets;
  List.iter (fun foe -> fill_rect renderer red foe.foe_pos) foes;
  fill_rect renderer blue player.p_pos;
  Render.render_present renderer;
;;


let proc_events player = function
  | Event.KeyDown { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = true } }
  | Event.KeyDown { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = true } }
  | Event.KeyDown { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = true } }
  | Event.KeyDown { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = true } }

  | Event.KeyUp { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = false } }
  | Event.KeyUp { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = false } }
  | Event.KeyUp { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = false } }
  | Event.KeyUp { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = false } }

  | Event.KeyDown { Event.keycode = Keycode.Z } ->
      { player with p_shooting = true }
  | Event.KeyUp { Event.keycode = Keycode.Z } ->
      { player with p_shooting = false }

  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0

  | _ -> player


let rec event_loop player =
  match Event.poll_event () with
  | None -> player
  | Some ev ->
      let player = proc_events player ev in
      event_loop player


let f_bullet_inside bullet =
  let x, y = bullet.bullet_pos in
  (y < height) &&
  (x < width) &&
  (y > -20) &&
  (x > -20)


let vec_mul (x, y) k =
  (x * k,
   y * k)

let vec_div (x, y) k =
  (x / k,
   y / k)

let vec_add (ax, ay) (bx, by) =
  (ax + bx,
   ay + by)

let point_on_line (p1, p2) i t =
  let ti = i - t in
  vec_div (
      vec_add
        (vec_mul p1 ti)
        (vec_mul p2 t)
    ) i


let step_foes_bullets f_bullets t =
  let step_bullet bullet =
    let dt = t - bullet.bullet_birth in
    let p = point_on_line bullet.bullet_line 6000 dt in
    { bullet with bullet_pos = p }
  in
  let f_bullets = List.map step_bullet f_bullets in
  let f_bullets = List.filter f_bullet_inside f_bullets in
  (f_bullets)


let new_foe t =
  let foe_pos = (20 * Random.int (width / 20), -20) in
  let foe_last_shot = t in
  let foe_shoot_freq = 1600 + Random.int 1800 in
  { foe_pos; foe_last_shot; foe_shoot_freq }


let new_foes_opt foes t =
  if Random.int 100 > 2
  then foes
  else
    let new_foe = new_foe t in
    new_foe :: foes


let gun_new_f_bullets f_bullets foes player t =
  let rec aux acc1 acc2 foes =
    match foes with
    | [] -> (acc1, acc2)
    | foe :: foes ->
        if t - foe.foe_last_shot < foe.foe_shoot_freq
        then aux acc1 (foe :: acc2) foes
        else
          let updated_foe = { foe with foe_last_shot = t } in
          let bullet =
            { bullet_pos = foe.foe_pos;
              bullet_line = (foe.foe_pos, player.p_pos);
              bullet_birth = t; }
          in
          aux (bullet :: acc1) (updated_foe :: acc2) foes
  in
  let new_f_bullets, foes = aux [] [] foes in
  let f_bullets = List.rev_append new_f_bullets f_bullets in
  (f_bullets, foes)


let foe_inside foe =
  let (x, y) = foe.foe_pos in
  (y < height)


let step_foes foes f_bullets player t =
  let step_foe foe =
    let (x, y) = foe.foe_pos in
    let new_pos = (x, y + 2) in
    { foe with foe_pos = new_pos }
  in
  let foes = new_foes_opt foes t in
  let f_bullets, foes = gun_new_f_bullets f_bullets foes player t in
  let foes = List.map step_foe foes in
  let foes = List.filter foe_inside foes in
  (foes, f_bullets)


let player_touched player f_bullets =
  let x, y = player.p_pos in
  let player_rect = Rect.make4 x y 20 20 in
  List.exists (fun bullet ->
    let x, y = bullet.bullet_pos in
    let bullet_rect = Rect.make4 x y 20 20 in
    Rect.has_intersection player_rect bullet_rect
  ) f_bullets


let step_player player =
  let x, y = player.p_pos in
  { player with p_pos =
    match player.p_dir with
    | { left = true; right = false; up = false; down = false } -> (x - 10, y)
    | { left = false; right = true; up = false; down = false } -> (x + 10, y)
    | { left = false; right = false; up = true; down = false } -> (x, y - 10)
    | { left = false; right = false; up = false; down = true } -> (x, y + 10)

    | { left = true; right = false; up = true; down = false } -> (x - 7, y - 7)
    | { left = true; right = false; up = false; down = true } -> (x - 7, y + 7)
    | { left = false; right = true; up = true; down = false } -> (x + 7, y - 7)
    | { left = false; right = true; up = false; down = true } -> (x + 7, y + 7)

    | _ -> (x, y)
  }


let rec game_over renderer player f_bullets p_bullets foes =
  let _ = event_loop player in
  display renderer orange player f_bullets foes;
  Timer.delay 200;
  game_over renderer player f_bullets p_bullets foes


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
        ~width ~height ~flags:[]
  in
  let player = {
    p_pos = (width / 2, height - 60);
    p_last_shot = 0;
    p_shoot_freq = 0;
    p_shooting = false;
    p_dir =
      { left = false;
        right = false;
        up = false;
        down = false;
      };
  } in
  let foes = [] in
  let p_bullets = [] in
  let f_bullets = [] in

  let rec main_loop player f_bullets p_bullets foes =
    let player = event_loop player in
    let t = Timer.get_ticks () in
    let foes, f_bullets = step_foes foes f_bullets player t in
    let f_bullets = step_foes_bullets f_bullets t in
    let player = step_player player in
    display renderer black player f_bullets foes;
    Timer.delay 60;
    if player_touched player f_bullets
    then game_over renderer player f_bullets p_bullets foes
    else main_loop player f_bullets p_bullets foes
  in
  main_loop player f_bullets p_bullets foes
