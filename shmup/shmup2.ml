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
  last_shot: int;
  shoot_freq: int;
}

type bullet = {
  bullet_pos: int * int;
  bullet_dir: int * int;
}

type player = {
  player_pos: int * int;
}

let width, height = (640, 480)

let red    = (255, 0, 0)
let blue   = (0, 0, 255)
let yellow = (255, 255, 0)
let black  = (0, 0, 0)
let alpha  = 255


let fill_rect renderer color (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let display renderer player bullets foes =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;
  List.iter (fun bullet -> fill_rect renderer yellow bullet.bullet_pos) bullets;
  List.iter (fun foe -> fill_rect renderer red foe.foe_pos) foes;
  fill_rect renderer blue player.player_pos;
  Render.render_present renderer;
;;


let proc_events = function
  | Event.KeyDown { Event.keycode = Keycode.Left } -> `left
  | Event.KeyDown { Event.keycode = Keycode.Right } -> `right
  | Event.KeyDown { Event.keycode = Keycode.Up } -> `up
  | Event.KeyDown { Event.keycode = Keycode.Down } -> `down
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> `none


let rec event_loop dir_player =
  match Event.poll_event () with
  | None -> (dir_player)
  | Some ev ->
      let dir = proc_events ev in
      event_loop dir


let bullet_inside bullet =
  let x, y = bullet.bullet_pos in
  (y < height) &&
  (x < width) &&
  (y > -20) &&
  (x > -20)


let step_bullets bullets =
  let step_bullet bullet =
    let x, y = bullet.bullet_pos in
    let dx, dy = bullet.bullet_dir in
    let new_pos = (x + dx, y + dy) in
    { bullet with bullet_pos = new_pos }
  in
  let bullets = List.map step_bullet bullets in
  let bullets = List.filter bullet_inside bullets in
  (bullets)


let new_foe t =
  let foe_pos = (20 * Random.int 32, -20) in
  let shoot_freq = 1200 + Random.int 1000 in
  let last_shot = t in
  { foe_pos; last_shot; shoot_freq }


let new_foes_opt foes t =
  if Random.int 100 > 2
  then foes
  else
    let new_foe = new_foe t in
    new_foe :: foes


let gun_new_bullets bullets foes player t =
  let rec aux acc1 acc2 foes =
    match foes with
    | [] -> (acc1, acc2)
    | foe :: foes ->
        if t - foe.last_shot < foe.shoot_freq
        then aux acc1 (foe :: acc2) foes
        else
          let updated_foe = { foe with last_shot = t } in
          let bullet =
            { bullet_pos = foe.foe_pos;
              bullet_dir = (0, 10) }
          in
          aux (bullet :: acc1) (updated_foe :: acc2) foes
  in
  let new_bullets, foes = aux [] [] foes in
  let bullets = List.rev_append new_bullets bullets in
  (bullets, foes)


let foe_inside foe =
  let (x, y) = foe.foe_pos in
  (y < height)


let step_foes foes bullets player t =
  let step_foe foe =
    let (x, y) = foe.foe_pos in
    let new_pos = (x, y + 2) in
    { foe with foe_pos = new_pos }
  in
  let foes = new_foes_opt foes t in
  let bullets, foes = gun_new_bullets bullets foes player t in
  let foes = List.map step_foe foes in
  let foes = List.filter foe_inside foes in
  (foes, bullets)


let player_touched player bullets =
  let x, y = player.player_pos in
  let player_rect = Rect.make4 x y 20 20 in
  List.exists (fun bullet ->
    let x, y = bullet.bullet_pos in
    let bullet_rect = Rect.make4 x y 20 20 in
    Rect.has_intersection player_rect bullet_rect
  ) bullets


let step_player player req_dir =
  let x, y = player.player_pos in
  { player_pos =
    match req_dir with
    | `left  -> (x - 10, y)
    | `right -> (x + 10, y)
    | `up    -> (x, y - 10)
    | `down  -> (x, y + 10)
    | `none  -> (x, y)
  }


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  let player = { player_pos = (width / 2, height - 60) } in
  let dir_player = `none in
  let bullets = [] in
  let foes = [] in

  let rec main_loop player dir_player bullets foes =
    let req_dir = event_loop dir_player in
    let t = Timer.get_ticks () in
    let foes, bullets = step_foes foes bullets player t in
    let bullets = step_bullets bullets in
    let player = step_player player req_dir in
    display renderer player bullets foes;
    Timer.delay 60;
    if player_touched player bullets
    then begin Sdl.quit (); exit 0 end
    else main_loop player dir_player bullets foes
  in
  main_loop player dir_player bullets foes
