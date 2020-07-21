(* A Minimalist Demo inspired by Chain Reaction
 * https://yvoschaap.com/chainrxn/
 Copyright (C) 2019 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

let width, height = (640, 480)

let white = (255, 255, 255)
let black = (0, 0, 0)
let alpha = 255

type rect = {
  x: float;
  y: float;
  dx: float;
  dy: float;
  size: float;
  color: int * int * int;
}

let max_size = 60.0


let proc_events expended = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | Event.Mouse_Button_Down e ->
      let x, y =
        float e.Event.mb_x,
        float e.Event.mb_y
      in
      begin match expended with
      | [] ->
          [{ x; y;
            dx = 0.0;
            dy = 0.0;
            size = 16.0;
            color = white;
          }]
      | l -> l
      end
  | _ -> expended


let rec event_loop expended =
  match Event.poll_event () with
  | None -> expended
  | Some ev ->
      let expended = proc_events expended ev in
      event_loop expended


let fill_rect renderer x y s color =
  let rect = Rect.make4 x y s s in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let draw_rects renderer rects =
  List.iter (fun rect ->
    let x = int_of_float rect.x in
    let y = int_of_float rect.y in
    let size = int_of_float rect.size in
    fill_rect renderer x y size rect.color;
  ) rects


let display renderer rects expended =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;
  draw_rects renderer rects;
  draw_rects renderer expended;
  Render.render_present renderer;
;;


let new_color () =
  (Random.int 256,
   Random.int 256,
   Random.int 256)


let new_rect () =
  { x = float (Random.int width);
    y = float (Random.int height);
    dx = (Random.float 4.0) -. 2.0;
    dy = (Random.float 4.0) -. 2.0;
    color = new_color ();
    size = 16.0;
  }


let rect_move rects =
  List.map (fun r ->
    { r with
      x = r.x +. r.dx;
      y = r.y +. r.dy;
    }
  ) rects


let rect_inside rects =
  List.map (fun r ->
    if r.x < 0.0 then
      { r with dx = Float.abs r.dx }
    else if r.y < 0.0 then
      { r with dy = Float.abs r.dy }
    else if r.x +. r.size > float width then
      { r with dx = -. (Float.abs r.dx) }
    else if r.y +. r.size > float height then
      { r with dy = -. (Float.abs r.dy) }
    else 
      r
  ) rects


let step_rects rects =
  rects
    |> rect_move
    |> rect_inside


let step_expended expended =
  expended |>
    (List.map (fun e ->
      if e.size < max_size then
        { e with
          size = e.size +. 0.8;
          x = e.x -. 0.4;
          y = e.y -. 0.4;
        }
      else e
    ))


let collide e r =
  let i = int_of_float in
  let e = Rect.make4 (i e.x) (i e.y) (i e.size) (i e.size) in
  let r = Rect.make4 (i r.x) (i r.y) (i r.size) (i r.size) in
  Rect.has_intersection e r


let remove rect r =
  List.filter (fun r -> r <> rect) r


let step_collide rects expended =
  let r = ref rects in
  let e = ref expended in
  List.iter (fun rect ->
    List.iter (fun exp ->
      if collide exp rect then begin
        e := rect :: !e;
        r := remove rect !r;
      end
    ) expended;
  ) rects;
  (!r, !e)


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  let rects =
    Array.to_list (
      Array.init 22 (fun i -> new_rect ()))
  in
  let expended = [] in

  let rec main_loop rects expended =
    let expended = event_loop expended in
    display renderer rects expended;
    let rects = step_rects rects in
    let expended = step_expended expended in
    let rects, expended = step_collide rects expended in
    Timer.delay 40;
    main_loop rects expended
  in
  main_loop rects expended
