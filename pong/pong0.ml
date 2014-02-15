open Sdl

let width, height = (480, 360)

let blue  = (0, 0, 255)
let black = (0, 0, 0)
let alpha = 255

let fill_rect renderer (x, y) (w, h) =
  let rect = Rect.make4 x y w h in
  Render.fill_rect renderer rect;
;;

let display renderer pos_ball =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;
  Render.set_draw_color renderer blue alpha;
  fill_rect renderer pos_ball (20, 20);
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
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
      ~width ~height ~flags:[]
  in
  let pos_ball = (220, 160) in
  let dir_ball = (20, 20) in

  let rec main_loop pos_ball dir_ball =
    let () = event_loop () in
    let pos_ball, dir_ball =
      let x, y = pos_ball in
      let _x, _y = dir_ball in
      let x, y = (x + _x, y + _y) in
      let _x = if x + 20 >= width  || x <= 0 then - _x else _x in
      let _y = if y + 20 >= height || y <= 0 then - _y else _y in
      (x, y), (_x, _y)
    in
    display renderer pos_ball;
    Timer.delay 240;
    main_loop pos_ball dir_ball
  in
  main_loop pos_ball dir_ball
