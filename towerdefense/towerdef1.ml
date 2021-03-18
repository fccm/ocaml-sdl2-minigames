(* Copyright (C) 2019 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

type point2d = int * int  (* (x, y) *)
type vector2d = int * int  (* (x, y) *)

module Vector2d : sig
  type t = vector2d  (** (x, y) *)

  val add : t -> t -> t
  (** [a + b] *)

  val sub : t -> t -> t
  (** [a - b] *)

  val mul : t -> int -> t
  (** [v * k] *)

  val div : t -> int -> t
  (** [v / k] *)

  module Infix : sig
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> int -> t
    val ( /. ) : t -> int -> t
  end
end = struct
  type t = vector2d

  let add (ax, ay) (bx, by) =
    (ax + bx,
     ay + by)

  let sub (ax, ay) (bx, by) =
    (ax - bx,
     ay - by)

  let mul (x, y) k =
    (x * k,
     y * k)

  let div (x, y) k =
    (x / k,
     y / k)

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
  end
end


module LinearBezierCurves : sig
  val interval : int * int
  (** The interval for interpolation is [(0, 1000)]
      instead of [(0.0, 1.0)] for [floats]. *)

  val point_on_line :
    point2d * point2d ->
    int -> point2d
  (** [point_on_line (p1, p2) t] returns a point on the linear bezier
      curve defined by p1 and p2 with t in the interval predefined above *)

end = struct
  let interval = (0, 1000)

  let point_on_line (p1, p2) t =
    let ti = 1000 - t in
    Vector2d.Infix.(
      ( (p1 *. ti) +.
        (p2 *. t)
      ) /. 1000
    )
end


module QuadraticBezierCurves : sig
  val interval : int * int
  (** The interval for interpolation is [(0, 1000)]
      instead of [(0.0, 1.0)] for [floats]. *)

  val point_on_curve :
    point2d * point2d * point2d ->
    int -> point2d
  (** [point_on_curve (p1, p2, p3) t] returns a point on the quadratic bezier
      curve defined by p1, p2 and p3, with t in the interval predefined above *)

end = struct
  let interval = (0, 1000)

  let point_on_curve (p1, p2, p3) t =
    let ti = 1000 - t in
    Vector2d.Infix.(
      ( p1 *. ((ti * ti) / 1000) +.
        p2 *. ((2 * ti * t) / 1000) +.
        p3 *. ((t * t) / 1000)
      ) /. 1000
    )
end


module Timeline : sig
  type time = int

  type ('a, 'b) animated = [
    | `From of time * 'a
      (** [From (t, v)] after time [t] is reach (and before next timeline chunk)
          the returned value will be [v] *)
    | `Evol of time * time * (time -> time -> time -> 'b -> 'a) * 'b
      (** [Evol (t1, t2, f, d)] when [t] is between [t1] and [t2] the value is
          the result of [f t1 t2 t d] *)
    ]

  val val_at :
    time -> ('a, 'b) animated list -> 'a
  (** [val_at t anim] returns the value at time [t] from the animation [anim] *)

  val finished :
    time -> ('a, 'b) animated list -> bool
  (** tells if the animation is finished *)

  val print :
    ('a, 'b) animated list -> unit
  (** debug function *)

end = struct
  type time = int

  (* animating a value over time *)

  type ('a, 'b) animated = [
    | `From of time * 'a
    | `Evol of time * time * (time -> time -> time -> 'b -> 'a) * 'b
    ]

  (* timeline function *)

  let rec val_at t = function
    | `From(t1, v) :: `From(t2,_) :: _
    | `From(t1, v) :: `Evol(t2,_,_,_) :: _
      when t1 <= t && t < t2 -> v
    
    | `From(t, v) :: [] -> v
    
    | `Evol(t1, t2, f, v) :: []
      when t >= t2 -> f t1 t2 t2 v
    
    | `Evol(t1, t2, f, v) :: _
      when t1 <= t && t <= t2 -> f t1 t2 t v
    
    | _ :: tl -> val_at t tl
    
    | [] -> invalid_arg "val_at"


  let rec print = function
    | `From(t, _) :: tl ->
        Printf.printf " From(%d,_)\n" t; print tl
    | `Evol(t1, t2, _, _) :: tl ->
        Printf.printf " Evol(%d,%d,_,_)\n" t1 t2; print tl
    | [] -> ()


  let rec finished t = function
    | `From _ :: [] -> true
    | `Evol(_, t2, _, _) :: [] -> t > t2

    | `From(t2, _) :: tl -> if t < t2 then false else finished t tl
    | `Evol(_, t2, _, _) :: tl -> if t < t2 then false else finished t tl

    | _ -> false
end

module QBCurve = QuadraticBezierCurves
module LBCurve = LinearBezierCurves


let width, height = (640, 480)

let red    = (255, 0, 0)
let blue   = (0, 0, 255)
let yellow = (255, 255, 0)
let grey   = (100, 100, 100)
let alpha  = 255


let fill_rect renderer color (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let display renderer foes chuncks =
  Render.set_draw_color renderer grey alpha;
  Render.clear renderer;

  Render.set_draw_color renderer yellow alpha;
  List.iter (fun (p1, p2) ->
    Render.draw_line2 renderer p1 p2;
  ) chuncks;

  List.iter (fill_rect renderer yellow) foes;

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


let distance (x1, y1) (x2, y2) =
  let x_diff = x1 - x2
  and y_diff = y1 - y2 in
  let sq_dist = (
    (x_diff * x_diff) +
    (y_diff * y_diff) )
  in
  sqrt (float_of_int sq_dist)


let inter1 t t1 t2 v1 v2 =
  ((v2 - v1) * (t - t1)) / (t2 - t1) + v1

let t_min, t_max = QBCurve.interval

let fe t1 t2 t ps =
  let t = inter1 t t1 t2 t_min t_max in
  LBCurve.point_on_line ps t


let qbcurve_length n ps =
  let tp = (t_max - t_min) / n in
  let r = ref [] in
  for i = 0 to n do
    let t = t_min + tp * i in
    let p = QBCurve.point_on_curve ps t in
    r := p :: !r
  done;
  let rec aux len = function
  | p1 :: p2 :: ps ->
      let d = distance p1 p2 in
      aux (d +. len) (p2 :: ps)
  | [] | _ :: [] ->
      int_of_float len
  in
  aux 0.0 !r


let qbcurve_chunks n ps =
  let (p1, p2, p3) = ps in
  let tp = (t_max - t_min) / n in
  let r = ref [p1] in
  for i = 1 to pred n do
    let t = t_min + tp * i in
    let p = QBCurve.point_on_curve ps t in
    r := p :: !r;
  done;
  r := p3 :: !r;
  let rec aux acc = function
  | p1 :: p2 :: ps ->
      aux ((p1, p2)::acc) (p2 :: ps)
  | [] | _ :: [] ->
      (List.rev acc)
  in
  aux [] (List.rev !r)


let seg_list_length segs =
  int_of_float (
    List.fold_left (fun acc (p1, p2) ->
      (acc +. (distance p1 p2))
    ) 0.0 segs
  )


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer
        ~width ~height ~flags:[]
  in

  let p1, p2, p3, p4, p5 =
    (0, 0),
    (width / 4, height),
    (width / 2, height / 2),
    ((width / 4) * 3, 0),
    (width, height)
  in
  let ps1 = (p1, p2, p3) in
  let ps2 = (p3, p4, p5) in

  let path = [ps1; ps2] in
  List.iter (fun ps ->
    let len = qbcurve_length 16 ps in
    Printf.printf "length: %d\n" len;
  ) path;
  let chuncks =
    List.map (qbcurve_chunks 16) path
  in
  let chuncks = List.flatten chuncks in
  let chuncks_and_len =
    List.map (fun (p1, p2) ->
      let len = distance p1 p2 in
      ((p1, p2), len)
    ) chuncks
  in
  let len_tot =
    List.fold_left (fun acc (_, len) ->
      acc +. len
    ) 0.0 chuncks_and_len
  in
  let p0 = (-100, -100) in
  let timeline_of_chuncks chuncks_and_len t0 t_tot =
    let path_and_t =
      List.map (fun (ps, len) ->
        let t = len *. float t_tot /. len_tot in
        (ps, int_of_float t)
      ) chuncks_and_len
    in
    let rec aux acc t_acc = function
    | (ps, t) :: tail ->
        let this = `Evol (t_acc, t_acc + t, fe, ps) in
        aux (this::acc) (t_acc + t) tail
    | [] ->
        (List.rev acc)
    in
    let init_acc =
      if t0 = 0 then []
      else [ `From (0, p0) ]
    in
    aux init_acc t0 path_and_t
  in
  let anim = timeline_of_chuncks chuncks_and_len 0 6000 in
  let anims = [anim] in

  let rec main_loop () =
    let () = event_loop () in
    let t = Timer.get_ticks () in
    let foes = List.map (Timeline.val_at t) anims in
    display renderer foes chuncks;
    Timer.delay 20;
    main_loop ()
  in
  main_loop ()
