#!/usr/bin/env ocaml
#load "str.cma"
#load "unix.cma"
#load "graphics.cma"

let rec range' a b acc =
  if b < a then acc
  else range' a (b - 1) (b :: acc)
let range a b = range' a b []


(* Sample rate in Hertz. *)
let sample_rate = 48000


(* === Oscillators === *)
let sawtooth freq duration = []
let sine     freq duration = []

let square_cycle samples =
  let half_cycle = samples / 2 in
  List.map (fun _ -> 0) (range 0 half_cycle) (*@@
    List.map (fun _ -> 1) (range 0 half_cycle)*)

let square   freq duration =
  let samples_needed = duration * sample_rate in
  let samples_per_cycle = sample_rate / freq in
  let cycles_needed = samples_needed / samples_per_cycle in
  List.flatten @@
    List.map (fun _ -> square_cycle samples_per_cycle) (range 0 cycles_needed)

let triangle freq duration = []

type oscillator_type =
  | Sawtooth
  | Sine
  | Square
  | Triangle

(* === Filters === *)
(* A filter that does nothing. *)
let noop samples time = samples

type filter_type =
  | Noop

(* Miscellaneous *)

type step_type = {  step_osc:       oscillator_type;
                    step_freq:      int;
                    step_filter:    filter_type }

let get_oscillator step =
  match (step_osc step) with
  | Sawtooth -> sawtooth
  | Sine     -> sine
  | Square   -> square
  | Triangle -> triangle

let get_filter step =
  match (step_filter step) with
  | Noop -> noop


let run step duration =
  let osc    = get_oscillator step in
  let freq   = step_freq step in
  let filter = get_filter step in
  filter @@ osc freq duration


(* Functions for manipulating raw PCM (L8) files. *)
let pcm_open name = open_out_bin name
let pcm_append channel data =
  List.map (fun x -> output_byte channel x) data;
  flush channel
let pcm_close channel =
  flush channel;
  close_out channel


let () =
  let file = pcm_open "out.l8" in
  run {step_osc=Square; step_freq=220; step_filter=Noop} 3
