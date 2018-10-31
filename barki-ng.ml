#!/usr/bin/env ocaml
#load "str.cma"
#load "unix.cma"
#load "graphics.cma"

(* Sample rate in Hertz. *)
let sample_rate = 48000


(* === Oscillators === *)
let sawtooth freq duration amplitude = []
let sine     freq duration amplitude = []

let rec square_cycle' samples_needed amplitude =
  if samples_needed > 0
    then amplitude :: (square_cycle' (samples_needed - 1) amplitude)
    else [amplitude]
let square_cycle samples amplitude =
  let half_cycle = samples / 2 in
  (square_cycle' half_cycle 0) @
    (square_cycle' half_cycle amplitude)

let rec square' cycle cycles_needed =
  if cycles_needed > 0
    then cycle @ (square' cycle (cycles_needed - 1))
    else []

let square   freq duration amplitude =
  let samples_needed = duration * sample_rate in
  let samples_per_cycle = sample_rate / freq in
  let cycles_needed = samples_needed / samples_per_cycle in
  let cycle = square_cycle samples_per_cycle amplitude in
  square' cycle cycles_needed

let triangle freq duration amplitude = []

type oscillator_type =
  | Sawtooth
  | Sine
  | Square
  | Triangle

(* === Filters === *)
(* A filter that does nothing. *)
let noop samples = samples

type filter_type =
  | Noop

(* Miscellaneous *)

type step_type = {  step_osc:       oscillator_type;
                    step_freq:      int;
                    step_filter:    filter_type }

let get_oscillator step =
  match step.step_osc with
  | Sawtooth -> sawtooth
  | Sine     -> sine
  | Square   -> square
  | Triangle -> triangle

let get_filter step =
  match step.step_filter with
  | Noop -> noop


let run step duration amplitude =
  let osc    = get_oscillator step in
  let freq   = step.step_freq in
  let filter = get_filter step in
  filter @@ osc freq duration amplitude


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
  pcm_append file (run {step_osc=Square; step_freq=220; step_filter=Noop} 3 20);
  pcm_close file;
  ()
