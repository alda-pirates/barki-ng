#!/usr/bin/env ocaml
#load "str.cma"
#load "unix.cma"
#load "graphics.cma"

(* Sample rate in Hertz. *)
let sample_rate = 48000


(* === Oscillators === *)
(*
let rec sawtooth_cycle samples_left samples_per_cycle amplitude =
  let time = samples_per_cycle / amplitude in
  if samples_left > 0
    then (2 * (time / 2) * amplitude) :: (sawtooth_cycle (samples_left - 1) samples_per_cycle amplitude)
    else []
let rec sawtooth' cycle cycles_needed =
  if cycles_needed > 0
    then cycle @ (sawtooth' cycle (cycles_needed - 1))
    else cycle
let sawtooth freq duration amplitude =
  let samples_needed = duration * sample_rate in
  let samples_per_cycle = sample_rate / freq in
  let cycles_needed = samples_needed / freq in
  let cycle = sawtooth_cycle samples_per_cycle samples_per_cycle amplitude in
  sawtooth' cycle cycles_needed
*)
let sawtooth freq duration amplitude = []


let sine     freq duration amplitude = []


let rec square_cycle samples_needed samples amplitude acc =
  let half_cycle = samples / 2 in
  let value = if samples_needed > half_cycle then amplitude else 0 in
  if samples_needed < 0
    then acc
    else square_cycle (samples_needed - 1) samples amplitude (value :: acc)

let rec square' cycle cycles_needed acc =
  if cycles_needed < 0
    then acc
    else square' cycle (cycles_needed - 1) (acc @ cycle)

let square   freq duration amplitude =
  let samples_needed    = duration * sample_rate in
  let samples_per_cycle = sample_rate / freq in
  let cycles_needed     = samples_needed / freq in
  let cycle = square_cycle samples_per_cycle samples_per_cycle amplitude [] in
  square' cycle cycles_needed []


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
  let data = run {step_osc=Square; step_freq=220; step_filter=Noop} 3 20 in
  print_endline "awoo2";
  pcm_append file data;
  pcm_close file;
  ()
