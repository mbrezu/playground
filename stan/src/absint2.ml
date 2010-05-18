
open Utils;;
open Printf;;

type ir =
  | NewFrame
  | KillFrame
  | Goto of string
  | GotoIf of string * string * string
  | Label of string
  | Assignment of string * string
  | Declare of string
  | Output of string;;

type var_value =
  | Uninitialized
  | Unknown

module VarMap = Map.Make(String);;

type env = var_value VarMap.t list;;

type block = { name: string;
               labels: string list;
               instructions: ir list;
               next_blocks: string list;
               prev_blocks: string list;
               end_state: env;
               run_count: int};;

module BlockMap = Map.Make(String);;

let gensym_counter = ref 0;;

let gensym prefix =
  gensym_counter := !gensym_counter + 1;
  sprintf "%s_%d" prefix !gensym_counter;;

let reset_gensym () =
  gensym_counter := 0;;

let split_while pred list =
  let rec split_while_iter pred list acc =
    match list with
      | [] ->
          (List.rev acc, [])
      | hd :: tl ->
          if pred hd
          then split_while_iter pred tl (hd :: acc)
          else (List.rev acc, list)
  in
    split_while_iter pred list [];;

(* The block splitting pass should be preceded by another pass that
   verifies that:

   1. No labels are defined twice.

   2. All labels that are not used in gotos are removed.

   3. `process_goto_if` has been called on this ir_list.

*)

let split_into_blocks ir_list =
  let make_block irs =
    let prefix =
      match irs with
        | Label(label) :: _ -> label
        | _ -> "Block"
    in
    let labels =
      let starting_labels, _ =
        split_while (function Label(_) -> true | _ -> false) irs
      in
        starting_labels
             |> List.map (function Label(label) -> [label] | _ -> [])
             |> List.concat
    in
      { name = gensym prefix;
        labels = labels;
        instructions = irs;
        next_blocks = [];
        prev_blocks = [];
        end_state = [];
        run_count = 0}
  in
  let add_block current_block blocks =
    if current_block = []
    then blocks
    else (current_block |> List.rev |> make_block) :: blocks
  in
  let rec split_into_blocks_iter ir_list current_block blocks =
    match ir_list with
      | [] ->
          add_block current_block blocks |> List.rev
      | Label lbl :: _ ->
          let new_blocks = add_block current_block blocks in
          let labels, rest =
            split_while (function | Label _ -> true | _ -> false) ir_list
          in
            split_into_blocks_iter rest (List.rev labels) new_blocks
      | (GotoIf(_, _, _) as hd) :: tl
      | (Goto(_) as hd) :: tl ->
          let full_block = hd :: current_block in
          let new_blocks = add_block full_block blocks in
            split_into_blocks_iter tl [] new_blocks
      | hd :: tl ->
          split_into_blocks_iter tl (hd :: current_block) blocks
  in
    split_into_blocks_iter ir_list [] [];;

(* let topo_sort_blocks block_map = None;; *)

let process_goto_if ir_list =
  let rec pgi_iter ir_list =
    match ir_list with
      | [] -> []
      | GotoIf(cond, dest, _) :: tl ->
          let label = gensym "Pgi"in
            GotoIf(cond, dest, label) :: Label(label) :: (pgi_iter tl)
      | hd :: tl ->
          hd :: (pgi_iter tl)
  in
    pgi_iter ir_list;;

let example_1 = [ NewFrame;
                  Declare "N";
                  Assignment("N", "137");
                  Label "While";
                  GotoIf("!(N > 1)", "AfterWhile", "");
                  GotoIf("N mod 2 = 0", "ElseBranch", "");
                  Assignment("N", "N/2");
                  Goto("AfterIf");
                  Label("ElseBranch");
                  Assignment("N", "3 * N + 1");
                  Label("AfterIf");
                  Goto("While");
                  Label("AfterWhile");
                  KillFrame;
                ];;
