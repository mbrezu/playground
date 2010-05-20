
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
          let maybe_add_goto current_block lbl =
            match current_block with
              | Goto(_) :: tl ->
                  current_block
              | GotoIf(_, _, _) :: tl ->
                  current_block
              | [] -> []
              | _ ->
                  Goto(lbl) :: current_block
          in
          let new_current_block = maybe_add_goto current_block lbl in
          let new_blocks = add_block new_current_block blocks in
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

let rebuild_ir blocks =
  blocks
             |> List.map (fun block -> block.instructions)
             |> List.concat;;

let map_of_list list =
  let rec mol_iter map list =
    match list with
      | [] -> map
      | (key, value) :: tl ->
          mol_iter (BlockMap.add key value map) tl
  in
    mol_iter BlockMap.empty list;;

let make_block_maps blocks =
  let name_list = List.map (fun block -> (block.name, block)) blocks in
  let block_map = map_of_list name_list in
  let label_list =
    List.map (fun block -> List.map (fun label -> (label, block)) block.labels) blocks
             |> List.concat
  in
  let label_map = map_of_list label_list in
    (block_map, label_map);;

let add_prev_next block_map label_map =
  let next_blocks_links block =
    let last_insn = block.instructions |> List.rev |> List.hd in
      match last_insn with
        | Goto(lbl) ->
            [block.name, (BlockMap.find lbl label_map).name]
        | GotoIf(_, lbl1, lbl2) ->
            [block.name, (BlockMap.find lbl1 label_map).name;
             block.name, (BlockMap.find lbl2 label_map).name]
        | _ ->
            []
  in
  let from_to_links =
    BlockMap.fold (fun block_name block from_to_links ->
                     let new_links = next_blocks_links block in
                       new_links @ from_to_links) block_map []
  in
  let next_blocks block =
    List.filter (fun (fst, _) -> fst = block.name) from_to_links
             |> List.map (fun (_, next) -> next)
  in
  let prev_blocks block =
    List.filter (fun (_, snd) -> snd = block.name) from_to_links
             |> List.map (fun (prev, _) -> prev)
  in
    BlockMap.map (fun block -> { block with
                                   next_blocks = next_blocks block;
                                   prev_blocks = prev_blocks block }) block_map;;

module StringMap = Map.Make(String);;

(* 

   This is actually the abstract interpreter core. It needs the
   following parameters:

   * `blocks: block list` the list of blocks to run on;

   * `state_converges: state -> state -> bool` the function that
   decides if fixed point is reached; the parameters are the old
   state, the new state; it returns true if the states converge, false
   otherwise;

   * `state_combine: state list -> state` the function used to decide
   the state at the start of a block based on the state of previous
   blocks.

   * `state_update: state -> instruction -> state` the function ran on
   every instruction to update the state in the current block.

   At the end, the final state is the combined state (via
   `state_combine`) of all final blocks (i.e. blocks without follow up
   blocks).

   The abstract interpreter core will run these functions on a set of
   blocks.

   An abstract interpreter pass is made up of a set of these
   functions.

*)
(* let block_sort blocks = *)
(*   let first_blocks = List.filter (fun block -> block.prev_blocks = []) blocks in *)
(*   let touched, touched_count = *)
(*     List.fold_left *)
(*       (fun (touched_map, touched_count) block -> *)
(*          let bool_value = List.mem block first_blocks in *)
(*            (StringMap.add block.name bool_value touched_map, *)
(*             if bool_value then touched_count + 1 else touched_count)) *)
(*       (StringMap.empty, 0) *)
(*   in *)
(*   let block_count = List.count blocks in *)
(*   let add_end elm list = (elm :: (List.rev list) |> List.rev in *)
(*   let bs_iter queue touched touched_count = *)
(*     match queue with *)
(*       | [] -> *)
(*   in *)
(*     bs_iter blocks first_blocks touched touched_count [];; *)

let process ir_list =
  let blocks = ir_list |> process_goto_if |> split_into_blocks in
  let block_map, label_map = make_block_maps blocks in
  let block_map = add_prev_next block_map label_map in
  let block_list = BlockMap.fold (fun _ block list -> block :: list) block_map []
             |> List.rev
  in
    block_list;;
