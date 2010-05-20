
open Utils;;
open Printf;;

type expr =
  | Number of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Mod of expr * expr
  | Divide of expr * expr
  | Multiply of expr * expr
  | Not of expr
  | Var of string
  | LessThan of expr * expr
  | GreaterThan of expr * expr
  | Equal of expr * expr;;

type ir =
  | NewFrame
  | KillFrame
  | Goto of string
  | GotoIf of expr * string * string
  | Label of string
  | Assignment of string * expr
  | Declare of string
  | Output of string;;

let example_1 = [ NewFrame;
                  Declare "N";
                  Assignment("N", Number 137);
                  Label "While";
                  GotoIf(Not(GreaterThan(Var "N", Number 1)), "AfterWhile", "");
                  GotoIf(Equal(Mod(Var"N", Number 2), Number 0), "ElseBranch", "");
                  Assignment("N", Divide(Var "N", Number 2));
                  Goto("AfterIf");
                  Label("ElseBranch");
                  Assignment("N", Add(Multiply(Number 3, Var "N"), Number 1));
                  Label("AfterIf");
                  Goto("While");
                  Label("AfterWhile");
                  KillFrame;
                ];;

module StringMap = Map.Make(String);;

type block = { name: string;
               labels: string list;
               instructions: ir list;
               next_blocks: string list;
               prev_blocks: string list;
             };;

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
        prev_blocks = [] }
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

let rebuild_ir blocks =
  blocks
             |> List.map (fun block -> block.instructions)
             |> List.concat;;

let map_of_list list =
  let rec mol_iter map list =
    match list with
      | [] -> map
      | (key, value) :: tl ->
          mol_iter (StringMap.add key value map) tl
  in
    mol_iter StringMap.empty list;;

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
            [block.name, (StringMap.find lbl label_map).name]
        | GotoIf(_, lbl1, lbl2) ->
            [block.name, (StringMap.find lbl1 label_map).name;
             block.name, (StringMap.find lbl2 label_map).name]
        | _ ->
            []
  in
  let from_to_links =
    StringMap.fold (fun block_name block from_to_links ->
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
    StringMap.map (fun block -> { block with
                                    next_blocks = next_blocks block;
                                    prev_blocks = prev_blocks block }) block_map;;

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
type 'state absint_pass = { state_combine: 'state option list -> 'state option;
                            state_converges: 'state option -> 'state option -> bool;
                            state_update: 'state option -> ir -> 'state option;
                          };;

type 'state block_state = int * 'state option;;

let block_sort blocks pass max_iterations_per_block =
  let blocks_state =
    List.fold_left
      (fun map block -> StringMap.add block.name (0, None) map)
      StringMap.empty
      blocks
  in
  let blocks_map, _ = make_block_maps blocks in
  let first_blocks = List.filter (fun block -> block.prev_blocks = []) blocks in
  let final_blocks = List.filter (fun block -> block.next_blocks = []) blocks in
  let add_end elms list = (elms @ (List.rev list)) |> List.rev in
  let extract_state (count, state) = state in
  let rec bs_iter queue blocks_state =
    match queue with
      | [] ->
          let final_blocks_states =
            List.map
              (fun block -> StringMap.find block.name blocks_state |> extract_state)
              final_blocks
          in
            pass.state_combine final_blocks_states
      | hd :: tl ->
          let count, old_state = StringMap.find hd.name blocks_state in
          let new_state = List.fold_left pass.state_update old_state hd.instructions in
          let process_children =
            count < max_iterations_per_block
            && not(pass.state_converges old_state new_state)
          in
          let new_blocks_state =
            StringMap.add hd.name (count + 1, new_state) blocks_state
          in
            if process_children
            then
              let children_blocks =
                List.map (fun name -> StringMap.find name blocks_map) hd.next_blocks
              in
              let new_queue = add_end children_blocks tl in
                bs_iter new_queue new_blocks_state
            else
              bs_iter tl new_blocks_state
  in
    bs_iter first_blocks blocks_state;;

(* Undefined variable pass. *)
type var_value =
  | Uninitialized
  | Unknown

type env = var_value StringMap.t list;;

module StringSet = Set.Make(String);;

type state = env * string StringMap.t;;

let combine_vals expr1 expr2 =
  match (expr1, expr2) with
    | Unknown, Unknown -> Unknown
    | _ -> Uninitialized;;

let rec eval expr env =
  let rec env_get_var env var =
    match env with
      | var_map :: other_maps ->
          if StringMap.mem var var_map
          then Some(StringMap.find var var_map)
          else env_get_var other_maps var
      | [] ->
          None
  in
  match expr with
    | Number _ -> Unknown
    | Add(expr1, expr2)
    | Subtract(expr1, expr2)
    | Mod(expr1, expr2)
    | Multiply(expr1, expr2)
    | Divide(expr1, expr2)
    | LessThan(expr1, expr2)
    | GreaterThan(expr1, expr2)
    | Equal(expr1, expr2)  ->
        combine_vals (eval expr1 env) (eval expr2 env)
    | Not(expr) ->
        eval expr env
    | Var var ->
        match env_get_var env var with
          | None -> Uninitialized
          | Some _ -> Unknown;;

let process ir_list =
  let blocks = ir_list |> process_goto_if |> split_into_blocks in
  let block_map, label_map = make_block_maps blocks in
  let block_map = add_prev_next block_map label_map in
  let block_list = StringMap.fold (fun _ block list -> block :: list) block_map []
                          |> List.rev
  in
    block_list;;
