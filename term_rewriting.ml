type var = string


type symbol = string


type term = Var of var | Fun of symbol * term list


type position = int list


type substitution = (var * term) list


type rewrite_rule = term * term


type trs = rewrite_rule list


let rec positions = function Var _ -> [[]] | Fun (_, args) -> [] :: List.concat (List.mapi (fun i t -> List.map (fun p -> i :: p) (positions t)) args)


let rec subterm_at t pos = match pos, t with
    | [], _ -> Some t
    | i :: rest, Fun (_, args) ->
        if i >= 0 && i < List.length args then
            subterm_at (List.nth args i) rest
        else None
    | _ -> None


let rec replace_at t pos new_term = match pos, t with
    | [], _ -> new_term
    | i :: rest, Fun (f, args) ->
        if i >= 0 && i < List.length args then
            let new_args = List.mapi (fun j arg ->
                if j = i then replace_at arg rest new_term else arg
            ) args in
            Fun (f, new_args)
        else t
    | _ -> t


let rec size = function Var _ -> 1 | Fun (_, args) -> 1 + List.fold_left (fun acc t -> acc + size t) 0 args


let rec vars = function Var x -> [x] | Fun (_, args) -> List.concat (List.map vars args)


let apply_subst sigma t = let rec apply = function Var x -> (try List.assoc x sigma with Not_found -> Var x) | Fun (f, args) -> Fun (f, List.map apply args) in apply t


let rec occurs x = function Var y -> x = y | Fun (_, args) -> List.exists (occurs x) args


let rec unify t1 t2 = match t1, t2 with
    | Var x, Var y when x = y -> Some []
    | Var x, t | t, Var x ->
        if occurs x t then None else Some [(x, t)]
    | Fun (f, args1), Fun (g, args2) when f = g && List.length args1 = List.length args2 ->
        let rec unify_list acc = function
            | [], [] -> Some acc
            | a1 :: rest1, a2 :: rest2 ->
                (match unify (apply_subst acc a1) (apply_subst acc a2) with
                | None -> None
                | Some s -> unify_list (acc @ s) (rest1, rest2))
            | _ -> None
        in unify_list [] (args1, args2)
    | _ -> None


type precedence = symbol -> symbol -> bool


(* Lexicographic Path Order *)
let lpo (prec : precedence) =
    let rec lpo_compare s t = match s, t with
        | _, Var y -> (match s with Var x when x = y -> 0 | _ -> if occurs y s then 1 else 0)
        | Var _, Fun _ -> -1
        | Fun (f, args_s), Fun (g, args_t) ->
            if List.exists (fun si -> lpo_geq si t) args_s then 1
            else if prec f g && List.for_all (fun tj -> lpo_compare s tj > 0) args_t then 1
            else if f = g && List.for_all (fun tj -> lpo_compare s tj > 0) args_t then
                let rec lex_compare = function
                    | [], [] -> 0
                    | si :: rest_s, ti :: rest_t ->
                        if si = ti then lex_compare (rest_s, rest_t)
                        else if lpo_compare si ti > 0 then 1
                        else -1
                    | _ -> 0
                in lex_compare (args_s, args_t)
            else -1
    and lpo_geq s t =
        lpo_compare s t >= 0
    in
    fun s t -> lpo_compare s t > 0


type weight_function = symbol -> int


(* Knuth-Bendix Order *)
let kbo (prec : precedence) (weight : weight_function) (var_weight : int) =
    let rec term_weight = function
        | Var _ -> var_weight
        | Fun (f, args) -> weight f + List.fold_left (fun acc t -> acc + term_weight t) 0 args
    in
    let rec var_count x = function
        | Var y -> if x = y then 1 else 0
        | Fun (_, args) -> List.fold_left (fun acc t -> acc + var_count x t) 0 args
    in
    let all_vars t = List.sort_uniq compare (vars t) in
    let var_condition s t =
        let vs = all_vars s in
        let vt = all_vars t in
        List.for_all (fun x -> var_count x s >= var_count x t) (vs @ vt)
    in
    let rec kbo_compare s t =
        if not (var_condition s t) then -1
        else
            let ws = term_weight s in
            let wt = term_weight t in
            if ws > wt then 1
            else if ws < wt then -1
            else match s, t with
                | Var x, Var y -> if x = y then 0 else -1
                | Fun (f, [arg]), Var _ when weight f = 0 -> 1
                | Fun (f, args_s), Fun (g, args_t) ->
                    if prec f g then 1
                    else if prec g f then -1
                    else if f = g then
                        let rec lex_compare = function
                            | [], [] -> 0
                            | si :: rest_s, ti :: rest_t ->
                                if si = ti then lex_compare (rest_s, rest_t)
                                else
                                    let cmp = kbo_compare si ti in
                                    if cmp <> 0 then cmp else -1
                            | _ -> 0
                        in lex_compare (args_s, args_t)
                    else -1
                | _ -> -1
    in
    fun s t -> kbo_compare s t > 0


let rename_vars t suffix = let rec rename = function Var x -> Var (x ^ suffix) | Fun (f, args) -> Fun (f, List.map rename args) in rename t


let critical_pairs (trs : trs) : (term * term) list =
    let rec find_positions_nonvar t = match t with
        | Var _ -> []
        | Fun (_, args) -> [] :: List.concat (List.mapi (fun i arg ->
            List.map (fun p -> i :: p) (find_positions_nonvar arg)
        ) args)
    in
    let rec generate_pairs acc rules = match rules with
        | [] -> acc
        | (l1, r1) :: rest ->
            let pairs_with_rest = List.concat (List.map (fun (l2, r2) ->
                let l2_renamed = rename_vars l2 "_r" in
                let r2_renamed = rename_vars r2 "_r" in
                List.filter_map (fun p ->
                    match subterm_at l1 p with
                    | Some sub when sub <> Var "" ->
                        (match unify sub l2_renamed with
                        | Some theta ->
                            let u1 = apply_subst theta r1 in
                            let u2 = apply_subst theta (replace_at l1 p r2_renamed) in
                            Some (u1, u2)
                        | None -> None)
                    | _ -> None
                ) (find_positions_nonvar l1)
            ) ((l1, r1) :: rest)) in
            generate_pairs (acc @ pairs_with_rest) rest
    in
    generate_pairs [] trs


let rec reduce_step trs t =
    let rec try_reduce pos_list = match pos_list with
        | [] -> None
        | p :: rest ->
            (match subterm_at t p with
            | Some sub ->
                let rec try_rules = function
                    | [] -> try_reduce rest
                    | (l, r) :: rules ->
                        (match unify sub l with
                        | Some sigma ->
                            Some (replace_at t p (apply_subst sigma r))
                        | None -> try_rules rules)
                in try_rules trs
            | None -> try_reduce rest)
    in try_reduce (positions t)


let rec normalize trs t = match reduce_step trs t with None -> t | Some t' -> normalize trs t'


let is_joinable trs t1 t2 = let n1 = normalize trs t1 in let n2 = normalize trs t2 in n1 = n2


let is_locally_confluent trs = let cps = critical_pairs trs in List.for_all (fun (u1, u2) -> is_joinable trs u1 u2) cps


let rec string_of_term = function Var x -> x | Fun (f, []) -> f | Fun (f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_term args) ^ ")"


let string_of_rule (l, r) = string_of_term l ^ " -> " ^ string_of_term r


let examples () =
    Printf.printf "=== Term Rewriting Examples ===\n\n";
    
    Printf.printf "--- Term Structure ---\n";
    let t1 = Fun ("f", [Fun ("e", []); Fun ("f", [Var "x"; Fun ("i", [Var "x"])])]) in
    Printf.printf "Term: %s\n" (string_of_term t1);
    Printf.printf "Size: %d\n" (size t1);
    Printf.printf "Positions: %d\n\n" (List.length (positions t1));
    
    Printf.printf "--- Substitution ---\n";
    let sigma = [("x", Fun ("a", []))] in
    let t2 = apply_subst sigma t1 in
    Printf.printf "After substituting x -> a: %s\n\n" (string_of_term t2);
    
    Printf.printf "--- Unification ---\n";
    let t3 = Fun ("f", [Var "x"; Var "y"]) in
    let t4 = Fun ("f", [Fun ("a", []); Var "z"]) in
    Printf.printf "Unifying %s and %s\n" (string_of_term t3) (string_of_term t4);
    (match unify t3 t4 with
    | Some theta ->
        Printf.printf "Success: {";
        List.iter (fun (v, t) -> Printf.printf "%s -> %s; " v (string_of_term t)) theta;
        Printf.printf "}\n\n"
    | None -> Printf.printf "Failed\n\n");
    
    Printf.printf "--- Lexicographic Path Order ---\n";
    let prec f g = 
        if f = "i" && g = "f" then true
        else if f = "f" && g = "e" then true
        else if f = "i" && g = "e" then true
        else false
    in
    let lpo_cmp = lpo prec in
    let s1 = Fun ("i", [Fun ("f", [Var "x"; Var "y"])]) in
    let s2 = Fun ("f", [Fun ("i", [Var "x"]); Fun ("i", [Var "y"])]) in
    Printf.printf "Comparing %s >lpo %s: %b\n\n" 
        (string_of_term s1) (string_of_term s2) (lpo_cmp s1 s2);
    
    Printf.printf "--- Knuth-Bendix Order ---\n";
    let weight f = if f = "i" || f = "f" then 0 else 1 in
    let kbo_cmp = kbo prec weight 1 in
    let k1 = Fun ("i", [Fun ("f", [Var "x"; Var "y"])]) in
    let k2 = Fun ("f", [Fun ("i", [Var "y"]); Fun ("i", [Var "x"])]) in
    Printf.printf "Comparing %s >kbo %s: %b\n\n"
        (string_of_term k1) (string_of_term k2) (kbo_cmp k1 k2);
    
    Printf.printf "--- TRS Rewriting ---\n";
    let trs = [
        (Fun ("+", [Fun ("0", []); Var "x"]), Var "x");
        (Fun ("+", [Fun ("s", [Var "x"]); Var "y"]), Fun ("s", [Fun ("+", [Var "x"; Var "y"])]))
    ] in
    List.iter (fun r -> Printf.printf "%s\n" (string_of_rule r)) trs;
    Printf.printf "\n";
    
    let term = Fun ("+", [Fun ("s", [Fun ("0", [])]); Fun ("s", [Fun ("s", [Fun ("0", [])])])]) in
    Printf.printf "Term: %s\n" (string_of_term term);
    let normal = normalize trs term in
    Printf.printf "Normal form: %s\n\n" (string_of_term normal);
    
    Printf.printf "--- Critical Pairs ---\n";
    let trs2 = [(Fun ("f", [Fun ("f", [Var "x"])]), Fun ("g", [Var "x"]))] in
    Printf.printf "TRS: %s\n" (string_of_rule (List.hd trs2));
    let cps = critical_pairs trs2 in
    Printf.printf "Critical pairs: %d\n" (List.length cps);
    List.iter (fun (u1, u2) ->
        Printf.printf "  <%s, %s>\n" (string_of_term u1) (string_of_term u2)
    ) cps;
    Printf.printf "\n";
    
    Printf.printf "--- Local Confluence Test ---\n";
    Printf.printf "Is locally confluent: %b\n" (is_locally_confluent trs2)


let () = examples ()
