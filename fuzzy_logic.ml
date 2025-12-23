type prop = string


type formula_Fuzzy =
    | Prop_Fuzzy of prop
    | Bottom_Fuzzy
    | Top_Fuzzy
    | Not_Fuzzy of formula_Fuzzy
    | WeakAnd_Fuzzy of formula_Fuzzy * formula_Fuzzy
    | WeakOr_Fuzzy of formula_Fuzzy * formula_Fuzzy
    | StrongAnd_Fuzzy of formula_Fuzzy * formula_Fuzzy
    | Implies_Fuzzy of formula_Fuzzy * formula_Fuzzy


type t_norm = float -> float -> float


type t_conorm = float -> float -> float


type residuum = float -> float -> float


let godel_tnorm x y = min x y


let product_tnorm x y = x *. y


let lukasiewicz_tnorm x y = max 0.0 (x +. y -. 1.0)


let make_tconorm (tnorm : t_norm) : t_conorm = fun x y -> 1.0 -. tnorm (1.0 -. x) (1.0 -. y)


let godel_tconorm = make_tconorm godel_tnorm


let product_tconorm = make_tconorm product_tnorm


let lukasiewicz_tconorm = make_tconorm lukasiewicz_tnorm


let godel_residuum x y = if x <= y then 1.0 else y


let product_residuum x y = if x <= y then 1.0 else y /. x


let lukasiewicz_residuum x y = min 1.0 (1.0 -. x +. y)


let make_negation (res : residuum) : (float -> float) = fun x -> res x 0.0


type fuzzy_semantics = {
    tnorm: t_norm;
    tconorm: t_conorm;
    residuum: residuum;
}


let godel_semantics = {
    tnorm = godel_tnorm;
    tconorm = godel_tconorm;
    residuum = godel_residuum;
}


let product_semantics = {
    tnorm = product_tnorm;
    tconorm = product_tconorm;
    residuum = product_residuum;
}


let lukasiewicz_semantics = {
    tnorm = lukasiewicz_tnorm;
    tconorm = lukasiewicz_tconorm;
    residuum = lukasiewicz_residuum;
}


let rec eval_fuzzy (sem : fuzzy_semantics) (valuation : prop -> float) = function
    | Prop_Fuzzy p -> valuation p
    | Bottom_Fuzzy -> 0.0
    | Top_Fuzzy -> 1.0
    | Not_Fuzzy f -> sem.residuum (eval_fuzzy sem valuation f) 0.0
    | WeakAnd_Fuzzy (f1, f2) -> min (eval_fuzzy sem valuation f1) (eval_fuzzy sem valuation f2)
    | WeakOr_Fuzzy (f1, f2) -> max (eval_fuzzy sem valuation f1) (eval_fuzzy sem valuation f2)
    | StrongAnd_Fuzzy (f1, f2) -> sem.tnorm (eval_fuzzy sem valuation f1) (eval_fuzzy sem valuation f2)
    | Implies_Fuzzy (f1, f2) -> sem.residuum (eval_fuzzy sem valuation f1) (eval_fuzzy sem valuation f2)


type symbol = string


type term = Var of string | Fun of symbol * term list


type similarity_relation = symbol -> symbol -> float


let similarity_extend (r : similarity_relation) : (term -> term -> float) =
    let rec sim t1 t2 = match t1, t2 with
        | Var v1, Var v2 -> if v1 = v2 then 1.0 else 0.0
        | Fun (f, args1), Fun (g, args2) ->
            if List.length args1 = List.length args2 then
                let sym_sim = r f g in
                let args_sim = List.map2 sim args1 args2 in
                List.fold_left min sym_sim args_sim
            else 0.0
        | _ -> 0.0
    in sim


type substitution = (string * term) list


let apply_subst (theta : substitution) (t : term) : term =
    let rec apply = function
        | Var v -> (try List.assoc v theta with Not_found -> Var v)
        | Fun (f, args) -> Fun (f, List.map apply args)
    in apply t


let fuzzy_unify (r : similarity_relation) (t1 : term) (t2 : term) : (substitution * float) option =
    let rec unify term1 term2 = match term1, term2 with
        | Var x, Var y when x = y -> Some ([], 1.0)
        | Var x, t | t, Var x -> Some ([(x, t)], 1.0)
        | Fun (f, args1), Fun (g, args2) when List.length args1 = List.length args2 ->
            let sym_sim = r f g in
            let rec unify_args acc_subst acc_sim = function
                | [], [] -> Some (acc_subst, acc_sim)
                | a1::rest1, a2::rest2 ->
                    (match unify a1 a2 with
                    | Some (s, sim) -> 
                        let new_sim = min acc_sim sim in
                        unify_args (acc_subst @ s) new_sim (rest1, rest2)
                    | None -> None)
                | _ -> None
            in
            (match unify_args [] sym_sim (args1, args2) with
            | Some (s, sim) -> Some (s, sim)
            | None -> None)
        | _ -> None
    in unify t1 t2


let rec string_of_term = function Var v -> v | Fun (f, []) -> f | Fun (f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_term args) ^ ")"


let rec string_of_formula_fuzzy = function
    | Prop_Fuzzy p -> p
    | Bottom_Fuzzy -> "F"
    | Top_Fuzzy -> "T"
    | Not_Fuzzy f -> "~(" ^ string_of_formula_fuzzy f ^ ")"
    | WeakAnd_Fuzzy (f1, f2) -> "(" ^ string_of_formula_fuzzy f1 ^ " ^ " ^ string_of_formula_fuzzy f2 ^ ")"
    | WeakOr_Fuzzy (f1, f2) -> "(" ^ string_of_formula_fuzzy f1 ^ " v " ^ string_of_formula_fuzzy f2 ^ ")"
    | StrongAnd_Fuzzy (f1, f2) -> "(" ^ string_of_formula_fuzzy f1 ^ " & " ^ string_of_formula_fuzzy f2 ^ ")"
    | Implies_Fuzzy (f1, f2) -> "(" ^ string_of_formula_fuzzy f1 ^ " => " ^ string_of_formula_fuzzy f2 ^ ")"


let examples () =
    Printf.printf "=== Fuzzy Logic Examples ===\n\n";
    
    Printf.printf "--- T-Norms ---\n";
    Printf.printf "Godel(0.6, 0.8) = %.2f\n" (godel_tnorm 0.6 0.8);
    Printf.printf "Product(0.6, 0.8) = %.2f\n" (product_tnorm 0.6 0.8);
    Printf.printf "Lukasiewicz(0.6, 0.8) = %.2f\n\n" (lukasiewicz_tnorm 0.6 0.8);
    
    Printf.printf "--- T-Conorms ---\n";
    Printf.printf "Godel conorm(0.3, 0.5) = %.2f\n" (godel_tconorm 0.3 0.5);
    Printf.printf "Product conorm(0.3, 0.5) = %.2f\n" (product_tconorm 0.3 0.5);
    Printf.printf "Lukasiewicz conorm(0.3, 0.5) = %.2f\n\n" (lukasiewicz_tconorm 0.3 0.5);
    
    Printf.printf "--- Residuums (Implications) ---\n";
    Printf.printf "Godel: 0.9 => 0.4 = %.2f\n" (godel_residuum 0.9 0.4);
    Printf.printf "Product: 0.9 => 0.4 = %.2f\n" (product_residuum 0.9 0.4);
    Printf.printf "Lukasiewicz: 0.9 => 0.4 = %.2f\n\n" (lukasiewicz_residuum 0.9 0.4);
    
    Printf.printf "--- Formula Evaluation ---\n";
    let val_func p = if p = "p" then 0.7 else 0.6 in
    let f = Implies_Fuzzy (Prop_Fuzzy "p", Prop_Fuzzy "q") in
    Printf.printf "Formula: %s\n" (string_of_formula_fuzzy f);
    Printf.printf "Godel semantics: %.2f\n" (eval_fuzzy godel_semantics val_func f);
    Printf.printf "Product semantics: %.2f\n" (eval_fuzzy product_semantics val_func f);
    Printf.printf "Lukasiewicz semantics: %.2f\n\n" (eval_fuzzy lukasiewicz_semantics val_func f);
    
    Printf.printf "--- Similarity-Based Unification ---\n";
    let sim_rel f g = 
        if f = g then 1.0
        else if (f = "f" && g = "g") || (f = "g" && g = "f") then 0.9
        else 0.0
    in
    let t1 = Fun ("f", [Var "X"]) in
    let t2 = Fun ("g", [Fun ("b", [])]) in
    Printf.printf "Unifying: %s and %s\n" (string_of_term t1) (string_of_term t2);
    (match fuzzy_unify sim_rel t1 t2 with
    | Some (theta, lambda) ->
        Printf.printf "Substitution: {";
        List.iter (fun (v, t) -> Printf.printf "%s -> %s; " v (string_of_term t)) theta;
        Printf.printf "}\n";
        Printf.printf "Similarity degree: %.2f\n" lambda
    | None -> Printf.printf "No unification\n");
    Printf.printf "\n";
    
    Printf.printf "--- Safety Property Example ---\n";
    let safety = WeakAnd_Fuzzy (Not_Fuzzy (Prop_Fuzzy "error"), Prop_Fuzzy "safe") in
    Printf.printf "Formula: %s\n" (string_of_formula_fuzzy safety);
    let val_safe p = 
        if p = "error" then 0.1 else if p = "safe" then 0.9 else 0.5
    in
    Printf.printf "Godel evaluation: %.2f\n" (eval_fuzzy godel_semantics val_safe safety)


let () = examples ()
