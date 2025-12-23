type prop = string


type formula_LTL =
    | Prop_LTL of prop
    | True_LTL
    | False_LTL
    | Not_LTL of formula_LTL
    | And_LTL of formula_LTL * formula_LTL
    | Or_LTL of formula_LTL * formula_LTL
    | Implies_LTL of formula_LTL * formula_LTL
    | Next_LTL of formula_LTL
    | Eventually_LTL of formula_LTL
    | Always_LTL of formula_LTL
    | Until_LTL of formula_LTL * formula_LTL
    | WeakUntil_LTL of formula_LTL * formula_LTL


let rec string_of_formula_LTL = function
    | Prop_LTL p -> p
    | True_LTL -> "T"
    | False_LTL -> "F"
    | Not_LTL f -> "~(" ^ string_of_formula_LTL f ^ ")"
    | And_LTL (f1, f2) -> "(" ^ string_of_formula_LTL f1 ^ " & " ^ string_of_formula_LTL f2 ^ ")"
    | Or_LTL (f1, f2) -> "(" ^ string_of_formula_LTL f1 ^ " | " ^ string_of_formula_LTL f2 ^ ")"
    | Implies_LTL (f1, f2) -> "(" ^ string_of_formula_LTL f1 ^ " -> " ^ string_of_formula_LTL f2 ^ ")"
    | Next_LTL f -> "X(" ^ string_of_formula_LTL f ^ ")"
    | Eventually_LTL f -> "F(" ^ string_of_formula_LTL f ^ ")"
    | Always_LTL f -> "G(" ^ string_of_formula_LTL f ^ ")"
    | Until_LTL (f1, f2) -> "(" ^ string_of_formula_LTL f1 ^ " U " ^ string_of_formula_LTL f2 ^ ")"
    | WeakUntil_LTL (f1, f2) -> "(" ^ string_of_formula_LTL f1 ^ " W " ^ string_of_formula_LTL f2 ^ ")"


let print_formula desc formula = Printf.printf "%s:\n  %s\n\n" desc (string_of_formula_LTL formula)


let examples () =
    Printf.printf "=== Basic Temporal Operators ===\n\n";
    print_formula "Next" (Next_LTL (Prop_LTL "p"));
    print_formula "Eventually" (Eventually_LTL (Prop_LTL "success"));
    print_formula "Always" (Always_LTL (Prop_LTL "condition"));
    print_formula "Until" (Until_LTL (Prop_LTL "p", Prop_LTL "q"));
    print_formula "Weak Until" (WeakUntil_LTL (Prop_LTL "p", Prop_LTL "q"));
    
    Printf.printf "=== Safety Properties ===\n\n";
    print_formula "G ~error" (Always_LTL (Not_LTL (Prop_LTL "error")));
    print_formula "~error W shutdown" (WeakUntil_LTL (Not_LTL (Prop_LTL "error"), Prop_LTL "shutdown"));
    print_formula "Mutual Exclusion" (Always_LTL (Not_LTL (And_LTL (Prop_LTL "crit1", Prop_LTL "crit2"))));
    
    Printf.printf "=== Liveness Properties ===\n\n";
    print_formula "F success" (Eventually_LTL (Prop_LTL "success"));
    print_formula "G (request -> F response)" (Always_LTL (Implies_LTL (Prop_LTL "request", Eventually_LTL (Prop_LTL "response"))));
    
    Printf.printf "=== Exercise Examples ===\n\n";
    print_formula "Condition until shutdown" (WeakUntil_LTL (Prop_LTL "condition", Prop_LTL "shutdown"));
    print_formula "No error before init" (WeakUntil_LTL (Not_LTL (Prop_LTL "error"), Prop_LTL "initialized"));
    print_formula "Request-response" (Always_LTL (Implies_LTL (Prop_LTL "request", Eventually_LTL (Prop_LTL "response"))));
    print_formula "Using U" (Until_LTL (Prop_LTL "p", Prop_LTL "success"));
    print_formula "Using W" (WeakUntil_LTL (Prop_LTL "safe", Prop_LTL "terminated"))

let () = examples ()
