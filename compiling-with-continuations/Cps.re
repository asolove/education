type var = String.t;

type value =
  | Var var
  | Label var
  | Int int
  | Real string
  | String string;

type accesspath =
  | OffP int
  | SelP int accesspath;

type primop =
  | Plus | Minus | Times | Div | Pow;
  /* ... */

type cexp =
  | Record (list (value, accesspath)) var cexp
  | Select int value var cexp
  | Offset int value var cexp
  | App value (list value)
  | Fix (list (var, list var, cexp)) cexp
  | Switch value (list cexp)
  | Primop primop (list value) (list var) (list cexp)
  ;

module VarSet = Set.Make String;
let union_all sets => List.fold_right VarSet.union sets VarSet.empty;

/* Free variables in list of expressions */
let rec fvl: list value => VarSet.t = fun
  | [] => VarSet.empty
  | [Var v, ...r] => VarSet.add v (fvl r)
  | [_, ...r] => fvl(r)
  ;

/* Free variables in an expression */
let rec fv: cexp => VarSet.t = fun
  | App v vs => fvl [v, ...vs]
  | Switch v cs => VarSet.union (fvl [v]) (union_all (List.map fv cs))
  | Record fs w e =>
    VarSet.remove
      w
      (VarSet.union (fvl (List.map (fun (v, p) => v) fs)) (fv e))
  | Select i v w e =>
    VarSet.remove
      w
      (VarSet.union (fvl [v]) (fv e))
  | Offset i v w e =>
    VarSet.remove
      w
      (VarSet.union (fvl [v]) (fv e))
  | Primop p ls ws cs =>
    VarSet.diff
      (VarSet.union
        (fvl ls)
        (union_all (List.map fv cs)))
      (VarSet.of_list ws)
  | Fix fs e => {
    let func_names = List.map (fun (var, _, _) => var) fs;
    let free_vars (var, args, body) => VarSet.diff (fv body) (VarSet.of_list args);
    VarSet.diff
      (VarSet.union (fv e) (union_all (List.map free_vars fs)))
      (VarSet.of_list func_names)
  };

let print_set s => VarSet.iter print_endline s;

let sample_exp =
  Fix
    [
      ("f", ["x", "k"], Primop Times [Int 2, Var "x"] ["u"] [])
    ]
    (Primop Plus [Var "a", Var "b"] ["n"] [App (Var "f") [Var "n", Var "k1"]]);

print_set @@ fv sample_exp;