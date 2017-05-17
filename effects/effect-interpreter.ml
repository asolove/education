(* Requires: OCaml 4.03.0+ *)
(* Usage: `ocaml eff.ml` *)

(** Identifiers *)
module Ident = struct
  type t = string

  let id = ref 0

  let create base =
    incr id;
    base ^ "_" ^ (string_of_int !id)

  module Map =  Map.Make (struct
    type nonrec t = t
    let compare = String.compare
  end)

end

(* Our lambda calculus, which is standard lambda-calculus, plus:
   - atoms;
   - primitives: basically ocaml functions, applied to λ-terms.
     The arguments are guaranteed to be reduced before being given
     to the function;
   - effects primitives: handle & perform.
*)

type term = ..

type atom =
  | Unit
  | Int of int
  | String of string

and handlers = {
  hv: Ident.t * term;              (* Value *)
  hx: Ident.t * term;          (* Exception *)
  hf: Ident.t * Ident.t * term    (* Effect *)
}

(* A runtime value: a closure of a term with its environment.
   It is defined now as primitives can access and modify the runtime
   environment; and thus take it as an argument and return it.
*)
and value = Closure of environment * term
and environment = value Ident.Map.t

type term +=
  | Var of Ident.t
  | Lambda of Ident.t * term
  | App of term * term
  | Atom of atom
  | Prim of string * (term list -> term) * term list
  | Handle of term * handlers
  | Perform of term
  | Raise of term



(* Helpers ********************************************************************)

(* Lambda and App constructors are the simplest possible. To define
   lambdas with multiple identifiers at the same time, or application
   with multiple arguments, use the [lam] and [app] helpers.
*)

let lam (idents: Ident.t list) (body: term): term =
  List.fold_right (fun id acc -> Lambda (id, acc)) idents body

let app (f: term) (args: term list): term =
  let hd, args = (List.hd args), (List.tl args) in
  List.fold_left (fun acc arg -> App (acc, arg)) (App (f, hd)) args

let rec seq : term list -> term = function
  | [] -> Atom Unit
  | [e] -> e
  | e :: es ->
    let dummy = Ident.create "_" in
    App (Lambda (dummy, seq es), e)

let letin (x: Ident.t) (e1: term) (e2: term): term =
  App (Lambda (x, e2), e1)

(* Helpers for building atomic terms. *)

let int (x: int): term = Atom (Int x)
let string (s: string): term = Atom (String s)
let unit : term = Atom Unit

(* Effects wrappers. *)
let handle body handlers = Handle (body, handlers)

let delegate =
  let ve = Ident.create "ve" in
  let vk = Ident.create "vk" in
  ve, vk, App (Var vk, Perform (Var ve))

let continue k v = App (k, v)

(* Printers. *)

let print_atom ppf = function
  | Unit -> Format.fprintf ppf "()"
  | Int i -> Format.fprintf ppf "%d" i
  | String s -> Format.fprintf ppf "%s" s

let print_t ppf =
  let open Format in
  let rec aux paren ppf = function
    | Var x -> fprintf ppf "%s" x
    | Lambda (x, e) ->
      fprintf ppf "@[<2>%s%a%s@]"
        (if paren then "(" else "")
        (aux_lambda [x]) e
        (if paren then ")" else "")

    | App (u, v) ->
      fprintf ppf "@[<2>%s%a%s@]"
        (if paren then "(" else "")
        (aux_app [v]) u
        (if paren then ")" else "")

    | Atom a -> print_atom ppf a
    | Prim (name, f, args) ->
      fprintf ppf "@[<2>%s%s%a%s@]"
        (if paren then "(" else "")
        name
        (fun ppf _ ->
           List.iter (fun arg -> fprintf ppf "@ %a" (aux false) arg) args)
        ()
        (if paren then ")" else "")

    | Perform e ->
      fprintf ppf "@[<2>%sperform@ %a%s@]"
        (if paren then "(" else "")
        (aux false) e
        (if paren then ")" else "")

    | Handle (body, {hv = (v, hv); hx = (x, hx); hf = (e, k, hf)}) ->
      fprintf ppf "@[<2>%s%a@ with@ %s@ ->@ %a@ |@ exn@ %s@ ->@ %a@ |@ effect@ %s@ %s@ ->@ %a%s@]"
        (if paren then "(" else "")
        (aux true) body
        v (aux false) hv x (aux false) hx e k (aux false) hf
        (if paren then ")" else "")

    | Raise e ->
      fprintf ppf "@[<2>%sraise@ %a%s@]"
        (if paren then "(" else "")
        (aux false) e
        (if paren then ")" else "")

    | _ -> failwith "aux : unknown term"

  and aux_lambda idents ppf = function
    | Lambda (i, e) -> aux_lambda (i :: idents) ppf e
    | e ->
      fprintf ppf "λ";
      List.iter (fun id -> fprintf ppf "@ %s" id) (List.rev idents);
      fprintf ppf ".@ %a" (aux false) e

  and aux_app args ppf = function
    | App (u, v) -> aux_app (v :: args) ppf u
    | e ->
      fprintf ppf "%a"
        (aux true) e;
      List.iter (fun arg -> fprintf ppf "@ %a" (aux true) arg) args
  in
  aux false ppf

(* Printing primitives. *)

let debug = ref true

let printl t =
  let f = function
    | [t] when !debug ->
        Format.(fprintf str_formatter "%a\n%!" print_t t);
        unit
    | [t] -> Format.printf "%a\n%!" print_t t; unit
    | _ as l ->
        Printf.printf "printl.args_length = %d\n%!" (List.length l);
        List.iter (fun t -> Format.printf "%a\n%!" print_t t) l;
        raise (Invalid_argument "printl")
  in
  Prim ("printl", f, [t])

(* CPS transform **************************************************************)

(* Variable substitution. Only used to perform administrative
   reductions. *)
let rec subst map e =
  match e with
  | Var x                -> (try Ident.Map.find x map with Not_found -> e)
  | Lambda (x, e)        -> Lambda (x, subst map e)
  | App (u, v)           -> App (subst map u, subst map v)
  | Prim (name, f, args) -> Prim (name, f, List.map (subst map) args)
  | _ -> e

let rec cps e =
  let k = Ident.create "k" in

  (* [cont e c] "continues" term [e] with continuation [c].
     It is semantically equivalent to [app e [c]], but can
     perform some administrative reductions.
  *)
  let cont e c =
    let is_value = function
      | Var _ | Atom _ | Prim _ -> true
      | _ -> false
    in

    match e with
    | Lambda (k, App (Var k', e')) when k = k' && is_value e' ->
        begin match c with
        | Var k -> App (Var k, e')
        | Lambda (x, body) ->
          subst Ident.Map.(add x e' empty) body
        | _ -> raise (Invalid_argument "cont")
        end

    | _ -> app e [c]
  in

  match e with
  | Var _ | Atom _ -> lam [k] (App (Var k, e))

  | Prim (name, f, args) ->
    let args_idents = List.map (fun _ -> Ident.create "v") args in
    lam [k] @@
      List.fold_right (fun (arg, id) e -> cont (cps arg) (Lambda (id, e)))
        (List.combine args args_idents)
        (App (Var k, Prim (name, f, List.map (fun v -> Var v) args_idents)))


  | Lambda (x, e) -> lam [k] (App (Var k, Lambda (x, cps e)))

  | App (u, v) ->
    let val_u = Ident.create "v" in
    let val_v = Ident.create "v" in
    lam [k] @@
      (cont (cps u) (lam [val_u]
      (cont (cps v) (lam [val_v]
      (cont (App (Var val_u, Var val_v)) (Var k))))))

  | Handle (body, { hv = (v, hv); hx = (vx, hx); hf = (ve, vk, hf) }) ->
    let _1 = Ident.create "_" in
    let _2 = Ident.create "_" in
    lam [k] @@
      app (cps body) [lam [v; _1; _2] (cps hv);
                      lam [vx] (cps hx);
                      lam [ve; vk] (cps hf);
                      Var k]

  | Perform e ->
    let val_e = Ident.create "e" in
    let x = Ident.create "x" in
    let hx = Ident.create "hx" in
    let hf = Ident.create "hf" in
    let k' = Ident.create "k'" in

    let return_k =
      lam [x; k'] @@ app (Var k) [Var x; Var hx; Var hf; Var k']
    in

    lam [k] @@
      cont (cps e) (lam [val_e; hx; hf]
        (app (Var hf) [ Var val_e; return_k ]))

  | Raise e ->
    let val_e = Ident.create "ve" in
    let hx = Ident.create "hx" in
    let hf = Ident.create "hf" in
    lam [k] @@
      cont (cps e) (lam [val_e]
        (lam [hx; hf] (app (Var hx) [Var val_e])))

  | _ -> failwith "cps: unhandled term"

let unhandled_effect ?(cps=false) e k =
  let f = function
    | [e; k] ->
      Format.printf "Unhandled effect: %a\n%!" print_t e;
      if cps then
        let _k = Ident.create "_" in
        lam [_k] unit
      else unit
    | _ -> raise (Invalid_argument "unhandled_effect") in
  Prim ("unhandled_effect", f, [e; k])

let unhandled_exn ?(cps=false) e =
  let f = function
    | [e] ->
      Format.printf "Unhandled exception: %a\n%!" print_t e;
      if cps then
        let _k = Ident.create "_" in
        lam [_k] unit
      else unit
    | _ -> raise (Invalid_argument "unhandled_exn") in
  Prim ("unhandled_exception", f, [e])

(* CPS transformation for a toplevel term: CPS transforms it, and
   applies it to "identity" continuations. *)
let cps_main e =
  Ident.id := 0;
  let x = Ident.create "x" in
  let k = Ident.create "k" in
  let kv = Ident.create "kv" in
  let _1 = Ident.create "hx" in
  let _2 = Ident.create "hf" in
  let res =
  app (cps e) [
    lam [x; _1; _2; k] (app (Var k) [Var x]);
    lam [x] (unhandled_exn ~cps:true (Var x));
    lam [x; kv] (unhandled_effect ~cps:true (Var x) (Var kv));
    lam [x] (Var x)
  ]
  in
  res

(* Interpreter ****************************************************************)

let rec eval env = function
  | Var v ->
      begin try Ident.Map.find v env
      with Not_found ->
        Printf.printf "DEBUG: %s\n%!" v;
        failwith "Unbound identifier"
      end
  | Lambda (_, _) | Atom _ as e -> Closure (env, e)
  | Prim (name, f, args) ->
    let args_values_r =
      List.fold_left (fun args_values_r arg ->
        let Closure (_, arg_val) = eval env arg in
        arg_val :: args_values_r) [] args
    in
    let ret = f (List.rev args_values_r) in
    eval env ret
  | App (u, v) ->
      let f = eval env u in
      let x = eval env v in
      apply f x
  | _ -> failwith ("not handled by the interpreter")

and apply (Closure (envu, u)) (Closure (envv, v) as cv) =
  match u with
  | Lambda (x, e) -> eval (Ident.Map.add x cv envu) e
  | _ ->
    Format.eprintf "DEBUG: %a\n" print_t u;
    failwith "trying to apply a value that is not a function"

(* CEK machine *******************************************************************)

module Cek_machine = struct

  type nonrec environment = environment

  type prim =
    { prim        : term list -> term;
      todo_args   : term list;
      done_args_r : term list }

  type continuation =
    | Done
    | EvalArg of term * environment * continuation
    | Call of term * environment * continuation
    | CallPrim of prim * environment * continuation
    | CallRaise
    | CallPerform of continuation

  type handler = {
    hv  : term;
    hx  : term;
    hf  : term;
    env : environment;
    k   : continuation
  }

  type term += Continue of Ident.t * handler

  type control = term

  type machine = control * environment * handler list * continuation

  type step_result =
  | Todo of machine
  | Done of value

  let step : machine -> step_result = function

  | Var x, e, h, k ->
      begin match Ident.Map.find x e with
      | Closure (e', t) -> Todo (t, e', h, k)
      | exception Not_found ->
        Printf.printf "Cek_machine.step.Var: %s\n%!" x;
        failwith "Unbound identifier"
      end

  | App (t1,t2), e, h, k ->
      Todo (t1, e, h, EvalArg(t2,e,k))

  | Prim (_, prim, arg::todo_args), e, h, k ->
      let p = {prim; todo_args; done_args_r = []} in
      Todo (arg, e, h, CallPrim (p, e, k))

  | Handle (body, {hv = v,vv; hx = x, vx; hf = f, vk, vf}), e, h, k ->
      let h' = {hv = lam [v] vv; hx = lam [x] vx;
                hf = lam [f;vk] vf; env = e; k}
      in
      Todo (body, e, h'::h, Done)

  | Continue (x, h), e, hs, k ->
      let e' = Ident.Map.add x (Ident.Map.find x e) h.env in
      Todo (Var x, e', {h with k = k; env = e}::hs, h.k)

  | Raise t, e, h, k -> Todo (t, e, h, CallRaise)

  | Perform t, e, h, k -> Todo (t, e, h, CallPerform k)

  | Lambda (x,t), e, h, EvalArg (t',e',k) ->
      Todo (t', e', h, Call(Lambda(x,t), e, k))

  | t, e, h, Call(Lambda(x',t'),e',k) ->
      let e'' = Ident.Map.add x' (Closure(e, t)) e' in
      Todo (t', e'', h, k)

  | t, _, h, CallPrim ({prim; todo_args; done_args_r}, e', k) ->
      begin match todo_args with
      | [] -> Todo (prim (List.rev (t::done_args_r)), e', h, k)
      | x::xs ->
          let p = {prim; todo_args = xs; done_args_r = t::done_args_r} in
          Todo (x, e', h, CallPrim (p, e', k))
      end

  | t, _, h::hs, CallRaise -> Todo (App (h.hx, t), h.env, hs, h.k)

  | t, e, h::hs, CallPerform k ->
      let x = Ident.create "cv" in
      let cont = lam [x] (Continue (x, {h with k = k; env = e})) in
      Todo (app h.hf [t; cont], h.env, hs, h.k)

  | t, e, [], Done -> Done (Closure (e,t))

  | t, e, h::hs, Done -> Todo (App (h.hv, t), h.env, hs, h.k)

  | t,_,_,_ ->
      Format.eprintf "DEBUG: %a\n" print_t t;
      failwith "Cek_machine.step: no step defined"

  let default_handler =
    let x = Ident.create "x" in
    let k = Ident.create "k" in
    { hv = lam [x] (Var x);
      hx = lam [x] (unhandled_exn (Var x));
      hf = lam [x; k] (unhandled_effect (Var x) (Var k));
      env = Ident.Map.empty;
      k = Done}

  let rec eval p =
    let rec loop i m =
      match step m with
      | Todo m -> loop (i+1) m
      | Done v -> v
    in
    loop 0 (p,Ident.Map.empty,[default_handler],Done)
end

(* Examples *******************************************************************)

let eval = eval Ident.Map.empty

(* Prints a term, evaluates it, and prints the result. *)
let ev t =
  Format.printf "%a\n" print_t t;
  let Closure (_, res) = eval t in
  Format.printf "\n>> %a\n%!" print_t res

let rec check_scope env = function
  | Var v -> (try Ident.Map.find v env; [] with Not_found -> [Var v])
  | Lambda (x, e) -> check_scope (Ident.Map.add x () env) e
  | App (e1, e2) ->
    (check_scope env e1) @ (check_scope env e2)
  | Atom _ -> []
  | Prim (_,_,_) -> []
  | _ -> failwith "not handled"

(* default handlers *)

let identity =
  let v = Ident.create "v" in
  v, Var v

let reraise =
  let vx = Ident.create "vx" in
  let f = function
    | [e] -> Raise e
    | _ -> raise (Invalid_argument "reraise") in
  vx, Prim ("reraise", f, [Var vx])

let test s p ret out =
  assert (!debug = true);
  Printf.printf "(** Test %s **)\n%!" s;
  let run k e p =
    Printf.printf "%s: %!" k;
    let Closure (_,x) = e p in
    assert (x = ret);
    let out_seen = (Format.flush_str_formatter ()) in
    assert (String.equal out out_seen);
    print_endline "success"
  in
  run "simple" eval (cps_main p);
  run "cek" Cek_machine.eval p

(** Examples *)

let ex0 =
  let x = Ident.create "x" in
  app (lam [x] (Var x)) [int 3]

let _ = test "ex0" ex0 (int 3) ""

let ex01 = int 3

let _ = test "ex01" ex01 (int 3) ""


let ex1 =
  let x = Ident.create "x" in
  printl (app (lam [x] (Var x)) [Atom (Int 3)])

let _ = test "ex1" ex1 unit "3
"

let ex11 =
  seq [printl (string "a"); printl (string "b")]

let _ = test "ex11" ex11 unit "a
b
"

let ex12 =
  let x = Ident.create "x" in
  app (lam [x] (Var x)) [app (lam [x] (Var x)) [int 3]]

let _ = test "ex12" ex12 (int 3) ""

let ex13 = Raise (int 0)

let _ = test "ex13" ex13 unit ""

let ex14 = Perform (int 0)

let _ = test "ex14" ex14 unit ""

let ex2 =
  seq [printl (int 3);
       printl (string "abc");
       printl (string "def")]

let _ = test "ex2" ex2 unit "3
abc
def
"

let ex30 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (seq [printl (string "abc")])
    { hv = identity;
      hx = reraise;
      hf = e, k, continue (Var k) (int 18) }

let _ = test "ex30" ex30 unit "abc
"

let ex31 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (seq [printl (Perform (int 0))])
    { hv = identity;
      hx = reraise;
      hf = e, k, int 18 }

let _ = test "ex31" ex31 (int 18) ""

let ex32 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (Perform (int 0))
    { hv = identity;
      hx = reraise;
      hf = e, k, continue (Var k) (int 18) }

let _ = test "ex32" ex32 (int 18) ""

let ex3 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (seq [printl (string "abc");
          printl (Perform (int 0));
          printl (string "def")])
    { hv = identity;
      hx = reraise;
      hf = e, k, continue (Var k) (int 18) }

let _ = test "ex3" ex3 unit "abc
18
def
"

let ex4 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (handle
      (seq [printl (string "abc");
            printl (Perform (int 0));
            printl (string "def")])
      { hv = identity;
        hx = reraise;
        hf = delegate; })
    { hv = identity;
      hx = reraise;
      hf = e, k, continue (Var k) (int 18) }

let _ = test "ex4" ex4 unit "abc
18
def
"

let ex5 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (seq [printl (string "abc");
          printl (Perform (int 0));
          printl (string "def")])
    { hv = identity;
      hx = reraise;
      hf = e, k, seq [continue (Var k) (int 18);
                      printl (string "handler end")] }

let _ = test "ex5" ex5 unit "abc
18
def
handler end
"

let ex6 =
  seq [
    handle unit
      { hv = identity;
        hx = reraise;
        hf = delegate; };
    printl (string "abc")
  ]

let _ = test "ex6" ex6 unit "abc
"

let ex7 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  let v = Ident.create "my_v" in
  handle
    (seq [Perform unit; Perform unit])
    { hv = v, seq [printl (string "hv"); Var v];
      hx = reraise;
      hf = e, k, seq [
          printl (string "hf1");
          continue (Var k) unit;
          printl (string "hf2")
        ]
    }

let _ = test "ex7" ex7 unit "hf1
hf1
hv
hf2
hf2
"

let ex8 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  handle
    (seq [printl (string "a");
          printl (Perform (int 0));
          printl (string "b")])
    { hv = identity;
      hx = reraise;
      hf = e, k, continue (seq [printl ex31; Var k]) (int 19) }

let _ = test "ex8" ex8 unit "a
18
19
b
"

(* Evaluates to 1 *)
let ex9 =
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  let v = Ident.create "my_v" in
  handle
    (handle
       (Perform unit)
       { hv = identity;
         hx = v, int 0;
         hf = e, k, Raise unit; };)
   { hv = identity;
     hx = v, int 1;
     hf = delegate; }

let _ = test "ex9" ex9 (int 1) ""

let ex10 =
  let v = Ident.create "my_v" in
  let e = Ident.create "my_e" in
  let k = Ident.create "my_k" in
  printl
  (handle
     (handle
        unit
        { hv = v, Perform unit;
          hx = reraise;
          hf = delegate })
     { hv = v, printl (Var v);
       hx = reraise;
       hf = e, k, int 3 }
  )

let _ = test "ex10" ex10 unit "3
"

let debug = ref false