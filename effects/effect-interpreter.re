/*
Simple interpreter of a language with effect handlers
Originally by @kayceesrk: https://gist.github.com/kayceesrk/26e7cf8b3e9d435062eaddf22a2662c8

Converted to Reason and with some changes by @asolove
*/
module Ident = {
  type t = string;
  let id = ref 0;
  let create base => {
    incr id;
    base ^ "_" ^ string_of_int !id
  };
  module Map =
    Map.Make {
      type nonrec t = t;
      let compare = String.compare;
    };
};

/*
Our lambda calculus, which is standard lambda-calculus, plus:
- atoms;
- primitives: basically ocaml functions, applied to λ-terms.
  The arguments are guaranteed to be reduced before being given
  to the function;
- effects primitives: handle & perform.
*/

type term = ..;

type atom =
  | Unit
  | Int int
  | String string
and handlers = {
  hv: (Ident.t, term), /* Value */
  hx: (Ident.t, term), /* Exception */
  hf: (Ident.t, Ident.t, term) /* Effect */
}
and value =
  | Closure environment term
and environment = Ident.Map.t value;

type term +=
  | Var Ident.t
  | Lambda Ident.t term
  | App term term
  | Atom atom
  | Prim string (list term => term) (list term)
  | Handle term handlers
  | Perform term
  | Raise term;

/* Helpers */

/*
Lambda and App constructors are the simplest possible. To define
lambdas with multiple identifiers at the same time, or application
with multiple arguments, use the [lam] and [app] helpers.
*/
let lam (idents: list Ident.t) (body: term): term =>
  List.fold_right (fun id acc => Lambda id acc) idents body;

let app (f: term) (args: list term): term => {
  let (hd, args) = (List.hd args, List.tl args);
  List.fold_left (fun acc arg => App acc arg) (App f hd) args
};

let rec seq : list term => term = fun
  | [] => Atom Unit
  | [e] => e
  | [e, ...es] => {
    let dummy = Ident.create "_";
    App (Lambda dummy (seq es)) e
  };

let letin (x: Ident.t) (e1: term) (e2: term): term =>
  App (Lambda x e2) e1;

/* Helpers for building atomic terms */
let int x => Atom (Int x);
let string s => Atom (String s);
let unit = Atom Unit;

/* Effects wrappers */
let handle body handlers => Handle body handlers;

let delegate = {
  let ve = Ident.create "ve";
  let vk = Ident.create "vk";
  (ve, vk, App (Var vk) (Perform (Var ve)))
};

let continue k v => App k v;

/* Printers */

let printAtom ppf =>
  fun
  | Unit => Format.fprintf ppf "()"
  | Int i => Format.fprintf ppf "%d" i
  | String s => Format.fprintf ppf "%s" s;

let printT ppf => {
  open Format;

  let rec aux paren ppf =>
    fun
    | Var x => fprintf ppf "%s" x
    | Lambda x e =>
      fprintf
        ppf
        "@[<2>%s%a%s@]"
        (if paren {"("} else {""})
        (aux_lambda [x])
        e
        (if paren {")"} else {""})
    | _ => failwith "aux: unknown term"
  and let aux_lambda idents ppf =
    fun
    | Lambda i e => aux_lambda [i, ...idents] ppf e;

  aux false ppf
};