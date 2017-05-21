type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp

val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* Ex 1: maximum number of args to print statement *)
fun maxL [x] = x
  | maxL (x::xs) = Int.max (x, maxL xs)

fun maxargs (CompoundStm (s1, s2)) = Int.max (maxargs s1, maxargs s2)
  | maxargs (AssignStm (id, exp)) = maxargsE exp
  | maxargs (PrintStm es) = maxL ((length es)::(map maxargsE es))

and maxargsE (IdExp x) = 0
  | maxargsE (NumExp x) = 0
  | maxargsE (OpExp x) = 0
  | maxargsE (EseqExp (s, e)) = maxargs s

val () = print (Int.toString (maxargs prog))

(* Ex 2: interpreter *)

type store = (id * int) list

val emptyStore = []

fun update (id: id, value: int, store: store): store = (id, value)::store

exception VariableError
fun lookup (id, []) = raise VariableError
  | lookup (id, ((id2, v)::rest)): int = if id = id2 then v else lookup (id, rest)

fun applyOp (Plus, v1, v2) = v1 + v2
  | applyOp (Minus, v1, v2) = v1 - v2
  | applyOp (Times, v1, v2) = v1 * v2
  | applyOp (Div, v1, v2) = v1 div v2

fun interpStm (CompoundStm (s1, s2), store): store = interpStm (s2, (interpStm (s1, store)))
  | interpStm (AssignStm (id, exp), store) =
    let
        val (v, store1) = interpExp (exp, store)
    in
        update(id, v, store1)
    end
  | interpStm (PrintStm [], store) = store
  | interpStm (PrintStm (e::es), store) =
    let
        val (v, store1) = interpExp (e, store)
    in
        print (Int.toString v);
        interpStm (PrintStm es, store)
    end

and interpExp (IdExp x, store): int * store = (lookup (x, store), store)
  | interpExp (NumExp x, store) = (x, store)
  | interpExp (OpExp (e1, oper, e2), store) =
    let
        val (v1, store1) = interpExp (e1, store)
        val (v2, store2) = interpExp (e2, store1)
    in
        (applyOp (oper, v1, v2), store2)
    end
  | interpExp (EseqExp (s, e), store) =
    let 
        val store1 = interpStm (s, store)
    in
        interpExp (e, store1)
    end

val s = interpStm (prog, emptyStore)