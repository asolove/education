/*
  Intermediate lamba language
*/

type var  = int;

type accessPath =
  | Offset int
  | Select int accessPath;

/* Describes runtime representations of data constructors */
type conRep =
  | Undecided
  | Tagged int
  | Constant int
  | Transparent
  | TransU
  | TransB
  | Ref
  | Variable var accessPath
  | VariableC var accessPath;

/* Describes runtime representations of denotable values */
type con = 
  | DataCon conRep
  | IntCon int
  | RealCon string
  | StringCon string;


type lexp =
  | Var var
  | Fn var lexp
  | Fix (list var) (list lexp) lexp
  | App lexp lexp
  | Int int
  | Real string
  | String string
  | Switch lexp (list conRep) (list (con, lexp)) (option lexp)
  | Con conRep lexp
  | Record (list lexp)
  | Select int lexp
  | Raise lexp
  | Handle lexp lexp
  | Prim primop;