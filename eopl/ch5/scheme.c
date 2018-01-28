
/* Tiny AST for a simple Lisp program with closures:
 * (let (x 2)
 *   (let (times2 (lambda (y) (* x y)))
 *     (times2 3)))
 */
MakeLet(MakeSymbol(1), MakeInt(2),
    MakeLet(MakeSymbol(2), MakeLambda(MakeSymbol(3), MakeTimes(MakeSymbol(1), MakeSymbol(3))),
      MakeCall(MakeSymbol(2), MakeInt(3))));

typedef struct let_expr_t {
  symbol* var;
  expr_t* val;
  expr_t* body;
} let_expr_t;
