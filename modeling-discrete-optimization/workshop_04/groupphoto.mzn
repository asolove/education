par int: n;
set of int: PERSON = 1..n;
array[PERSON, PERSON] of par bool: friend;

int: cols = n div 2;

array[1..cols, 1..2] of var PERSON: person;

include "alldifferent.mzn";
constraint alldifferent([ person[col, row] | col in 1..cols, row in 1..2]);

var int: obj = sum(col in 1..cols, row in 1..2, col2 in 1..cols, row2 in 1..2 where (col = col2 /\ row < row2) \/ col2 - col = 1 /\ row = row2)
                  (friend[person[col, row], person[col2, row2]]);

solve maximize obj;

output [show(obj) ++ "\n" ++ show(person)]