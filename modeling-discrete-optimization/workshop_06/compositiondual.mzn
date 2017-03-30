par int: n = 10;

set of int: Note = 1..n;
set of int: Position = 1..n;

array[Position] of var Note: order;
array[Note] of var Position: posn;

array[1..n-1] of var 1..n-1: diffs = [abs(order[p] - order[p+1]) | p in 1..n-1];
array[1..n-1] of var 1..n-1: dposn;

include "inverse.mzn";
constraint inverse(order, posn);
constraint inverse(diffs, dposn);

constraint forall(d in 1..n-1)
                 (dposn[d] = min(posn[(n-d) div 2], posn[(n+1) div 2 + d div 2]));


solve satisfy;