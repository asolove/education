par int: n = 100;

set of int: Note = 1..n;
set of int: Position = 1..n;

array[Position] of var Note: order;
array[1..n-1] of var 1..n-1: diffs = [abs(order[p] - order[p+1]) | p in 1..n-1];

include "alldifferent.mzn";
constraint alldifferent(order);
constraint alldifferent(diffs);

solve satisfy;