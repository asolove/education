par int: n; % number of players
par int: m; % number of captains
array[1..m, 1..n] of int: value; % value[c,p] is value captain c places on player p

array[1..n] of var 1..m: teams;

include "globals.mzn";
constraint global_cardinality_low_up_closed(teams, 1..m, [n div m | i in 1..n], [n div m + 1 | i in 1..n]);

var int: obj = sum(i in 1..n)(value[teams[i], i]);
solve maximize obj;

output ["obj = " ++ show(obj) ++ "\nteams = " ++ show(teams)]