array[int] of par int: readings;
par int: start;

set of int: CHOICE = 1..5;

array[CHOICE] of int: price = [10, 50, 30, 90, 0];
array[CHOICE] of int: change = [1, 4, -2, -5, 0];

array[1..length(readings)] of var CHOICE: choice;
array[1..length(readings)+1] of var 16..22: temp;

constraint temp[1] = start;
constraint length(temp) = length(readings)+1;
constraint forall(i in 2..length(readings)+1)
                  (temp[i] = (temp[i-1] + readings[i-1]) div 2 + change[choice[i-1]]);
                  
var int: cost = sum(c in choice)(price[c]);

solve minimize cost;

output [show(cost) ++ show(choice)]