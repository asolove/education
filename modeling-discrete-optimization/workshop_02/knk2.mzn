% each var represents whether person is a knight
array[1..3] of var bool: knight;
var 1..3: knights;

constraint knights = sum([1|i in 1..3 where knight[i]]);

constraint knight[1] -> (knights = 1);
constraint knight[2] -> (knights = 2);
constraint knight[3] -> (knights = 3);

solve satisfy;