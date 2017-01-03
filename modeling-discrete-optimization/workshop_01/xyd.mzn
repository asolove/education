var 0..9: x;
var 0..9: y;

constraint x >= 4 \/ y <= 5;
constraint y >= x+4 \/ (x >= y+6 /\ x+y >= 8);

solve satisfy;

output ["(" ++ show(x) ++ "," ++ show(y) ++")"];