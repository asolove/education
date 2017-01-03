par int: n;
array[0..n-1] of var 0..9: x;

constraint sum(x) == product(x);
constraint forall(i in 0..n-2)(x[i] <= x[i+1]);

solve satisfy;