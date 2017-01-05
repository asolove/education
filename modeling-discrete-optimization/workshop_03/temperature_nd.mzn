array[int] of par int: readings;
par int: start;

array[1..length(readings)+1] of var int: temp;

constraint temp[1] = start;
constraint length(temp) = length(readings)+1;
constraint forall(i in 2..length(readings)+1)
                  (temp[i] = (temp[i-1] + readings[i-1]) div 2);

solve satisfy;