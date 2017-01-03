int: x = 1;
int: y = 2;
var int: z;

constraint x+y=z;

solve satisfy;

output ["Hello World!"]