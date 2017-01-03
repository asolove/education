par int: bushels = 100;

var 0..50: women;
var 0..34: men;
var 0..200: children;

constraint 3*men + 2*women + children div 2 = bushels;
constraint 5 * women = men;

solve satisfy;
