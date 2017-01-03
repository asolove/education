% Variables represent each character being guilty
var bool: huey;
var bool: dewey;
var bool: louie;

constraint dewey = louie;
constraint huey -> dewey;
constraint dewey=false \/ louie=false;

solve satisfy;