open Core;
open Core_kernel;
open Core_bench.Std;

/* A very naive solution with lots of intermediate results creating garbage. */
let naiveEval = (coefficients, x) => {
  coefficients
  |> List.rev
  |> List.mapi((i, c) => c *. (x ** float_of_int(i)))
  |> List.fold_left((t1, t2) => t1 +. t2, 0.)
};

/* Using the same naive algorithm but without creating much garbage. */
let lessGcEval = (coefficients, x) => {
  let rec step = (cs, pow, so_far) =>
    switch(cs) {
    | [] => so_far
    | [c, ...cs] => step(cs, pow -. 1., so_far +. (c *. (x ** pow)))
    };

  step(coefficients, float_of_int(List.length(coefficients)) -. 1., 0.)
}; 

/* Replace an exponentiation with a multiplication by passing on power of x */
let lessNaiveEval = (coefficients, x) => {
  let rec step = (cs, factor, so_far) =>
    switch(cs) {
    | [] => so_far
    | [c, ...cs] => {
      let factor' = factor /. x;
      step(cs, factor', so_far +. (c *. factor'))
    }
    };

  step(coefficients, x ** float_of_int(List.length(coefficients)), 0.)
}; 

/* fasterEval uses Horner's rule to only need one addition and one multiplication per term */
let fasterEval = (coefficients, x) => {
  List.fold_left((acc, c) => acc *. x +. c, 0., coefficients)
};

Core_random.init(2);



Core_random.init(10);
let coefficients = 
  Core_list.range(0, 100000)
  |> List.map((_i) => Core_random.float(1.));
let x = Core_random.float(1.);


/* 
/*
Convenient debugging case: makes it clear what the algo does wrong
*/
let coefficients = [5., 4., 3., 2., 1.];
let x = 10.; */


let a1 = naiveEval(coefficients, x);
let a2 = lessGcEval(coefficients, x);
let a3 = lessNaiveEval(coefficients, x);
let a4 = fasterEval(coefficients, x);

let test =
  if (a1 != a2 || a1 != a3 || a1 != a4) {
    Printf.printf("Answer mismatch. Got: %f, %f, %f and %f for x=%f\n", a1, a2, a3, a4, x);
    false
  } else {
    true
  };

Command.run(Bench.make_command([
  Bench.Test.create("naive", () => ignore(naiveEval(coefficients, x))),
  Bench.Test.create("less gc", () => ignore(lessGcEval(coefficients, x))),
  Bench.Test.create("less naive", () => ignore(lessNaiveEval(coefficients, x))),
  Bench.Test.create("faster", () => ignore(fasterEval(coefficients, x)))
]));