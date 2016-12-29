% Ugh the Oz emacs mode syntax doesn't understand block comments
% and tries to parse plain text inside them. So have to use % if
% you actually want to mix code and text.

% 3.1: Declarativeness
%
% I am curious how the authors description of specifications
% and proofs would be different if written today, 15 years later.

% 3.2: Iterative computation
%
declare
fun {Sqrt X}
   fun {SqrtIter Guess}
      if {GoodEnough Guess} then Guess
      else
         {SqrtIter {Improve Guess}}
      end
   end
   
   fun {Improve Guess}
      (Guess + X/Guess) / 2.0
   end
   
   fun {GoodEnough Guess}
      {Number.abs Guess*Guess - X}/X < 0.00001
   end
   Guess = 1.0
in
   {SqrtIter Guess}
end

% Abstract iteration into a higher-order function:
declare
fun {Iterate S Done Step}
   if {Done S} then S
   else
      {Iterate {Step S} Done Step}
   end
end
fun {Sqrt2 X}
   {Iterate 1.0
    fun {$ G} {Number.abs G*G-X}/X < 0.00001 end
    fun {$ G} (G+X/G) / 2.0 end}
end

% I thought the pedagogy of this example was really well done.
% We started with pseudocode for the iteration process, then made
% it into a concrete use of iteration to solve one problem, then
% stepped back up a level and built a function that wraps up the
% pattern. There was no grand explanation of higher-order functions,
% nor did there need to be, because this function seems like a
% very natural step from the pseudocode and the code for Sqrt.

% 3.3: Recursive computation
%
% The operational semantics notation introduced in ch2 is put to
% good use here. One of the most awkward parts of SICP is the
% places where it shows stack growth in recursive processes in
% terms of diagrams, or partial expressions with (...) in them, etc.
% But here, the operational semantics give us a great notation for
% showing this directly:
%   [{Fact 3 r2}, r1=4*r2, r0=5*r1]
% Makes clear both what step we're on and what work we're leaving
% behind to be done when the recursion bottoms out.
%
% A good reminder: the right notation sometimes solves the problem.

declare
fun {Fact N}
   fun {FactIter N R}
      if N==0 then R
      elseif N>0 then {FactIter N-1 R*N}
      else raise domainError end
      end
   end
in
   {FactIter N 1}
end

% 3.4: Programming with recusion
% Some very key ideas presented here:
% - Using state invariants when building recursive functions
% - Natural recusion by just following the data type

declare
fun {Append As Bs}
   case As
   of nil then Bs
   [] A|Ar then A|{Append Ar Bs}
   end
end
fun {Reverse As}
   fun {ReverseIter As Rs}
      case As
      of nil then Rs
      [] A|Ar then {ReverseIter Ar A|Rs}
      end
   end
in
   {ReverseIter As nil}
end

