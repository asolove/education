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

{Show {Fact 10}}

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
local
   fun {ReverseIter As Rs}
      case As
      of nil then Rs
      [] A|Ar then {ReverseIter Ar A|Rs}
      end
   end
in
   fun {Reverse As}
      {ReverseIter As nil}
   end
end
fun {NestedListLength Xs}
   case Xs
   of nil then 0
   [] X|Xr andthen {IsList X} then
      {NestedListLength X} + {NestedListLength Xr}
   [] X|Xr then
      1 + {NestedListLength X}
   end
end

declare
fun {Merge Xs Ys}
   case Xs # Ys
   of nil # Ys then Ys
   [] Xs # nil then Xs
   [] (X|Xr) # (Y|Yr) then
      if X<Y
      then X | {Merge Xr Ys}
      else Y | {Merge Xs Yr}
      end
   end
end
proc {Split Xs ?Ys ?Zs}
   case Xs
   of nil then Ys=nil Zs=nil
   [] [Y] then Ys=[Y] Zs=nil
   [] Y|Z|Xr then Yr Zr in
      Ys=Y|Yr
      Zs=Z|Zr
      {Split Xr Yr Zr}
   end
end
fun {MergeSort Xs}
   case Xs
   of nil then Xs
   [] [X] then [X]
   else Ys Zs in
      {Split Xs Ys Zs}
      {Merge {MergeSort Ys} {MergeSort Zs}}
   end
end

% The use of accumulators in this section is very interesting
% in contrast with the expression-oriented approach I am more
% familiar with.

% Difference lists
declare
fun {AppendD D1 D2}
   S1#E1 = D1
   S2#E2 = D2
in
   E1=S2
   S1#E2
end

declare
local
   proc {FlattenD Xs ?S E}
      case Xs
      of nil then S=E
      [] X|Xr andthen {IsList X} then Y2 in
         {FlattenD X S Y2}
         {FlattenD Xr Y2 E}
      [] X|Xr then Y1 in
         S=X|Y1
         {FlattenD Xr Y1 E}
      end
   end
in
   fun {Flatten Xs}
      {FlattenD Xs $ nil}
   end
end


% Follow the d list through the tree:
% {Flatten [1 [2] 3]}
% {FlattenD [1 [2] 3] S1 nil} (S1=_)
% {FlattenD [[2] 3] S2 nil} (S2=_, S1=1|S2)
% a. {FlattenD [2] S2 Y2} (S2=_, S1=1|S2)
%    {FlattenD [] S3 Y2} (S3=_, S2=2|S3, S1=1|S2)
%    S3=Y2=_
% b. {FlattenD [[3]] Y2 nil} (Y2=S3=_, S2=2|S3, S1=1|S2)
% b. 1. {FlattenD [3] Y2 Y3} (Y2=S3=_, Y3=_, ...)
%       Y2=3|Y3
% b. 2. {FlattenD [] Y2 nil} (Y2=S3=3|nil, S2=2|S3, ...
%
% so S1 = 1|2|3|nil, nifty

% Or, we can see the difference list as two separate values,
% S#E: E is actually input to the function, it means "the rest of
% the answer will eventually be in this variable". So the function
% calculates what part of the answer it provides and then tacks on
% E where the downstream part of the answer should go.
declare
local
   fun {FlattenD2 Xs E}
      case Xs
      of nil then E
      [] X|Xr andthen {IsList X} then
         {FlattenD2 X {FlattenD2 Xr E}}
      [] X|Xr then
         X|{FlattenD2 Xr E}
      end
   end
in
   fun {Flatten2 Xs}
      {FlattenD2 Xs nil}
   end
end

declare
local
   proc {ReverseD2 Xs ?Y1 Y}
      case Xs
      of nil then Y1=Y
      [] X|Xr then
         {ReverseD2 Xs Y1 X|Y}
      end
   end
in
   fun {Reverse2 Xs}
      {ReverseD2 Xs Y1 nil}
      Y1
   end
end


% Big question: can you use different other things? records? trees?
% Small question: I can now read functions with diff lists, but I
% don't think I could write them from scratch. Is that expected after
% current material?

% 3.4.5: Queues

% Okasaki-style amortized constant time queue in strict FP:

% declare
% fun {NewQueue} q(nil nil) end
% fun {Check Q}
%    case Q of q(nil R) then q({Reverse R} nil) else Q end
% end
% fun {Insert Q X}
%    case Q of q(F R) then {Check q(F X|R)} end
% end
% fun {Delete Q ?X}
%    case Q of q(F R) then F1 in F=X|F1 {Check q(F1 R)} end
% end
% fun {IsEmpty Q}
%    case Q of q(nil nil) then true else false end   
% end

% Difference list version
declare
fun {NewQueue} X in q(0 X X) end
fun {Insert Q X}
   case Q of q(N S E) then E1 in E=X|E1 q(N+1 S E1) end
end
fun {Delete Q X}
   case Q of q(N S E) then S1 in S=X|S1 q(N-1 S1 E) end
end
fun {IsEmpty Q}
   case Q of q(N S E) then N==0 end
end
% This is starting to get more natural. I can write these myself.

% 3.4.6: Trees

declare
fun {Lookup X T}
   case T
   of leaf then notfound
   [] tree(Y V T1 T2) then
      if X<Y then {Lookup X T1}
      elseif X>Y then {Lookup X T2}
      else found(V) end
   end
end
