
declare
fun {Fact N}
   if N==0 then 1 else N*{Fact N-1} end
end

% {Browse {Fact 10}}

declare
fun {Comb N R}
   {Fact N} div ({Fact R}*{Fact N-R})
end

% {Browse {Comb 10 3}}

declare
fun {ShiftLeft Xs}
   {List.append Xs [0]}
end
fun {ShiftRight Xs}
   0|Xs
end
fun {AddList X|Xs Y|Ys}
   X+Y| if Xs==nil then nil else {AddList Xs Ys} end
end
fun {Pascal N}
   if N==1 then [1]
   else Prev = {Pascal N-1} in
      {AddList {ShiftLeft Prev} {ShiftRight Prev}}
   end
end

% {Browse {Pascal 20}}

fun lazy {Ints N}
   N|{Ints N+1}
end

/*
declare Is
Is={Ints 0}
{Browse Is}
{Browse Is.2.1}
*/

declare
fun lazy {PascalList Row}
   Row|{PascalList {AddList {ShiftLeft Row} {ShiftRight Row}}}
end

declare Ps
Ps = {PascalList [1]}
{Browse Ps.2.2.2.2.2.1}

declare
fun {GenericPascal Op N}
   if N==1 then [1]
   else P={GenericPascal Op N-1} in
      {OpList Op {ShiftLeft P} {ShiftRight P}}
   end
end
fun {OpList Op Xs Ys}
   case Xs of X|Xr then
      case Ys of Y|Yr then
         {Op X Y}|{OpList Op Xr Yr}
      end
   else nil end
end
fun {Xor X Y}
   if X==Y then 0 else 1 end
end

/*
declare
Xs = {GenericPascal Xor 7}
{Browse Xs}
*/

local C={NewCell 0}
   fun {Add X Y}
      C:=@C+1
      X+Y
   end
   fun {FastPascalCount N}
      {GenericPascal Add N}
   end
in
   %%{Browse {FastPascalCount 10}}
   %%{Browse @C}
end

local C={NewCell 0}
   fun {Bump}
      C:=@C+1
   end
   fun {Read}
      @C
   end
in
   {Browse {Bump}}
   {Browse {Read}}
end

declare
fun {NewCounter}
   C Bump Read
in
   C = {NewCell 0}
   fun {Bump}
      C:=@C+1
   end
   fun {Read}
      @C
   end
   counter(bump: Bump read:Read)
end

local C = {NewCounter} in
   _ = {C.bump}
   % {Browse C}
   % {Browse {C.read}}
end


%% Chapter 1 exercises

%% Ex 2: Faster Comb
local
   fun {Mult A B}
      A*B
   end
   fun {RangeProduct Lo Hi}
      {FoldL {List.number Lo Hi 1} Mult 1}
   end
   fun {Comb N R1}
      R = if R1 > N div 2 then R1 else N-R1 end in
      {RangeProduct (N-R+1) N} div {RangeProduct 1 R}
   end
in
   % {Browse {Comb 10 5}}
end

%% Ex 9: memory store
declare
local
   Rows={NewStore}
in
   fun {FasterPascal N}
      M = {Size Rows}
   in
      if N==0 then
         {Put Rows 0 [1]}
      elseif N>=M then
         Prev = {FasterPascal N-1}
         Curr = {AddList {ShiftLeft Prev} {ShiftRight Prev}}
      in
         {Put Rows N Curr}
      end
      {Get Rows N}
   end
end

{Browse {FasterPascal 20}}

/*
Thoughts on Chapter 1

I liked the pedagogical approach of showing lots of paradigms
quickly in order to motivate the rest of the text. For someone
familiar with many of these ideas, it was still a useful first
introduction to the syntax and code style in Oz. The example
of Pascal's triangle works really well in the early part of the
chapter and the higher-order programming and lazy stream steps
feel very easy and surprisingly elegant. This is heady stuff &
I imagine someone new to FP would be in awe.

In contrast, the applications of this example to mutable state,
OO, and concurrency felt less well motivated and less impressive.
This is probably unavoidable since any process complex enough to
require these paradigms wouldn't fit in a quick tour.

The two code-writing exercises seemed appropriate: the first quite
easy and the second requiring a bit of thought.

The exercises encourage trying out the non-deterministic examples
yet, at least in the version of Mozart I have, they always seemed
to give the same answer (first thread runs first) unless I manuallty
added Delay calls.

I started the book already excited about its approach to paradigms
and semantics and willing to get over my initial distaste for the
syntax of Oz. Wadler's Law requires that the three-space indentation
style is my main complaint.

Having read the first chapter, I now feel comfortable writing simple programs in Oz and am looking forward for my first introduction to the authors' approach to defining semantics.
                                                                                                      */