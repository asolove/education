
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
