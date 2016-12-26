% Getting a feel for how procs/case/destructuring works...
local
   proc {LocalStep Items}
      case Items
      of A#B|Rest then
         {Browse A}
         {Browse B}
         {LocalStep Rest}
      else
         {Browse 10}
      end
   end
   R
in
   {LocalStep 2#3|nil}
end

% MooC exercise: agent to evaluate single-arg functions
declare Eval
fun {Eval Port}
   local InputP Input
      proc {Step Items}
         case Items
         of (Function#Arg)|Rest then
            {Send Port {Function Arg}}
            {Step Rest}
         else
            {Send Port 100}
         end
      end
   in
      InputP = {NewPort Input}
      thread {Step Input} end
      % {Browse Input}
      InputP
   end
end

local
   fun {PlusOne X}
      X+1
   end
   OutputP
   Output
   InputP
in
   OutputP = {NewPort Output}
   InputP = {Eval OutputP}
   {Browse Output}
   {Send InputP PlusOne#2}
end