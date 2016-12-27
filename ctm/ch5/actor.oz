% MooC exercise: agent to evaluate single-arg functions
declare Eval
fun {Eval Port}
   Input
 in
    thread
       for Function#Arg in Input do
          {Send Port {Function Arg}}
       end
    end
    % {Browse Input}
    {NewPort Input}
end

local
   fun {PlusOne X}
      X+1
   end
   Output
   InputP
in
   InputP = {Eval {NewPort Output}}
   {Browse Output}
   {Send InputP PlusOne#5}
end
