/*
meth: isBalanced($) : Returns true if the tree is balanced
              Reminder: A tree is balanced if its two subtrees have the same amount of leaves (with a difference of maximum 1) and are balanced themselves.
                                                                      */

declare Tree
class Tree

   %% Basic attribute stuff
   attr value:any left:Tree right:Tree

   meth init(V)
      value := V
      left := leaf
      right := leaf
   end
   meth setLeft(T)
      left := T
   end
   meth setRight(T)
      right := T
   end
   meth setValue(V)
      value := V
   end
   
   meth getLeft($)
      @left
   end
   meth getRight($)
      @right
   end
   meth getValue($)
      @value
   end

   
   meth isBalanced(Ans)
      LBalanced RBalanced LCount RCount in
      LBalanced = if @left == leaf then true else {@left isBalanced()} end
      RBalanced = if @right == leaf then true else {@right isBalanced()} end
      LCount = if @left == leaf then 0 else {@left count()} end
      RCount = if @right == leaf then 0 else {@right count()} end
      Ans = LBalanced andthen RBalanced andthen {Number.abs (LCount - RCount)} < 2
   end

   meth leaves(Ans)
      LCount RCount in
      LCount = if @left == leaf then 0 else {@left leaves()} end
      RCount = if @right == leaf then 0 else {@right leaves()} end
      Ans = LCount + RCount + 1
   end
   
end

declare L R T X Y
L = {New Tree init(10)}
T = {New Tree init(5)}
{T setLeft(L)}
{Browse {T leaves(Y)}}
{Browse {T isBalanced(X)}}