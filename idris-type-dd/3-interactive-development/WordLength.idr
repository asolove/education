import Data.Vect

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

insertionSort : (Ord a) => Vect n a -> Vect n a
insertionSort [] = []
insertionSort (x :: xs) = let xsSorted = insertionSort xs in
                              insert x xsSorted
