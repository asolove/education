module Main

main : IO ()
main = putStrLn (cast 'c')

StringOrInt : Bool -> Type
StringOrInt True = Int
StringOrInt False = String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
                        True => 94
                        False => "Ninety four"

valToString : (x: Bool) -> StringOrInt x -> String
valToString x val = case x of
                         True => cast val
                         False => val
