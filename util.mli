val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val id : 'a -> 'a
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val const : 'a -> 'b -> 'a
val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
