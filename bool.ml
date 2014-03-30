(*Open Core.Std*)

type 'a bool = 
  | True
  | False
  | Var of 'a
  | Not of 'a bool
  | Or of 'a bool list
  | And of 'a bool list

type 'a literal =
  | Lit of 'a
  | Til of 'a



type 'a clause = Clause of 'a literal list

type 'a cnf = CNF of 'a clause list

let add x lst = 
  if List.mem x lst then lst else x :: lst

let rec del x = function 
  | [] -> []
  | y :: lst -> if x == y then lst else y :: del x lst


let rec lookup x = function 
  | [] -> None
  | ( y, z ) :: lst -> if x == y then Some z else lookup x lst

let rec value v p =
  match p with 
  | True -> true
  | False -> false
  | Var x -> List.assoc x v
  | Not q -> not ( value v q )
  | Or qs -> value_or v qs
  | And qs -> value_and v qs

and value_or v = function 
  | [] -> false
  | p :: ps -> ( value v p ) || ( value_or v ps )

and value_and v = function 
  | [] -> true
  | p :: ps -> ( value v p ) && ( value_and v ps )

let var p = 
  let rec collect xs = function 
    | False -> xs
    | True  -> xs
    | Var x -> add x xs
    | Not p -> collect xs p
    | And lst -> List.fold_left collect xs lst
    | Or  lst -> List.fold_left collect xs lst
  in 
     collect [] p


let rec nnf = function
  | True -> True
  | False -> False
  | Var x -> Var x
  | Not True -> False
  | Not False -> True
  | Not ( Var x ) -> Not ( Var x )
  | Not ( Not q ) -> nnf q
  | Not ( Or qs ) -> And ( List.map ( fun q -> nnf ( Not q ) ) qs )
  | Not ( And qs ) -> Or ( List.map ( fun q -> nnf ( Not q ) ) qs ) 
  | Or qs -> Or ( List.map nnf qs )
  | And qs -> And ( List.map nnf qs )

let cnf p =
  let rec convert cs = function
    | True -> cs
    | False -> [ Clause [] ]
    | Var x -> ( Clause [ Lit x ] ) :: cs 
    | Not ( Var x ) -> ( Clause [ Til x ] ) :: cs 
    | Not _ -> assert false
    | And ps -> List.fold_left convert cs ps
    | Or [] -> [ Clause [] ]
    | Or [p] -> convert cs p
    | Or ( p :: ps ) -> 
       let ds = convert [] p in 
       let es = convert [] ( Or ps ) in 
          List.fold_left 
	    ( fun cs ( Clause d ) -> 
	       List.fold_left ( fun cs ( Clause e ) -> ( Clause ( d @ e ) ) :: cs ) cs es )
	    cs ds
  in
    CNF ( convert [] ( nnf p ))


let var_cnf ( CNF cs ) = 
  List.fold_left ( fun xs ( Clause c ) -> 
		   List.fold_left 
		     ( fun xs l ->
		         match l with 
			 | Lit x -> x :: xs 
			 | Til x -> x :: xs )
		     xs c )
		 [] cs
					   
