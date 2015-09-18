(**************************************************************************)
(*                    LTS with context sensitive reduction                *)
(*  By Mystery Man                                                        *)
(**************************************************************************)

(* the result of Syntactical Correspondence is tagged with @SC*)

(**************************************************************************)
(*                         Low-Level Traces                               *)
(**************************************************************************)

type word = int
and pointer = int

type gamma =
  | Call of pointer * word list  
  | Ret of pointer * word
(* Observable traces *)
type alpha = 
  | Done
  | Attacker of gamma
  | Secure of gamma


(**************************************************************************)
(*                                 LTS                                    *)
(**************************************************************************)

module type Secure = sig 
  val start : pointer -> alpha
  val returnback : word -> alpha
  val apply : word -> word -> pointer -> alpha
  val alloc : word -> word -> pointer -> alpha
  val set : word -> word -> pointer -> alpha
  val deref : word -> pointer -> alpha
end

(* Implementation *)
module SecureML : Secure = 
struct

  (*--------------------------------------------*)
  (*                  AST                       *)       
  (*--------------------------------------------*)

  (* auxiliary types *)
  type variable = string
  type operands = PLUS | MINUS | TIMES | EQUALS | LARGERTHEN | LESSTHEN
  type nametype = FUNCTION | LOCATION
  type name = nametype * int

  (* sec types *)
  type ty = TBool
    | TInt
    | TUnit
    | TApp of ty * ty
    | TLoc of ty

  (* secure language definition *)
  type term = 
    | Var of variable
    | Lam of variable * ty * term
    | App of term * term
    | If of term * term * term
    | Let of variable * term * term
    | Letrec of variable * ty * term * term
    | Fix of term
    | Bool of bool
    | Int of int
    | Unit
    | Sequence of term * term
    | Oper of operands * term * term
    | Location of term ref * int
    | Set of term * term
    | Deref of term
    | Alloc of term
    | Foreign of pointer * ty
    | Hash of term

  (* @SC *)
  type mlkont =
    | Done
    | Ifkont of term * term * mlkont
    | Appkont of term * mlkont
    | Appkont2 of term * mlkont
    | Allockont of mlkont
    | Hashkont of mlkont
    | Operkont of operands * term * mlkont 
    | Operkont2 of operands * term * mlkont  
    | Derefkont of mlkont
    | Setkont of term * mlkont 
    | Setkont2 of term * mlkont 
    | Letkont of variable * term * mlkont 
    | Sequencekont of term * mlkont 
    | Fixkont of mlkont

  (* @SC *)
  type ffikont =
    | Empty
    | Waiting of mlkont * ty * ffikont
    | Executing of mlkont * ty * ffikont
    | Marshallin of ty * ffikont
    | Marshallout of ty * ffikont

  (* value identification *)
  let rec isvalue = function
    | Bool _ | Int _ | Unit | Location _ | Lam _ | Foreign _ -> true
    | _ -> false

  (* the control @SC *)
  type control =
    | Term of term
    | Word of word

  (*--------------------------------------------*)
  (*               Substitution                 *)       
  (*--------------------------------------------*)

  let rec subst var value target = match target with
    | Bool _ -> target
    | Int _ -> target
    | Unit -> Unit
    | Location _ -> target
    | Var str when str = var -> value
    | Var _ -> target
    | Lam (nvar,ty,a) when nvar = var -> target
    | Lam (nvar,ty,a) -> Lam (nvar,ty,(subst var value a))
    | App (a,b) -> App ((subst var value a),(subst var value b))
    | If (a,b,c) -> If ((subst var value a),(subst var value b),(subst var value c))
    | Let (nvar,a,b) when nvar = var -> target
    | Let (nvar,a,b) -> Let (nvar,(subst var value a),(subst var value b))
    | Letrec (nvar,ty,a,b) when nvar = var -> target
    | Letrec (nvar,ty,a,b) -> Letrec (nvar,ty,(subst var value a),(subst var value b))
    | Fix a -> Fix (subst var value a)
    | Sequence (a,b) -> Sequence ((subst var value a),(subst var value b))
    | Foreign _ -> target
    | Set (a,b) -> Set ((subst var value a),(subst var value b))
    | Deref a -> Deref (subst var value a)
    | Alloc a -> Alloc (subst var value a)
    | Hash a -> Hash (subst var value a)
    | Oper (op,a,b) -> Oper (op,(subst var value a),(subst var value b))


  (*--------------------------------------------*)
  (*                   State                    *)       
  (*--------------------------------------------*)

  (* global lists *)
  let ptr_stack = ref []
  let name_map = ref []
  let kontinuation = ref Empty (* @SC *)

  (* function name counter *)
  let fcount = ref 0

  (* location indices *)
  let lcount = ref 0

  (* add to stack *)
  let add_ptr p =  (ptr_stack := p :: !ptr_stack)

  (* pop return adress *)
  let pop_ptr = function () -> 
    try
      let r = (List.hd !ptr_stack) in
      ptr_stack := (List.tl !ptr_stack); r
    with _ -> raise (Failure "Stack is empty")

  (* add function to map *)
  let add_function (lam : term) (tt : ty) : name = 
    fcount := !fcount + 1;
    let n = (FUNCTION,!fcount) in
    (name_map := ((n,lam,tt) :: !name_map)); n
    
  (* add_location *)
  let add_location (loc : term) (tt : ty) : name =
    let n = (match loc with
    | Location (x,i) -> (LOCATION,i) 
    | _ -> raise (Failure "Wrong usage of location map")) in
    name_map := (n,loc,tt) :: !name_map; n

  (* find name in map *)
  let find_name nty i =
    let predicate = function (n,_,_) -> match n with
      | (nn,num) when (nn = nty && num = i) -> true
      | _ -> false
    in
    try let (_,control,typ) = (List.find predicate !name_map) in (control,typ)
    with _ -> raise (Failure "Name not found")


  (*--------------------------------------------*)
  (*             Compute Traces                 *)       
  (*--------------------------------------------*)

  (* compute the trace *)
  let ret_trace w =
    let ptr = pop_ptr () in 
    Secure (Ret (ptr,w))  

  let call_trace ptr w =
    Secure (Call (ptr,[w]))


  (*--------------------------------------------*)
  (*                   MiniML                   *)       
  (*--------------------------------------------*)

  (* marshall out to words *) 
  let marshallout value typ : word = 
    let wordify = function
      | (_,i) -> (-i) in (* negate to differentiate *)
    let lowfunc = function
      | TApp _ -> true
      | _ -> false in
    let lowloc = function
      | TLoc _ -> true
      | _ -> false in
    let word = match value with
      | Bool true when typ = TBool -> 1
      | Bool false when typ = TBool -> 0
      | Int i when typ = TInt -> i
      | Lam _ as z when (lowfunc typ) -> wordify (add_function z typ) 
      | Unit when typ = TUnit -> 0
      | Location _ as z when (lowloc typ) -> wordify (add_location z typ)
      | Foreign (ptr,ty) when ty = typ -> ptr
      | _ -> raise (Failure "Marshalling out Failed") 
    in
    word
      
  (* marshall in words *)
  let marshallin word typ : term = 
    let rec check x y = 
      let ensure p = if not (p y)
        then raise (Failure "typing")
        else ()
      in
      match x with
      | TBool -> ensure (fun z -> z = TBool) 
      | TInt -> ensure (fun z -> z = TInt)
      | TUnit -> ensure (fun z -> z = TUnit)
      | TApp(a,b) -> ensure (function 
        | TApp(aa,bb) -> (check a aa);(check b bb); true
        | _ -> false)
      | TLoc a -> ensure (function 
        | TLoc aa -> (check a aa); true
        | _ -> false)
    in
    match typ with
    | TBool -> (match word with
      | 0 -> (Bool false)
      | _ -> (Bool true))
    | TInt -> (Int word)
    | TUnit -> Unit
    | TLoc a -> (if word < 0 
      then let (loc,ty) = (find_name LOCATION (-word)) in
        (check ty typ); loc
      else raise (Failure "Foreign locaitons not supported"))
    | TApp (a,b) -> (if word < 0
      then let (lam,ty) = (find_name FUNCTION (-word)) in
        (check ty typ); lam
      else Foreign (word,typ))

  (* handle ML kontinuations @SC *)
  let rec plug_kont (v : term) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    let update k' = Executing (k',typ,outerk') in
     (match k with
      | Ifkont (c2,c3,k') -> reduce (If (v,c2,c3)) (update k')
      | Allockont k' -> reduce (Alloc v) (update k')
      | Hashkont k' -> reduce (Hash v) (update k')
      | Operkont (op,c2,k') -> reduce (Oper (op,v,c2)) (update k')
      | Operkont2 (op,v1,k') -> reduce (Oper (op,v1,v)) (update k')
      | Derefkont k' -> reduce (Deref v) (update k')
      | Setkont (c2,k') -> reduce (Set(v,c2)) (update k')
      | Setkont2 (l,k') -> reduce (Set(l,v)) (update k')
      | Letkont (nv,c2,k') -> reduce (Let(nv,v,c2)) (update k')
      | Sequencekont (c2,k') -> reduce (Sequence(v,c2)) (update k')
      | Fixkont k' -> reduce v (update k')
      | Appkont (c2,k') -> reduce (App (v,c2)) (update k')
      | Appkont2 (v1,k') -> reduce (App (v1,v)) (update k')
      | Done -> plug_outerkont (Term v) (Marshallout (typ,outerk')))
    | _ -> raise (Failure "Can't plug outer")

  (* handle outer kontinuations @SC *)
  and plug_outerkont (c : control) (k : ffikont) : alpha = match k with
    | Marshallin (lty,k) -> (match c with Term _ -> (raise (Failure "Expected word")) 
      | Word word -> 
        let v = (marshallin word lty) in 
          (plug_outerkont (Term v) k))
    | Marshallout (typ,k') -> (match c with Word _ -> (raise (Failure "Expected value")) 
      | Term v -> (kontinuation := k';  (ret_trace (marshallout v typ))))
    | Executing _ -> (match c with Term t -> (reduce t k) 
        | _ -> raise (Failure "not a term"))
    | Waiting (k',(TApp (l,r)),outerk') -> plug_outerkont c (Executing (k',r,outerk'))
    | Empty -> raise (Failure "Can't execute nothing")
    | _ -> raise (Failure "Type mismatch")

  (* reduce the terms @SC *)
  and reduce (t : term) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    (let update k' = Executing (k',typ,outerk') in
    (match t with

      (* values *)
      | v when (isvalue t) -> (plug_kont v outerk)

      (* if term *)
      | If ((Bool true),b,c) -> (reduce b outerk)
      | If ((Bool false),b,c) -> (reduce c outerk)
      | If (a,b,c) -> (reduce a (update (Ifkont(b,c,k))))

      (* let term *)
      | Let (nv,a,b) when (isvalue a) -> (reduce (subst nv a b) outerk)
      | Let (nv,a,b) -> reduce a (update (Letkont(nv,b,k)))
      | Letrec (nvar,ty,a,b) -> reduce (Let (nvar,Fix (Lam (nvar,ty,a)) ,b)) outerk

      (* Application *)
      | App ((Lam(nvar,ty,a)),b) when (isvalue b) -> (reduce (subst nvar b a) outerk)
      | App (Foreign(ptr,ty),b) when (isvalue b) -> (match ty with
        | TApp (lt,rt) -> kontinuation := outerk;
          call_trace ptr (marshallout b lt) 
        | _ -> raise (Failure "Internal Type inconsistency"))
      | App (a,b) when (isvalue a) -> reduce b (update (Appkont2(a,k)))
      | App (a,b) -> reduce a (update (Appkont(b,k)))

      (* Fix operator *) 
      | Fix (Lam (nv,ty,a)) -> reduce (subst nv (Fix (Lam (nv,ty,a))) a) outerk
      | Fix a -> reduce a (update (Fixkont k))

      (* sequence *)
      | Sequence (Unit,b) -> (reduce b outerk)
      | Sequence (a,b) -> reduce a (update (Sequencekont (b,k)))

      (* set location *)
      | Set (Location (a,_),b) when (isvalue b) -> a := b; (plug_kont Unit outerk)
      | Set (Location (a,i),b) -> reduce b (update (Setkont(Location (a,i),k)))
      | Set (a,b) -> reduce a (update (Setkont(b,k)))

      (* dereference *)
      | Deref (Location (a,_)) -> (plug_kont !a outerk)
      | Deref a -> reduce a (update (Derefkont k))

      (* allocation *)
      | Alloc a  when (isvalue a) -> lcount := !lcount + 1; (plug_kont (Location ((ref a),!lcount)) outerk)
      | Alloc a -> (reduce a (update (Allockont k)))

      (* Hash*)
      | Hash (Location (a,i)) -> plug_kont (Int i) outerk
      | Hash a -> reduce a (update (Hashkont k))

      (* operands *)
      | Oper (op,Int a,Int b) -> plug_kont (match op with 
        | PLUS -> Int (a+b)
        | MINUS -> Int (a-b)
        | TIMES -> Int (a*b)
        | EQUALS -> Bool (a = b)
        | LESSTHEN -> Bool (a < b)
        | LARGERTHEN -> Bool (a > b) ) outerk 
      | Oper (op,a,b) when isvalue a -> reduce b (update (Operkont2 (op,a,k)))
      | Oper (op,a,b) -> reduce a (update (Operkont (op,b,k)))
      | _ -> raise (Failure "Implementation mistake")))
    | _ -> raise (Failure "Incorrect Kontinuation")


  (*===============================================
    # start entry point 
   ===============================================*)
  let start ptr =  
    (* example program *)
    let control = Lam("x",TInt,Sequence((Set (Alloc (Var "x"),Int 400)),(Alloc (Var "x")))) in
    let typ =  TApp(TInt,(TLoc TInt)) in 
    let kont = Executing (Done,typ,Empty) in (* initial set up , execute with no small kontinuation, marshall out at the end*)
    (add_ptr ptr); 
    plug_outerkont (Term control) kont

  
  (*===============================================
    # return back entry point 
   ===============================================*)
  let returnback w = 
    match !kontinuation with
    | Waiting(_,ty,_) ->
      (match ty with 
        | TApp (lty,r) -> plug_outerkont (Word w) (Marshallin (lty,!kontinuation))
        | _ -> raise (Failure "Internal type inconsistency"))
    | _ -> raise (Failure "State machine not waiting on input")


  (*===============================================
    # apply entry point 
   ===============================================*)
  let apply wn w ptr = (add_ptr ptr);
    let (f,ty) = find_name FUNCTION (-wn) in
    match ty with
      | TApp (l,r) -> plug_outerkont (Word w) (Marshallin (l,Executing (Appkont(f,Done),r,!kontinuation)))
      | _ -> raise (Failure "Internal type inconsistency")


  (*===============================================
    # alloc entry point 
   ===============================================*)
  let alloc w wt ptr = (add_ptr ptr);
    let rec conv x y = match (x mod 10) with
      | 1 -> (TBool,y)
      | 2 -> (TInt,y)
      | 3 -> (TUnit,y)
      | 4 -> let (rt,depth) = (conv (x/10) (y+1)) in
        let divisor = int_of_float ((float 10) **  (float (1 +(depth -y)))) in
        let (lt,fdepth) = (conv (x / divisor) (depth+1)) in
        (TApp(lt,rt), fdepth)
      | 5 -> let (tt,depth) = (conv (x/10) (y+1)) in
        ((TLoc tt),depth)
      | _ -> raise (Failure "Could not decontroline type")
    in
    let (ty,_) = (conv wt 1) in
    plug_outerkont (Word w) (Marshallin (ty,Executing ((Allockont Done), (TLoc ty), !kontinuation)))


  (*===============================================
    # set entry point 
   ===============================================*)
  let set wn w ptr = (add_ptr ptr);
    let (loc,ty) = find_name LOCATION (-wn) in  
    match ty with
      | TLoc tt -> plug_outerkont (Word w) (Marshallin (tt,Executing (Setkont2(loc,Done),TUnit,!kontinuation))) 
      | _ -> raise (Failure "Internal type inconsistency")


  (*===============================================
    # deref entry point 
   ===============================================*)
  let deref wn ptr = (add_ptr ptr);
    let (loc,ty) = find_name LOCATION (-wn) in  
    match ty with
      | TLoc tt -> plug_outerkont (Term (Deref loc)) (Executing (Done,tt,!kontinuation))
      | _ -> raise (Failure "Internal type inconsistency")

end


(*--------------------------------------------*)
(*                Auxiliary                   *)       
(*--------------------------------------------*)

(* print a trace *)
let print_trace tr = 
  let intlist_to_string ls = (String.concat ","
    (List.map (fun x -> string_of_int x) ls)) in
  let gamma_to_string = function
    |  Call (ptr,ls) -> (Printf.sprintf ("called 0x%x (%s)") ptr (intlist_to_string ls))
    |  Ret (ptr,w) -> Printf.sprintf "returned %d @ 0x%x"  w ptr in
  let alpha_to_string = function
    | Done -> "Done"
    | Secure g -> "Secure "^(gamma_to_string g)
    | Attacker g -> "Attacker "^(gamma_to_string g)
  in
  Printf.printf "[*] %s\n" (alpha_to_string tr)

(* attcker actionds *)
let attacker_act (tr : alpha) : alpha =  print_trace tr;
  match tr with
  | Done -> exit 0
  | Attacker gamma -> (match gamma with
    | Call (p,ls) -> (match p with
      | 0 -> (SecureML.start (List.hd ls))
      | 1 -> (match ls with
        | wn::w::ptr::[] -> (SecureML.apply  wn w ptr)
        | _ -> raise (Failure "Attacker"))
      | 2 -> (match ls with
        | w::wt::ptr::[] -> (SecureML.alloc w wt ptr)
        | _ -> raise (Failure "Attacker"))
      | 3 -> (match ls with
        | wn::w::ptr::[] -> (SecureML.set wn w ptr)
        | _ -> raise (Failure "Attacker"))
      | 4 -> (match ls with
        | wn::ptr::[] -> (SecureML.deref wn  ptr)
        | _ -> raise (Failure "Attacker"))
      | 5 -> (match ls with
        | w::[] -> (SecureML.returnback w)
        | _ -> raise (Failure "Attacker"))
      | _ -> raise (Failure "Attacker"))
    | Ret (_,w) -> (SecureML.returnback w))
  | _ -> raise (Failure "Attacker")

(* for a basic back and forth *)
let input tr = let r = attacker_act tr in
  print_trace r;
  match r with 
  | Secure Ret (i,j) -> (i,j) 
  | Secure Call(ptr,ls) -> (ptr, (List.hd ls))
  | _ -> raise (Failure "Input failure")
  

(*--------------------------------------------*)
(*            Entry Point List                *)       
(*--------------------------------------------*)
(*
  Adress :: Purpose
     0   :: start entry point
     1   :: Application entry point
     2   :: Allocation entry point
     3   :: Location set entry point
     4   :: Location Dereference entry point
     5   :: Return back entry point
*)


(*--------------------------------------------*)
(*                Example                     *)       
(*--------------------------------------------*)

let main () =
  (* An example of interactions between the attacker and \ML through the FFI *)
  let (_,i) = input (Attacker (Call (0,[0x2]))) in
  let (_,j) = input (Attacker (Call (1,[i;5;0x12]))) in
  let (_,_) = input (Attacker (Call (4,[j;0x18]))) in
  let (_,_) = input (Attacker (Call (3,[j;8;0x22]))) in
  let (_,_) = input (Attacker (Call (4,[j;0x26]))) in
  let (_,j) = input (Attacker (Call (2,[j;25;0x29]))) in
  let (_,j) = input (Attacker (Call (4,[j;0x34]))) in
  let (_,_) = input (Attacker (Call (4,[j;0x38]))) in
  let (_,_) = input (Attacker (Call (2,[i;2254;0x29]))) in
  input Done

let _ = main()

