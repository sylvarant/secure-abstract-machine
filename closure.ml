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
    | Location of closure ref * int (* @SC *)
    | Set of term * term
    | Deref of term
    | Alloc of term
    | Foreign of pointer * ty
    | Hash of term

  (* environment @SC *)
  and environment = string -> closure

  (* closure @SC *)
  and closure =  
    | Closure of term * environment
    | Clapp of closure * closure
    | CLet of variable * closure  * closure
    | CFix of closure
    | CSet of closure * closure
    | CDeref of closure
    | CAlloc of closure
    | CIf of closure * closure * closure
    | COper of operands * closure * closure
    | CHash of closure
    | CSequence of closure * closure

  (* kontinuations *)
  type mlkont =
    | Done
    | Ifkont of closure * closure * mlkont  
    | Appkont of closure * mlkont 
    | Appkont2 of closure * mlkont
    | Allockont of mlkont 
    | Hashkont of mlkont 
    | Operkont of operands * closure * mlkont 
    | Operkont2 of operands * closure * mlkont  
    | Derefkont of mlkont 
    | Setkont of closure * mlkont 
    | Setkont2 of closure * mlkont 
    | Letkont of variable * closure * mlkont 
    | Sequencekont of closure * mlkont 
    | Fixkont of mlkont 

  (* FFI kontinuations *)
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

  (* the control *)
  type control =
    | Term of closure (* @SC *)
    | Word of word



  (*--------------------------------------------*)
  (*                   State                    *)       
  (*--------------------------------------------*)

  (* @SC deal with non closure locations *)
  type mapcontent = Cl of closure | Loc of term

  (* global lists *)
  let ptr_stack = ref []
  let name_map : (name * mapcontent * ty) list ref = ref []
  let kontinuation = ref Empty 

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

  (* add function to map @SC *)
  let add_function (lam : closure) (tt : ty) : name = 
    fcount := !fcount + 1;
    let n = (FUNCTION,!fcount) in
    (name_map := ((n,Cl lam,tt) :: !name_map)); n
    
  (* add_location *)
  let add_location (loc : term) (tt : ty) : name =
    let n = (match loc with
    | Location (x,i) -> (LOCATION,i) 
    | _ -> raise (Failure "Wrong usage of location map")) in
    name_map := (n,Loc loc,tt) :: !name_map; n

  (* find name in map *)
  let find_name nty i =
    let predicate = function (n,_,_) -> match n with
      | (nn,num) when (nn = nty && num = i) -> true
      | _ -> false
    in
    try let (_,control,typ) = (List.find predicate !name_map) in (control,typ)
    with _ -> raise (Failure "Name not found")

  let empty_env = (fun x -> raise (Failure "Variable not found"))

  let update_env name value e = (fun y -> if name = y then value else (e y))

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

  (* helper *)
  let closure_isvalue = function
    | Closure (x,_) -> isvalue x
    | _ -> false

  (* marshall out to words *) 
  let marshallout closure typ : word =  (* @SC *)
    let wordify = function
      | (_,i) -> (-i) in (* negate to differentiate *)
    let lowfunc = function
      | TApp _ -> true
      | _ -> false in
    let lowloc = function
      | TLoc _ -> true
      | _ -> false in
    match closure with
    | Closure (value,env) ->
      let word = (match value with
        | Bool true when typ = TBool -> 1
        | Bool false when typ = TBool -> 0
        | Int i when typ = TInt -> i
        | Lam _ as z when (lowfunc typ) -> wordify (add_function (Closure(z,env)) typ) 
        | Unit when typ = TUnit -> 0
        | Location _  when (lowloc typ) -> wordify (add_location value typ)
        | Foreign (ptr,ty) when ty = typ -> ptr
        | _ -> raise (Failure "Marshalling out Failed")) 
      in word
    | _ -> raise (Failure "Closure application can't be marshalled")
      
  (* marshall in words *) (* @SC *)
  let marshallin word typ : closure = 
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
      | 0 -> Closure((Bool false),empty_env)
      | _ -> Closure((Bool true),empty_env))
    | TInt -> Closure((Int word),empty_env)
    | TUnit -> Closure(Unit,empty_env)
    | TLoc a -> (if word < 0 
      then 
        (match (find_name LOCATION (-word)) with 
          | (Loc loc,ty) -> (check ty typ); Closure(loc,empty_env)
          | _ -> raise (Failure "Found lambda where location was expected"))
      else raise (Failure "Foreign locaitons not supported"))
    | TApp (a,b) -> (if word < 0
      then 
        (match (find_name FUNCTION (-word)) with 
          | (Cl lam,ty) -> (check ty typ); lam
          | _ -> (raise (Failure "Found location where lambda expected")))
      else Closure(Foreign (word,typ),empty_env))

  (* handle ML kontinuations *)
  let rec plug_kont (cl : closure) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    let update k' = Executing (k',typ,outerk') in
    (match cl with Clapp _ -> raise (Failure "Double Closure") 
      | v -> (match k with
        | Ifkont (c2,c3,k') -> reduce (CIf (v,c2,c3)) (update k')
        | Allockont (k') -> reduce (CAlloc v) (update k')
        | Hashkont (k') -> reduce (CHash v) (update k')
        | Operkont (op,c2,k') -> reduce (COper (op,v,c2)) (update k')
        | Operkont2 (op,v1,k') -> reduce (COper (op,v1,v)) (update k')
        | Derefkont (k') -> reduce (CDeref v) (update k')
        | Setkont (c2,k') -> reduce (CSet(v,c2)) (update k')
        | Setkont2 (l,k') -> reduce (CSet(l,v)) (update k')
        | Letkont (nv,c2,k') -> reduce (CLet(nv,v,c2)) (update k')
        | Sequencekont (c2,k') -> reduce (CSequence(v,c2)) (update k')
        | Fixkont (k') -> reduce (CFix v) (update k')
        | Appkont (c2,k') -> reduce (Clapp (cl,c2)) (update k')
        | Appkont2 (cl1,k') -> reduce (Clapp (cl1,cl)) (update k')
        | Done -> plug_outerkont (Term cl) (Marshallout (typ,outerk'))))
    | _ -> raise (Failure "Can't plug outer")

  (* handle outer kontinuations *) (* @SC *)
  and plug_outerkont (c : control) (k : ffikont) : alpha = 
    match k with
    | Marshallin (lty,k) -> (match c with Term _ -> (raise (Failure "Expected word")) 
      | Word word -> 
        let v = (marshallin word lty) in 
          (plug_outerkont (Term v) k))
    | Marshallout (typ,k') -> (match c with Word _ -> (raise (Failure "Expected value")) 
      | Term v -> (kontinuation := k';  (ret_trace (marshallout v typ))))
    | Executing _ -> (match c with Term cl -> (reduce cl k) 
        | _ -> raise (Failure "not a term"))
    | Waiting (k',(TApp (l,r)),outerk') -> plug_outerkont c (Executing (k',r,outerk'))
    | Empty -> raise (Failure "Can't execute nothing")
    | _ -> raise (Failure "Type mismatch")

  (* reduce the terms *)
  and reduce (cl : closure) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    let update k' = Executing (k',typ,outerk') in
    (* handle closure @SC *)
    (match cl with 
      | Clapp (Closure((Lam(nvar,ty,a)),e1),x) when (closure_isvalue x) -> reduce (Closure(a,(update_env nvar x e1))) outerk
      | Clapp (Closure(Foreign(ptr,ty),e1),x) when (closure_isvalue x) -> 
        (match ty with
          | TApp (lt,rt) -> kontinuation := Waiting(k,TApp(rt,typ),outerk');
            call_trace ptr (marshallout x lt) 
          | _ -> raise (Failure "Internal Type inconsistency"))
      | Clapp (x,y) when (closure_isvalue x) -> reduce y (update (Appkont2(x,k)))  
      | Clapp (x,y) -> reduce x (update (Appkont(y,k)))
      | CIf (Closure((Bool true),_),b,c) -> (reduce b outerk)
      | CIf (Closure((Bool false),_),b,c) -> (reduce c outerk)
      | CIf (a,b,c) -> (reduce a (update (Ifkont(b,c,k))))
      | CLet (nv,a,(Closure(b,env))) when (closure_isvalue a) -> reduce (Closure(b,(update_env nv a env))) outerk
      | CLet (nv,a,b) -> reduce a (update (Letkont(nv,b,k)))
      | CFix (Closure((Lam (nv,ty,a)),env) as x) -> reduce (Closure(a,(update_env nv (CFix x) env)))  outerk
      | CFix a -> reduce a (update (Fixkont k))
      | CSequence (Closure (Unit,_),b) -> (reduce b outerk)
      | CSequence (a,b) -> reduce a (update (Sequencekont (b,k)))
      | CSet (Closure(Location (a,_),_),b) when (closure_isvalue b) -> a := b; (plug_kont (Closure(Unit,empty_env)) outerk)
      | CSet (Closure(Location (a,i),_) as x,b) -> reduce b (update (Setkont2(x,k)))
      | CSet (a,b) -> reduce a (update (Setkont(b,k)))
      | CDeref Closure((Location (a,_)),_) -> (plug_kont !a outerk)
      | CDeref a -> reduce a (update (Derefkont k))
      | CAlloc a  when (closure_isvalue a) -> lcount := !lcount + 1; (plug_kont (Closure(Location ((ref a),!lcount),empty_env)) outerk)
      | CAlloc a -> (reduce a (update (Allockont k)))
      | CHash Closure((Location (a,i)),_) -> plug_kont (Closure((Int i),empty_env)) outerk
      | CHash a -> reduce a (update (Hashkont k))
      | COper (op,Closure(Int a,_),Closure(Int b,_)) -> plug_kont (Closure((match op with 
        | PLUS -> Int (a+b)
        | MINUS -> Int (a-b)
        | TIMES -> Int (a*b)
        | EQUALS -> Bool (a = b)
        | LESSTHEN -> Bool (a < b)
        | LARGERTHEN -> Bool (a > b) ),empty_env)) outerk 
      | COper (op,a,b) when closure_isvalue a -> reduce b (update (Operkont2 (op,a,k)))
      | COper (op,a,b) -> reduce a (update (Operkont (op,b,k)))

      (* basic closures *)
      | Closure (t,env) ->
      (match t with

        (* @SC *)
        | Var nv -> reduce (env nv) outerk

        (* values *)
        | v when (isvalue t) -> (plug_kont (Closure(v,env)) outerk)

        (* propagators *)
        | App (t1,t2) -> reduce (Clapp(Closure (t1,env),Closure (t2,env))) outerk
        | If (a,b,c) -> reduce (CIf(Closure(a,env),Closure(b,env),Closure(c,env))) outerk
        | Let (nv,a,b) -> reduce (CLet(nv,Closure (a,env),Closure (b,env))) outerk
        | Fix a -> reduce (CFix (Closure(a,env))) outerk
        | Sequence (a,b) -> reduce (CSequence(Closure(a,env),Closure(b,env))) outerk
        | Set (a,b) -> reduce (CSet(Closure(a,env),Closure(b,env))) outerk
        | Deref a -> reduce (CDeref(Closure(a,env))) outerk
        | Alloc a -> reduce (CAlloc(Closure(a,env))) outerk
        | Hash a -> reduce (CHash(Closure(a,env))) outerk
        | Oper (op,a,b) -> reduce (COper(op,Closure(a,env),Closure(b,env))) outerk


        (* syntactic sugar *)
        | Letrec (nvar,ty,a,b) -> reduce (Closure((Let (nvar,Fix (Lam (nvar,ty,a)),b)),env)) outerk
        | _ -> raise (Failure "Implementation mistake")))


        (* allocation *)


        (* Hash*)


        (* operands *)

    | _ -> raise (Failure "Incorrect Kontinuation")


  (*===============================================
    # start entry point 
   ===============================================*)
  let start ptr =  
    (* example program *)
    let control = Lam("x",TApp(TInt,TInt),App(Var "x",(Int 2))) in
    let typ =  TApp(TApp(TInt,TInt),TInt) in 
    let kont = Executing (Done,typ,Empty) in (* initial set up , execute with no small kontinuation, marshall out at the end*)
    (add_ptr ptr); 
    plug_outerkont (Term (Closure(control,empty_env))) kont

  
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
    match find_name FUNCTION (-wn) with
      | (Cl f,ty) -> (match ty with
        | TApp (l,r) -> plug_outerkont (Word w) (Marshallin (l,Executing (Appkont2(f,Done),r,!kontinuation)))
        | _ -> raise (Failure "Internal type inconsistency"))
      | _ -> raise (Failure "Found location where closure expected") 


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
    match (find_name LOCATION (-wn)) with
      | (Loc loc,ty) -> (match ty with
        | TLoc tt -> plug_outerkont (Word w) (Marshallin (tt,Executing (Setkont2((Closure(loc,empty_env)),Done),TUnit,!kontinuation))) 
        | _ -> raise (Failure "Internal type inconsistency"))
      | _ -> raise (Failure "Found lambda instead of location")


  (*===============================================
    # deref entry point 
   ===============================================*)
  let deref wn ptr = (add_ptr ptr);
    match (find_name LOCATION (-wn)) with
      | (Loc loc,ty) -> (match ty with
        | TLoc tt -> plug_outerkont (Term (Closure((Deref loc),empty_env))) (Executing (Done,tt,!kontinuation))
        | _ -> raise (Failure "Internal type inconsistency"))
      | _ -> raise (Failure "Found lambda instead of location")

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
  let (_,_) = input (Attacker (Call (1,[i;0x567;0x12]))) in
  let (_,r) = input (Attacker (Call (5,[8]))) in (* This example uses a call back *)
  assert(r = 8);
  input Done

let _ = main()

