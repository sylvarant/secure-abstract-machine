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

  (* secure language definition @SC *)
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
    | Closure of variable * ty * term * environment

  (* environment *)
  and environment = string -> term

  (* kontinuations @SC *)
  type mlkont =
    | Done
    | Ifkont of term * term * mlkont * environment
    | Appkont of term * environment * mlkont 
    | Appkont2 of term * mlkont
    | Allockont of mlkont * environment
    | Hashkont of mlkont * environment
    | Operkont of operands * term * mlkont * environment
    | Operkont2 of operands * term * mlkont  * environment
    | Derefkont of mlkont * environment
    | Setkont of term * mlkont * environment
    | Setkont2 of term * mlkont * environment
    | Letkont of variable * term * mlkont * environment
    | Sequencekont of term * mlkont * environment
    | Fixkont of mlkont * environment

  (* FFI kontinuations *)
  type ffikont =
    | Empty
    | Waiting of mlkont * environment * ty * ffikont
    | Executing of mlkont * ty * ffikont
    | Marshallin of ty * ffikont
    | Marshallout of ty * ffikont

  (* value identification *)
  let rec isvalue = function
    | Bool _ | Int _ | Unit | Location _ | Closure _ | Foreign _ -> true
    | _ -> false

  (* the control @SC *)
  type control =
    | Term of term
    | Word of word


  (*--------------------------------------------*)
  (*                   State                    *)       
  (*--------------------------------------------*)

  (* global lists *)
  let ptr_stack = ref []
  let name_map : (name * term * ty) list ref = ref []
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

  (* helper @SC *)
  let error () = raise (Failure "Typing error")
  
  (* marshall out to words *) 
  let marshallout value typ : word =  (* @SC *)
    let wordify = function
      | (_,i) -> (-i) in (* negate to differentiate *)
    let lowfunc = function
      | TApp _ -> true
      | _ -> false in
    let lowloc = function
      | TLoc _ -> true
      | _ -> false in
    let word = (match value with
      | Bool true when typ = TBool -> 1
      | Bool false when typ = TBool -> 0
      | Int i when typ = TInt -> i
      | Closure _ as z when (lowfunc typ) -> wordify (add_function z typ) 
      | Unit when typ = TUnit -> 0
      | Location _  when (lowloc typ) -> wordify (add_location value typ)
      | Foreign (ptr,ty) when ty = typ -> ptr
      | _ -> raise (Failure "Marshalling out Failed")) 
    in word
      
  (* marshall in words *) (* @SC *)
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

  (* handle ML kontinuations: @SC *)
  let rec plug_kont (t : term) (e : environment) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    let update k' = Executing (k',typ,outerk') in
      (match k with
        | Ifkont (c2,c3,k',e) -> (match t with 
          | Bool true   -> reduce c2 e (update k')
          | Bool false  -> reduce c3 e (update k')
          | _ -> error() ) 
        | Allockont (k',e) -> lcount := !lcount + 1; 
          (plug_kont (Location ((ref t),!lcount)) e (update k'))
        | Hashkont (k',e) -> (match t with
          | Hash (Location (a,i)) -> plug_kont (Int i) e (update k')
          | _ -> error())
        | Operkont (op,c2,k',e) -> reduce (Oper (op,t,c2)) e (update k')
        | Operkont2 (op,Int a,k',e) -> (match t with
          | Int b -> plug_kont (match op with 
            | PLUS -> Int (a+b)
            | MINUS -> Int (a-b)
            | TIMES -> Int (a*b)
            | EQUALS -> Bool (a = b)
            | LESSTHEN -> Bool (a < b)
            | LARGERTHEN -> Bool (a > b)) e (update k')
          | _ -> error())
        | Derefkont (k',e) -> (match t with 
            |  (Location (a,_)) -> (plug_kont !a e outerk)
            | _ -> error())
        | Setkont (c2,k',e) -> reduce (Set(t,c2)) e (update k')
        | Setkont2 (Location(a,_),k',e) -> a := t; (plug_kont Unit e (update k'))
        | Letkont (nv,c2,k',e) -> reduce c2 (update_env nv t e) (update k')
        | Sequencekont (c2,k',e) -> (match t with
          | Unit -> reduce c2 e (update k')
          | _ -> error())
        | Fixkont (k',e) -> (match t with 
          | (Closure (nv,ty,a,e1)) as cl -> reduce a (update_env nv (Fix cl) e) (update k')
          | _ -> error())
        | Appkont (c2,env,k') -> reduce (App(t,c2)) env (update k')
        | Appkont2 (v1,k') -> (match v1 with
          | Closure(nvar,ty,a,e1) ->  reduce a (update_env nvar t e1) (update k')
          | Foreign(ptr,ty)  -> (match ty with
            | TApp (lt,rt) -> kontinuation := Waiting(k,e,TApp(rt,typ),outerk');
              call_trace ptr (marshallout t lt) 
            | _ -> raise (Failure "Internal Type inconsistency"))
          | _ -> error())
        | Done -> plug_outerkont (Term t) e (Marshallout (typ,outerk'))
        | _ -> raise (Failure "forget something"))
    | _ -> raise (Failure "Can't plug outer")

  (* handle outer kontinuations *) (* @SC *)
  and plug_outerkont (c : control) (e : environment) (k : ffikont) : alpha = 
    match k with
    | Marshallin (lty,outerk) -> (match c with Term _ -> (raise (Failure "Expected word")) 
      | Word word -> 
        let v = (marshallin word lty) in 
          (plug_outerkont (Term v) e outerk))
    | Marshallout (typ,k') -> (match c with Word _ -> (raise (Failure "Expected value")) 
      | Term v -> (kontinuation := k';  (ret_trace (marshallout v typ))))
    | Executing _ -> (match c with Term cl -> (reduce cl e k) 
        | _ -> raise (Failure "not a term"))
    | Waiting (k',e,(TApp (l,r)),outerk') -> plug_outerkont c e (Executing (k',r,outerk'))
    | Empty -> raise (Failure "Can't execute nothing")
    | _ -> raise (Failure "Type mismatch")

  (* reduce the terms *)
  and reduce (t : term) (env : environment) (outerk : ffikont) : alpha = 
    match outerk with Executing (k,typ,outerk') -> 
    let update k' = Executing (k',typ,outerk') in
    (* Evaluation order and high-level transitions @SC *)
    (match t with
      | v when (isvalue t) -> (plug_kont v env outerk)
      | Lam(nv,ty,a) -> (plug_kont (Closure(nv,ty,a,env)) env outerk)
      | Var nv -> reduce (env nv) env outerk
      | If (a,b,c) -> (reduce a env (update (Ifkont(b,c,k,env))))
      | Sequence (a,b) -> reduce a env (update (Sequencekont (b,k,env)))
      | Set (Location (a,i),b) -> reduce b env (update (Setkont2(Location (a,i),k,env)))
      | Set (a,b) -> reduce a env (update (Setkont(b,k,env)))
      | Deref a -> reduce a env (update (Derefkont (k,env)))
      | Alloc a -> (reduce a env (update (Allockont (k,env))))
      | Hash a -> reduce a env (update (Hashkont (k,env)))
      | Oper (op,a,b) when isvalue a -> reduce b env (update (Operkont2 (op,a,k,env)))
      | Oper (op,a,b) -> reduce a env (update (Operkont (op,b,k,env)))
      | App (x,y) when isvalue x -> reduce y env (update (Appkont2(x,k)))  
      | App (x,y) -> reduce x env (update (Appkont(y,env,k)))
      | Fix a -> reduce a env (update (Fixkont (k,env)))
      | Let (nv,a,b) -> reduce a env (update (Letkont(nv,b,k,env)))
      | Letrec (nvar,ty,a,b) -> reduce (Let (nvar,Fix (Lam (nvar,ty,a)),b)) env outerk
      | _ -> raise (Failure "Implementation mistake"))
    | _ -> raise (Failure "Needs to be Executing")


  (*===============================================
    # start entry point 
   ===============================================*)
  let start ptr =  
    (* example program *)
    let control = Lam("x",TApp(TInt,TInt),App(Var "x",(Int 2))) in
    let typ =  TApp(TApp(TInt,TInt),TInt) in 
    let kont = Executing (Done,typ,Empty) in (* initial set up , execute with no small kontinuation, marshall out at the end*)
    (add_ptr ptr); 
    plug_outerkont (Term control) empty_env kont

  
  (*===============================================
    # return back entry point 
   ===============================================*)
  let returnback w = 
    match !kontinuation with
    | Waiting(_,e,ty,_) ->
      (match ty with 
        | TApp (lty,r) -> plug_outerkont (Word w) e (Marshallin (lty,!kontinuation))
        | _ -> raise (Failure "Internal type inconsistency"))
    | _ -> raise (Failure "State machine not waiting on input")


  (*===============================================
    # apply entry point 
   ===============================================*)
  let apply wn w ptr = (add_ptr ptr);
    let (f,ty) = find_name FUNCTION (-wn) in
      (match ty with
        | TApp (l,r) -> plug_outerkont (Word w) empty_env (Marshallin (l,Executing (Appkont2(f,Done),r,!kontinuation)))
        | _ -> raise (Failure "Internal type inconsistency"))


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
    plug_outerkont (Word w) empty_env (Marshallin (ty,Executing ((Allockont (Done,empty_env)), (TLoc ty), !kontinuation)))


  (*===============================================
    # set entry point 
   ===============================================*)
  let set wn w ptr = (add_ptr ptr);
    let (loc,ty) = (find_name LOCATION (-wn)) in
      (match ty with
        | TLoc tt -> plug_outerkont (Word w) empty_env (Marshallin (tt,Executing (Setkont2(loc,Done,empty_env),TUnit,!kontinuation))) 
        | _ -> raise (Failure "Internal type inconsistency"))

  (*===============================================
    # deref entry point 
   ===============================================*)
  let deref wn ptr = (add_ptr ptr);
    let (loc,ty) = (find_name LOCATION (-wn)) in
      (match ty with
        | TLoc tt -> plug_outerkont (Term (Deref loc)) empty_env (Executing (Done,tt,!kontinuation))
        | _ -> raise (Failure "Internal type inconsistency"))

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

