(**************************************************************************)
(*                         Starting Point                                 *)
(*  By Adriaan Larmuseau                                                  *)
(**************************************************************************)

(* Attacker Language *)
type variable = string

type atcalc = Bool of bool
  | Wrong
  | Var of variable
  | Name of int
  | Lam of variable * atcalc
  | App of atcalc * atcalc
  | If of atcalc * atcalc * atcalc
  | Let of variable * atcalc * atcalc
  | Call of atcalc * atcalc 
  | Pair of atcalc * atcalc 
  | Fix of atcalc

type output_label = Done
  | Output of atcalc

type input_label = Input of atcalc

(* Exceptions *)
exception Mistake of string

(**************************************************************************)
(*                                 LTS                                    *)
(**************************************************************************)

module type Secure = sig 
  type seclang
  val input : input_label -> output_label
  val output :  output_label
end

(* Implementation *)
module Seclang : Secure = 
struct

  (* sec types *)
  type stype = TBool
    | TApp of stype * stype
    | TPair of stype * stype

  (* secure language definition *)
  type sterm = SBool of bool
    | SVar of variable
    | SLam of variable * stype * sterm
    | SApp of sterm * sterm
    | SI of stype * atcalc
    | SIf of sterm * sterm * sterm
    | SLet of variable * sterm * sterm
    | SPair of sterm * sterm
    | SFst of sterm
    | SSnd of sterm
    | SFix of sterm
    | SWrong
  
  (* substitute *)
  let rec subst var value target = match target with
    | SBool _ -> target
    | SVar str when str = var -> value
    | SLam (nvar,a) when nvar = var -> target
    | SLam (nvar,ty,a) -> SLam (nvar,ty,(subst var value a))
    | SApp (a,b) -> SApp ((subst var value a),(subst var value b))
    | SI _ -> target
    | SIf (a,b,c) -> SIf ((subst var value a),(subst var value b),(subst var value c))
    | SLet (nvar,a,b) when nvar = var -> target
    | SLet (nvar,a,b) -> SLet (nvar,(subst var value a),(subst var value b))
    | SPair (a,b) -> SPair((subst var value a),(subst var value b))
    | SFix a -> SFix (subst var value a)
    | SFst a -> SFst (subst var value a)
    | SSnd a -> SSnd (subst var value a)

  (* the secure program *)
  let map = ref []
  
  (* output *)
  let output = 
    let src = (SLam("x",SBool true)) 
    and ty = (TApp (TBool,TBool)) in
    (lts src ty)

  let input = match label with
    | App ((Name n),v) -> let (va,TApp(left,right)) = (List.nth map n) in
      let arg = (marshall_in v left) in
      (lts (SApp(va,arg)) right)
    |  

  (* evaluate *)
  let rec lts prog ty = 

    (* reduce *)
    let rec reduce t = match t with
      | SBool _ -> t  
      | SLam _ -> t
      | SWrong _ -> t
      | SVar str -> raise (Mistake ("Variable evaluation "^str))
      | SIf ((SBool true),b,c) -> (reduce b)
      | SIf ((SBool false),b,c) -> (reduce c)
      | SIf (a,b,c) -> reduce (SIf ((reduce a),b,c))
      | SPair (a,b) when (isvalue t) -> t
      | SPair (a,b) when (isvalue a) -> reduce (SPair(a,(reduce b)))
      | SPair (a,b) -> reduce (SPair ((reduce a),b))
      | SLet (nv,a,b) when (isvalue a) -> reduce (subst nv a b)
      | SLet (nv,a,b) -> reduce (SLet(nv,(reduce a),b))
      | SApp ((SLam(nvar,a)),b) when (isvalue b) -> reduce (subst nvar b a)
      | SApp ((SI (TApp(l,r) ,a)) as out,b) when (isvalue b) -> (reduce (marshall_in (callback out (marshall_out b l)) (TAPP(r,ty))))
      | SApp (a,b) when (isvalue a) -> reduce (SApp (a, (reduce b)))
      | SApp (a,b) -> reduce (SApp ((reduce a),b))
      | SFst (SPair(a,b)) -> a
      | SFst a -> reduce (SFst (reduce a))
      | SSnd (SPair(a,b)) -> b
      | SSnd a -> reduce (SSnd (reduce a))
      | SFix (SLam (nv,ty,a)) -> reduce (subst nv (SFix (SLam (nv,ty,a))) a)
      | SFix a -> reduce (SFix (reduce a))
      | SI _ -> t
      | _ -> raise (Mistake ("Failed to pattern match"))

    (* marshall out *)
    and rec marshall_out t tt = match (t,tt) with
      | (SI (typ,Lam(nv,a)),nty) when typ = nty -> Lam (nv,a)
      | (SLam _,typ) -> nmap := ((l,typ) :: !nmap); Name ((List.length nmap) - 1)
      | (SPair (a,b),TPair(ty1,ty2)) -> nmap := (t,ty) :: !nmap; Name ((List.length nmap) - 1)
      | (SBool b,TBool) -> Bool b
      | _ -> Wrong

    (* marshall in *)
    and rec marshall_in l tt = match (t,tt) with
      | (Bool b,TBool) -> SBool b
      | (Lam(nv,a),ty) -> SI (ty,t)
      | (Pair(a,b),TPair(ll,rr)) -> SPair((marshall_in a ll),(marshall_in b rr))
      | Name n -> let (va,typ) = (List.nth map n) in
        (if typ = tt then va else SWrong)
      | _ -> SWrong
    in

    (* toplevel *)
    (marshall (reduce prog) ty)
              
end


(**************************************************************************)
(*                          Attacker λ                                    *)
(**************************************************************************)

(* derefernce *)
let get_opt m = function
    | Some x -> x
    | None   -> raise (Invalid_argument m)

module Regular (Sec : Secure) = struct

    (* env0 *)
    let empty_env = (fun x -> None)

    (* update the environment *)
    let rec update env x y = (fun xx -> match x with 
        | Var nn -> ( match xx with 
            | Var tx  when (tx = nn)  -> Some y   
            | _ -> (env xx)) 
        | _ ->  raise (Invalid_argument "Not a variable")
    ) 

    
    (* eval *)
    let rec atteval env nenv =  function
        | IS t -> (Sec.seceval nenv env atteval t)
        | Var x as d -> (get_opt "Variable not found in Environment" (env d))
        | Bool b as expr -> expr
        | App(x,y) -> (let (Closure f) = (atteval env nenv x) and w = (atteval env nenv y) in (f w)) 
        | Lam(x,t) -> Closure (fun w -> (atteval (update env x w) nenv t))
        | Closure f as clo -> clo

    (* tostring *)
    let rec tostring = function
        | IS t -> ("(IS <secret>)")
        | Var x -> x
        | Closure f -> ("(clo <function>)")
        | Bool b -> (if b then "true" else "false")
        | App(x,y) -> ("(Appl "^(tostring x)^","^(tostring y)^")")
        | Lam(x,y) -> ("(Lambda "^(tostring x)^","^(tostring y)^")")
    
end

(**************************************************************************)
(*                              Main                                      *)
(**************************************************************************)

(* programs *)
let factprog = 
   (* Lam (Var "x",Var "x") *)
   (*App ((Lam (Var "x",Var "x")),App(Lam(Var "x",Var "x"),(Bool true))) *)
   (IS (Seclang.secure_identity_app (Lam(Var "y",Var "y"))))

(* create an attacker *)
module Attacker_1  = Regular(Seclang)

(* main function *)
let main t = 
    let res = (Attacker_1.atteval Attacker_1.empty_env Seclang.empty_env t) in
    print_string ("Executing :: "^(Attacker_1.tostring t)^"\n"); 
    print_string ((Attacker_1.tostring res)^"\n");;

(main factprog);;
    


