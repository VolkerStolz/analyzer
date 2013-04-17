open Cil
open Pretty
open Analyses
open GobConfig
open Batteries

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Second lock point-analysis"
  module Dom  = LockDomain.SLPset
  module Glob = Glob.Make (Lattice.Unit)
  
  (* BEGIN directly hoisted from MayLocks *)
   
  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)
  
  (* Query the value (of the locking argument) to a list of locks. *)
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a) ->
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
      | b -> Messages.warn ("Could not evaluate '"^sprint 30 (d_exp () exp)^"' to an points-to set, instead got '"^Queries.Result.short 60 b^"'."); []
 
  let lock rw may_fail a lv arglist ls : (Dom.ReverseAddrSet.t * Cil.exp * bool) list =
    let set_ret tv sts = 
      match lv with 
        | None -> (sts,Cil.integer 1,true)
        | Some lv -> (sts,Lval lv,tv)
    in 
    let lock_one xs (e:LockDomain.Addr.t) =
        (set_ret false  (Dom.add (e,rw) ls)) ::
        if may_fail then set_ret true ls :: xs else xs
    in
    List.fold_left lock_one [] (List.concat (List.map (eval_exp_addr a) arglist)) 
  
  let startstate () = Dom.empty ()
  let otherstate () = Dom.empty ()
  let exitstate  () = Dom.top ()

  (* END *)

  (* transfer functions : Don't propagate anything *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = Dom.empty ()
  let branch ctx (exp:exp) (tv:bool) : Dom.t = ctx.local  (* BUG ?! *)
  let body ctx (f:fundec) : Dom.t = Dom.empty ()
  let return ctx (exp:exp option) (f:fundec) : Dom.t = Dom.empty ()
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = [Dom.empty (), Dom.empty ()]
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = Dom.empty ()

  (* transfer function to handle library functions --- for us locking & unlocking *)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    (* Acquire MayLockset from existing analysis *)
    let maylocks : Dom.t = 
      match ctx.presub with
        | [`MayLocks l] -> l
        | _ -> failwith "SLP unavailable --- dependencies missing!"
    in
    (* locking logic for SLPs. *)
    match (LibraryFunctions.classify f.vname arglist, f.vname) with
      | `Lock (failing, rw), _
          when not (Dom.is_empty maylocks) -> lock rw failing ctx.ask lval arglist ctx.local
      | _ -> [Dom.empty (), Cil.integer 1, true]

end

module SLPMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "slps" 
                let depends = ["maylocks"]
                type lf = Spec.Dom.t
                let inject_l x = `SLP x
                let extract_l x = match x with `SLP x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
