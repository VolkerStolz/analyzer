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
  
  let startstate () = Dom.empty ()
  let otherstate () = Dom.empty ()
  let exitstate  () = Dom.top ()

  (* transfer functions : Don't propagate anything *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = Dom.empty ()
  let branch ctx (exp:exp) (tv:bool) : Dom.t = Dom.empty ()
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
    (* locking logic for SLPs. Refer to MayLocks for resolving aliases. *)
    match (LibraryFunctions.classify f.vname arglist, f.vname) with
      | `Lock (failing, rw), _
          when not (Dom.is_empty maylocks) -> MayLocks.Spec.lock rw failing ctx.ask lval arglist ctx.local
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
