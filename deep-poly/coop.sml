(* Cooperative concurrency library using effects. *)

signature COOP =
sig
  val run  : (unit -> int) -> int
  val fork : (unit -> int) -> unit
  val yield : unit -> unit
end

structure Coop :> COOP =
struct
  exception Fork of (unit -> int)
  exception Yield

  open Eff

  fun fork f = perform (Fork f)
  fun yield () = perform (Yield)

  fun run main =
    let
      val threads = ref []
      fun enqueue t = threads := !threads @ [t]
      fun dequeue () =
        case !threads of
          [] => 0
        | k :: ks =>
            (threads := ks;
             print ("run.dequeue: thread finished with "^(Int.toString (continue k ()))^"\n");
             dequeue ())
      fun scheduler eff cont =
        case eff of
          Yield =>
            (enqueue cont;
             dequeue ())
        | Fork f =>
            (enqueue cont;
             print ("run.Fork: thread finished with "^(Int.toString (handleFun (handler ()) f ()))^"\n");
             dequeue ())
        | _ => raise Fail "scheduler: unhandled effect"
      and handler () = Handler {catch = scheduler, finally = fn x => x}
    in
      handleFun (handler ()) main ()
    end
end
