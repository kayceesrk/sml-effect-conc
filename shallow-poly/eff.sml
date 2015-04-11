signature UNIV =
sig
  type t
  val to_u   : 'a -> t
  val from_u : t -> 'a
end

structure Univ :> UNIV =
struct
  type t = Word8.word vector
  fun to_u v = MLton.serialize v
  fun from_u v = MLton.deserialize v
end

signature EFF =
sig
  type 'a eff = exn                                         (* Effect type *)
  type ('a,'b) cont                                   (* Continuation type *)
  type ('a,'b) handler = 'a eff -> ('a,'b) cont -> 'b      (* handler type *)

  val perform     : 'a eff -> 'a
  val handleFun   : ('a,'b) handler -> (unit -> 'b) -> 'b
  val continue    : ('a,'b) handler -> ('a,'b) cont -> 'a -> 'b
  val discontinue : ('a,'b) handler -> ('a,'b) cont -> exn -> 'b
end

structure Eff :> EFF =
struct
  structure MT = MLton.Thread

  type 'a eff = exn

  (* Type of argument of internal continuations. *)
  datatype arg =
    ArgVal of Univ.t
  | Exception of exn

  (* Type of result of internal continuations. *)
  datatype 'a result =
    ResVal of Univ.t
  | Effect of ('a eff * arg MT.t)

  type ('a,'b) cont = arg MT.t
  type ('a,'b) handler = 'a eff -> ('a,'b) cont -> 'b

  (* A reference that points to the parent thread. *)
  val parentContRef = ref NONE

  fun perform e =
    case !parentContRef of
      NONE => raise Fail "No parent thread"
    | SOME pt =>
        let
          val a = MT.switch (fn ct => MT.prepare (pt, Effect (e, ct)))
        in
          case a of
            ArgVal v => Univ.from_u v
          | Exception e => raise e
        end

  fun continueCore handler rt =
  let
    val pt = !parentContRef
    val r = MT.switch (fn t => (parentContRef := SOME t; rt))
    val () = parentContRef := pt
  in
    case r of
      ResVal v => Univ.from_u v
    | Effect (e, ct) => handler e ct
  end

  fun handleFun handler f =
  let
    fun mkThrd f () =
    let
      val res = f ()
    in
      case !parentContRef of
        NONE => raise Fail "No parent thread"
      | SOME pt => MT.switch (fn ct => MT.prepare (pt, ResVal (Univ.to_u res)))
    end
    val f' = mkThrd f
    val newRT = MT.prepare (MT.new f', ())
  in
    continueCore handler newRT
  end

  fun continue handler cont v =
    continueCore handler (MT.prepare (cont, ArgVal (Univ.to_u v)))

  fun discontinue handler cont e =
    continueCore handler (MT.prepare (cont, Exception e))
end
