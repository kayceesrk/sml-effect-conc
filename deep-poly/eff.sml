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
  (** Effect type. *)
  type 'a eff = exn

  (** Continuation type: This represents a continuation that is waiting for a
      value of type 'a. The continuation was forged from a function returns a
      value of type 'b, which is processed by a handler that consumes 'b value
      and produces a 'c value. This ends up being the return value of the
      continuation. *)
  type ('a,'b,'c) cont

  (** Handler has type "('a,'b,'c) handler" instead of 'b handler since Standard
      ML does not have existential types (or I am not aware of some clever way of
      encoding it). *)
  datatype ('a,'b,'c) handler = Handler of
    {catch   : 'a eff -> ('a,'b,'c) cont -> 'c,
     finally : 'b -> 'c}

  val perform     : 'a eff -> 'a
  val newCont     : ('a,'b,'c) handler -> ('a -> 'b) -> ('a,'b,'c) cont
  val continue    : ('a,'b,'c) cont -> 'a -> 'c
  val discontinue : ('a,'b,'c) cont -> exn -> 'c

  (** handleFun is a composition of newCont and continue. *)
  val handleFun   : ('a,'b,'c) handler -> ('a -> 'b) -> 'a -> 'c
end

structure Eff : EFF =
struct
  structure MT = MLton.Thread

  type 'a eff = exn

  (* Type of argument of internal continuations. *)
  datatype arg =
    ArgVal of Univ.t
  | Exception of exn

  datatype ('a,'b,'c) cont =
    Cont of {k : arg MT.t,
             h : ('a,'b,'c) handler}
  and ('a,'b,'c) handler =
    Handler of {catch   : 'a eff -> ('a,'b,'c) cont -> 'c,
                finally : 'b -> 'c}

  (* Type of result of internal continuations. *)
  datatype 'a result =
    ResVal of Univ.t
  | Effect of ('a eff * arg MT.t)

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

  fun newCont handler f =
  let
    fun mkThrd f v =
    let
      val res =
        case v of
          ArgVal a => f (Univ.from_u a)
        | Exception e => raise Fail "What is the expected behavior here?"
    in
      case !parentContRef of
        NONE => raise Fail "No parent thread"
      | SOME pt => MT.switch (fn ct => MT.prepare (pt, ResVal (Univ.to_u res)))
    end
  in
    Cont {k = MT.new (mkThrd f), h = handler}
  end

  fun continueCore (Cont {k=cont,h=handler}) v =
  let
    val Handler {catch, finally} = handler
    val pt = !parentContRef
    val r = MT.switch (fn t =>
             (parentContRef := SOME t;
              MT.prepare (cont, v)))
    val () = parentContRef := pt
  in
    case r of
      ResVal v => finally (Univ.from_u v)
    | Effect (e, ct) => catch e (Cont {k=ct,h=handler})
  end

  fun continue c v = continueCore c (ArgVal (Univ.to_u v))
  fun discontinue c v = continueCore c (Exception v)

  fun handleFun handler f v = continue (newCont handler f) v
end
