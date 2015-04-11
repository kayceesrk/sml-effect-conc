signature EFF =
sig
  type a (* Type of result of effects / argument of continuations *)
  type b (* Type of result of continuations *)
  type c (* Type of result of handlers *)

  type eff = exn  (* Effect type *)
  type cont       (* Continuation type *)

  datatype handler = Handler of
    {catch   : eff -> cont -> c,
     finally : b -> c}

  val perform     : eff -> a
  val newCont     : handler -> (a -> b) -> cont
  val continue    : cont -> a -> c
  val discontinue : cont -> exn -> c
  val handleFun   : handler -> (a -> b) -> a -> c
end

functor Eff (S: sig
                  type a
                  type b
                  type c
                end) : EFF =
struct
  structure MT = MLton.Thread

  type a = S.a
  type b = S.b
  type c = S.c

  type eff = exn

  (* Type of argument of internal continuations. *)
  datatype arg =
    ArgVal of a
  | Exception of exn

  datatype handler = Handler of {catch : eff -> cont -> c, finally : b -> c}
  and cont = Cont of {k : arg MT.t, h : handler}

  (* Type of result of internal continuations. *)
  datatype result =
    ResVal of b
  | Effect of (eff * arg MT.t)

  (* A reference that points to the parent thread. *)
  val parentContRef : result MT.t option ref = ref NONE

  fun perform e =
    case !parentContRef of
      NONE => raise Fail "No parent thread"
    | SOME pt =>
        let
          val a = MT.switch (fn ct => MT.prepare (pt, Effect (e, ct)))
        in
          case a of
            ArgVal v => v
          | Exception e => raise e
        end

  fun newCont handler f =
  let
    fun mkThrd f v =
    let
      val res =
        case v of
          ArgVal a => f a
        | Exception e => raise Fail "What is the expected behavior here?"
    in
      case !parentContRef of
        NONE => raise Fail "No parent thread"
      | SOME pt => MT.switch (fn ct => MT.prepare (pt, ResVal res))
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
      ResVal v => finally v
    | Effect (e, ct) => catch e (Cont {k=ct,h=handler})
  end

  fun continue c v = continueCore c (ArgVal v)
  fun discontinue c v = continueCore c (Exception v)

  fun handleFun handler f v = continue (newCont handler f) v
end
