type env = Unif.Env.t

(** Suppose the unification environment [env] contains the following equations:

    ?w = ?w1 -> ?w2
    ?w1 = int
    ?w2 = bool
    ?w3 = ?w1 -> ?w4
    ?w4 : no structure

    Then [decode env ?w] will return the type [int -> bool].

    Notice on the other hand that [?w3] is of the form [?w1 -> ?w4]
    for a still-undertermined variable [?w4]. Any type of the form
    [int -> foo] might work, for any [foo], but further progress in
    the inference process could end up in a solution incompatible with
    any specific choice of [foo]. We decide to decode this into
    a fresh (rigid) type variable (which is assumed distinct from
    everything else, but nothing else): [?w3] decodes into [int -> α].
*)
val decode : env -> Constraint.variable -> STLC.ty
