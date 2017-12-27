use "queues.sml";

(* select which representation you want to see run the example *)
structure Q = LLQ
(*structure Q = LQ*)

val q0 = Q.emp
val q1 = Q.ins (1,q0)
val q2 = Q.ins (2,q1)
val q3 = Q.ins (3,q2)
val SOME(1,q4) = Q.rem q3
val q5 = Q.ins(4,q4)
val SOME(2,q6) = Q.rem q5
val SOME(3,q7) = Q.rem q6
val SOME(4,q8) = Q.rem q7
val NONE = Q.rem q8
