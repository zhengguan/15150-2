structure PArraySequence 
(* 
: SEQUENCE 

FIXME: signature ascription gives spurious type errors:
signature: ['a<35CE>,'b<35CF>]('a<35CE> -> 'b<35CF>) -> 'a<35CE> seq -> 'b<35CF> seq
structure: ['M0<3630>,'M1<3631>]('M0<3630> -> 'M1<3631>) -> 'M0<3630> parray -> 'M1<3631> parray
[parray-sequence.pml:19.1-50.4] Error: failed to match value specification zip
signature: ['a<35DA>,'b<35DB>]('a<35DA> seq * 'b<35DB> seq) -> ('a<35DA> * 'b<35DB>) seq
structure: ['M0<36AC>,'M1<36AD>]('M1<36AD> parray * 'M0<36AC> parray) -> ('M1<36AD> * 'M0<36AC>) parray
[parray-sequence.pml:19.1-50.4] Error: failed to match value specification flatten
signature: ['a<35DD>]'a<35DD> seq seq -> 'a<35DD> seq
structure: ['M0<3706>]'M0<3706> parray parray -> 'M0<3706> parray
ntozake ~/150/src/sequence/manticore % 
*)
=
struct

    type 'a seq = 'a parray

    fun length s = PArray.length s

    fun nth i s = PArray.sub(s,i)

    fun tabulate f n = PArray.tab (n,f)

    fun map f s = PArray.map f s

    fun reduce n e s = PArray.reduce n e s

    fun mapreduce l e n s = reduce n e (map l s)

    fun repeat n e = tabulate (fn _ => e) n

    fun toString f s = PArray.toString f "," s

    fun zip (s1,s2) = tabulate (fn i => (nth i s1, nth i s2))
                               (Int.min (length s1, length s2))

    fun append s1 s2 = tabulate (fn i => 
                                 case i < length s1 of
                                     true => nth i s1
                                  | false => nth (i - length s1) s2)
                                (length s1 + length s2)
    fun empty () = tabulate (fn _ => raise Fail "empty-impossible") 0 (* implementing as [||] is causing trouble *)
    fun cons x xs = tabulate (fn 0 => x | i => nth (i - 1) xs) (length xs + 1)
    fun flatten ss = reduce (fn (x,y) => append x y) (empty()) ss
    fun singleton x = [| x |]

    (* FIXME: not very efficient, but PArray.filter doesn't seem to exist right now *)
    fun filter p s = 
        mapreduce 
        (fn x => case p x of true => singleton x 
                           | false => empty())
        (empty())
        (fn (x,y) => append x y)
        s
        
end

structure Seq = PArraySequence

