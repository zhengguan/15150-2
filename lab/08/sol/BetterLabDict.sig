signature BETTERLABDICT =
sig
    type ('k, 'v) dict

    val empty : ('k * 'k -> order) -> ('k, 'v) dict

    val insertWrapper : ('k, 'v) dict -> ('k * 'v) -> ('k, 'v) dict

    val lookupWrapper : ('k, 'v) dict -> 'k -> 'v option

end
