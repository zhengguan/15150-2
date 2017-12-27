structure ListSeq : SEQUENCE = struct
  structure LS = DeriveSequenceTransparent (ListCore)
  open LS
end
