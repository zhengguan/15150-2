signature VECTOR =
sig

  type elem
  type vector

  val tabulate : (int -> elem) -> int -> vector

  val dotprod : vector * vector -> elem

  val eq : vector * vector -> bool

  val length : vector -> int

  val nth : vector * int -> elem
end


signature MATRIX =
sig

  structure Vector : VECTOR

  type elem = Vector.elem
  type vector = Vector.vector
  type matrix

  val tabulate : (int * int -> elem) -> int -> int -> matrix
  val update : matrix -> (int * int) -> elem -> matrix
  
  val transpose : matrix -> matrix
  val identity : int -> matrix
  val size : matrix -> (int * int)
  
  val add : matrix * matrix -> matrix
  val subtract : matrix * matrix -> matrix
  
  val row : matrix -> int -> vector
  val col : matrix -> int -> vector

  val mult : matrix * matrix -> matrix
  
  val eq : matrix * matrix -> bool

end

