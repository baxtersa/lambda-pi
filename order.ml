module type ORDER =
sig
  type t
  val compare : t -> t -> int
end
