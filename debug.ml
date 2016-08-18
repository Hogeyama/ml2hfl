
module type COND = sig
  val check : unit -> bool
end

module type DEBUG = sig
  val check : unit -> bool
  val printf : ('a, Format.formatter, unit) format -> 'a
  val eprintf : ('a, Format.formatter, unit) format -> 'a
  val fprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val exec : (unit -> unit) -> unit
end

module Make (Cond : COND) : DEBUG = struct
  let check = Cond.check
  let fprintf fm f = if check() then Format.fprintf fm f else Format.ifprintf fm f
  let printf f = fprintf Format.std_formatter f
  let eprintf f = fprintf Format.err_formatter f
  let exec f = if check() then f ()
end
