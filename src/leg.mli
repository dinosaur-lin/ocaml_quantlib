
type t = {
  coupons: Coupon.t list;
}

val coupons: t -> Coupon.t list
val create: Coupon.t list -> t