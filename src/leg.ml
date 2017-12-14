open! Core_kernel

type t = {
  coupons: Coupon.t list;
}

let coupons leg = leg.coupons
let create coupons = { coupons; }