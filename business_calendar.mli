open! Core_kernel

type t =
  | USSettlementCalendar
  | USGovernmentBondCalendar

val is_business_day: t -> Date.t -> bool

val adjust: t -> Business_day_convention.t -> Date.t -> Date.t