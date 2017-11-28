open! Core_kernel

let discount_factors coupons y npv_date =
  let rev_d_factors,_ = List.fold_left coupons ~init:([1.0;],npv_date) 
  ~f:(fun b a -> 
    let acc_d,last_date = b in
    ((Interest_rate.discount_factor y last_date (Coupon.date a)) :: acc_d),(Coupon.date a)) in
  let plain_d_factors = List.drop (List.rev rev_d_factors) 1 in
  List.fold_left plain_d_factors ~init:[] ~f:(fun b a -> match List.hd b with
  | None -> a :: []
  | Some h -> a *. h :: b) |> List.rev

let npv leg y settlement_date npv_date =  
  let coupons = Leg.coupons leg in  
  let d_factors = discount_factors coupons y npv_date in
  let coupon_d_factors = List.zip_exn coupons d_factors in
  List.fold_left coupon_d_factors ~init:0.0 ~f:(fun b a -> 
    let c,d = a in 
    let amount = Coupon.amount c in
    let cf = if Coupon.has_occured c settlement_date then 0.0 else amount *. d in
    b +. cf)    
     