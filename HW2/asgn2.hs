multiply::Int->Int->Int
multiply x y
  |x==0 =0
  |y==0 =0
  |otherwise =y + multiply (x-1) y

multiply_tr::Int->Int->Int
multiply_tr x y = multiply_tr_h x y 0
multiply_tr_h::Int->Int->Int->Int
multiply_tr_h x y rs
  |x==0 =rs
  |y==0 =rs
  |otherwise = multiply_tr_h (x-1) y (rs+y)

power::Int->Int->Int
power b f
  |f==0 =1
  |b==0 =0
  |f==1 =b
  |otherwise = multiply b  (power b (f-1))

power_tr::Int->Int->Int
power_tr b f =power_tr_h b f 1
power_tr_h::Int->Int->Int->Int
power_tr_h b f rs
  |f==0 =rs
  |b==0 =0
  |otherwise = power_tr_h b (f-1) (multiply_tr rs b)

harmonic::Int->Float
harmonic n
  |n==1 =1
  |otherwise =(fromIntegral 1)/(fromIntegral n) + harmonic (n-1)

harmonic_tr::Int->Float
harmonic_tr n = harmonic_tr_h n 1.0
harmonic_tr_h::Int->Float->Float
harmonic_tr_h n rs
  |n==1 =rs
  |otherwise = harmonic_tr_h (n-1) ((fromIntegral 1)/(fromIntegral n) + rs)
