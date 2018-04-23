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
  |otherwise =b * power b (f-1)
