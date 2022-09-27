Define a function to double numbers

>double :: Int -> Int
>double x = 2*x

Define a function to find the hypotenuse of a right-angled triangle

>hypotenuse :: (Float, Float) -> Float
>hypotenuse (x,y) = sqrt(x*x + y*y)

Define a function to sum a list of integers

>sumList :: [Int] -> Int
>sumList x
>   |null x                 = 0
>   |otherwise              = head (x) + sumList(tail (x))

Use map in another function to sum the squares of a list