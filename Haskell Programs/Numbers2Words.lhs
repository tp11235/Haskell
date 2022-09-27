The function we need is:

>convert :: Int -> String

Define the data lists we will use:

>units, teens, tens :: [String]
>units = ["zero","one","two","three","four","five","six","seven","eight","nine"]
>teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
>tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

If you don't use a chevron you get a comment, but you must also leave blank lines
First we will convert 1 digit numbers - the wasy ones

>convert1 :: Int -> String
>convert1 n = units!!n

The symbol !! selects an item from a list at the nth location starting with zero

>digits2 :: Int -> (Int,Int)
>digits2 n = (div n 10, mod n 10)
>convert2 :: Int -> String
>convert2 = combine2.digits2
>combine2 :: (Int,Int) -> String
>combine2 (t,u)
>  |t==0          = units!!u
>  |t==1          = teens!!u
>  |2<=t && u==0  = tens!!(t-2)
>  |2<=t && u/=0  = tens!!(t-2) ++ "-" ++ units!!u

Now we will work on 3 digit numbers this is a different way of achieving a similar task, it avoids a combine function
Also notice the definition of h and t as an ordered pair at the end

>convert3 :: Int ->String
>convert3 n
>  | h==0      = convert2 t
>  | t==0      = units!!h ++ " hundred"
>  | otherwise = units!!h ++ " hundred and " ++ convert2 t
>  where (h,t) = (div n 100, mod n 100)

>convert6 :: Int->String
>convert6 n
>  | m==0      = convert3 h
>  | h==0      = convert3 m ++ " thousand"
>  | otherwise = convert3 m ++ " thousand" ++ link h ++
>                convert3 h
>  where (m,h) = (div n 1000, mod n 1000)

Now we need to define the function "link" that inserts the word "and" for some numbers, this shows an if-then-else structure

>link :: Int -> String
>link h = if h < 100 then " and " else " "

>convert = convert6

