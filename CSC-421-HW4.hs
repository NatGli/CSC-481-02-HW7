{-
CSC 421 Theory of Programming Luanguage Homework Assignment 4

You have to complete all the question by your own
-}

{- Question 1
    Using the addition function over the natural numbers, 
    give a recursive definition of multiplication of natural numbers.

    *natural numbers: positive integers
    *Needs recurssion and addition
    *Multiplies positive integers
-}
multofNN :: Integer -> Integer -> Integer 
multofNN m n
    |m == 0 = 0
    |otherwise = n + multofNN n (m - 1)

{- Question 2
    The integer square root of a positive integer n is the largest 
    integer whose square is less than or equal to n. For instance, 
    the integer square roots of 15 and 16 are 3 and 4, respectively. 
    Given a primitive recursive definition of this function.

    *Needs recurssion, this means we need base case which will be 0 and 1
    *positive integers
    *
-}
intsqrt :: Integer -> Integer
intsqrt m
    |m < 2 = m
    |(intsqrt (m-1)+1) * (intsqrt (m-1)+1) > m = intsqrt (m-1)
    |otherwise = intsqrt (m-1) + 1

{- Question 3
    Give a recursive definition of a function to find the highest common factor of two positive integers.

    *Modulous gets remainder
-}
hcf :: Int -> Int -> Int
hcf m n
    |n == 0 = m
    |otherwise = hcf n (m `mod` n)

{- Question 4
    Give a definition of the function below
	    \[\text{orderTriple } :: (\text{Integer, Integer, Integer})  -> (\text{Integer, Integer, Integer}) \]
	    which puts the elements of a triple of three integers into ascending order. 
    
    *Six possibilites, 3!
-}
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (m, n, p)
    |m <= n && m <= p = if n <= p then (m, n, p) else (m, p, n)
    |n <= m && n <= p = if m <= p then (n, m, p) else (n, p, m)
    |otherwise = if m <= n then (p, m, n) else (p, n, m)

{- Question 5
    Define a function to give the length of the perimeter of a geometrical shape, of type Shape. 
    What is the type of this function?

    *I don't think Shape is a type in Haskell. But you can make it.
    *However, "peri" takes type Shape and outputs a Float.
-}
data Shape = Circle Float
           |Rectangle Float Float
           |Triangle Float Float Float
           |Square Float

peri :: Shape -> Float
peri (Circle rad) = 2 * pi * rad
peri (Rectangle len wid) = 2 * (len + wid)
peri (Triangle m n p) = m + n + p
peri (Square sidelen) = 4 * sidelen

{- Question 6
    Add an extra constructor to Shape for triangles, and extend the functions isRound, 
    area and perimeter to include triangles.

    *area function?
-}
isRound :: Shape -> Bool
isRound (Circle m) = True
isRound (Rectangle m n) = False
isRound (Triangle m n p) = False
isRound (Square m) = False

area :: Shape -> Float --Area for Triangles would look like this
area (Triangle m n p) = sqrt (s * (s - m) * (s - n) * (s - p))
    where s = (m + n + p) / 2

{- Question 7
    Give a definition of a function below,
	which triples all the elements of a list of integers.
-}
tripleAll :: [Integer] -> [Integer]
tripleAll [] = []
tripleAll (x:xs) = (3 * x) : tripleAll xs

{- Question 8
    Give a definition of a function
	which converts all small letters in a String into capitals, 
    leaving the other characters unchanged. 
-}
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs)
    |x >= 'a' && x <= 'z' = (toEnum (fromEnum x - 32)) : capitalize xs
    |otherwise = x : capitalize xs

{- Question 9
    Define the function below,
	which returns the list of divisors of a positive integer (and the empty list for other inputs). 
    For instance,
	divisors 12 = [1,2,3,4,6,12]

    *We can use List comprehension instead of recursion for this problem.
    *divisors = mod
-}
divisor :: Integer -> [Integer]
divisor m = [x | x <- [1..m], m `mod` x == 0] 

{- Question 10
    A prime number $n$ is a number whose only divisors are 1 and n. 
    Using divisor or otherwise define a function
	which checks whether or not a positive integer is prime (and returns False if its input is not a positive integer).

    *prime #'s only have 1 and n as a divisor so...
-}
isPrime :: Integer -> Bool
isPrime n = n > 1 && divisor n == [1, n]