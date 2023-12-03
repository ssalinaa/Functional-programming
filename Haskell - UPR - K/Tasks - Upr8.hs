main :: IO()
main = do
        print (mymin 5 6)
        print (isInside 5 1 8)
        print (isInside' 5 1 8)
        print (myfunc 1 2)
        print (myfunc' 1 2)
        print (myfibRec 1)
        print (myfibRec 5)
        print (myfibRec' 5)
        print (myfibIter 1)
        print (myfibIter 5)
        print (mygcd 16 24)
        print (mymaxdivisor 16)
        print (sumOddsInRange 1 6)
        print (sumOddsInRange' 1 6)
        print (isPrime 6)
        print (isPrime 11)
        print (isPrime' 6)
        print (isPrime' 11)
        print (reversed 12345)
        print (isPalindrome 12321)
        print (isPalindrome 12547)
        print (countPalindromes 1 20)
        print (countDivisorsIter 15)

{-
Задача 1. Да се напише функция mymin, която приема два 
аргумента и връща по-малкия от тях.
-}

mymin :: Double -> Double -> Double
mymin a b = if a < b then a else b

{-
Задача 2. Да се дефинира функцията isInside x a b, 
която проверява дали числото x се намира в затворения 
интервал [a, b].
-}

isInside :: Int -> Int -> Int -> Bool
isInside x a b = x >= a && x <= b

isInside' :: Int -> Int -> Int -> Bool
isInside' x a b 
 | x < a = False
 | x > b = False
 | otherwise = True

{-
Задача 3. Да се напише функция myfunc, 
която пресмята средно аритметично на квадратите на 2 числа.
-}

myfunc :: Int -> Int -> Double
myfunc a b = fromIntegral (a * a + b * b) / 2 

myfunc' :: Double -> Double -> Double
myfunc' a b = (a ^ 2 + b ^ 2) / 2 

{-
Задача 4. Да се напише myfib, която получава един аргумент n 
и връща n-тото число на Фибоначи. 
Да се напише и итеративно решение

(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)
-}

myfibRec :: Int -> Int
myfibRec n 
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = myfibRec (n - 2) + myfibRec (n - 1)

myfibRec' :: Int -> Int
myfibRec' n = if  n <= 1 then 1 else myfibRec' (n - 2) + myfibRec' (n - 1)

myfibIter :: Int -> Int
myfibIter n = helper 0 0 1
    where 
        helper i prev cur =
            if i == n then cur
            else helper (i + 1) cur (prev + cur)

{-
Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}

mygcd :: Int -> Int -> Int
mygcd a b 
 | a == 0 = b
 | b == 0 = a
 | otherwise = mygcd b (mod a b)

{-
Задача 6. Да се напише функция mymaxdivisor x, 
която намира най-големия делител d на цялото число x > 1, 
за който d < x.
-}

mymaxdivisor :: Int -> Int
mymaxdivisor x = helper (x - 1)
    where
     helper d =
        if (x `mod` d == 0) then d
        else helper (d - 1)

{-
Задача 7. Да се дефинира функция, 
която намира сумата на нечетните числа в затворения 
интервал [a, b].
-}

sumOddsInRange :: Int -> Int -> Int
sumOddsInRange a b = helper a
    where
        helper a
         | a > b = 0
         | odd a = a + helper (a + 1)
         | otherwise = helper (a + 1)

sumOddsInRange' :: Int -> Int -> Int
sumOddsInRange' a b = helper a 0
    where 
        helper a sum 
         | a > b = sum
         | odd a = helper (a + 1) (sum + a)
         | otherwise = helper (a + 1) sum

{-
Задача 8. Да се дефинира предикат, 
който проверява дали естественото число n е просто.
-}

isPrime :: Int -> Bool
isPrime n 
 | n == 1 = False
 | n == 2 = True
 | otherwise = helper 2
    where 
     helper d
      | d == n = True
      | mod n d == 0 = False
      | otherwise = helper (d + 1)

isPrime' :: Int -> Bool
isPrime' 1 = False
isPrime' 2 = True
isPrime' n = helper 2
    where
     helper d
      | d == n = True
      | mod n d == 0 = False
      | otherwise = helper (d + 1)

{-
Задача 9. Да се дефинира функция, която намира броя на 
палиндромите в интервала [a, b], където a и b са цели 
неотрицателни числа и a < b.
-}

reversed :: Int -> Int
reversed n = helper 0 n
    where
        helper result n =
            if n < 10 then result * 10 + n
            else helper (result * 10 + n `mod` 10) (n `div` 10)

isPalindrome :: Int -> Bool
isPalindrome n = n == reversed n

countPalindromes :: Int -> Int -> Int
countPalindromes a b 
 | a > b          = 0
 | isPalindrome a = 1 + countPalindromes (a + 1) b
 | otherwise = countPalindromes (a + 1) b

 {-
 Задача 10. Да се дефинира функция, която чрез линейно 
 итеративен процес намира броя на естествените делители на 
 едно естествено число.
 -}

countDivisorsIter :: Int -> Int
countDivisorsIter n = helper n 0
  where
    helper 0 count = count
    helper d count =
      if mod n d == 0 then helper (d - 1) (count + 1)
      else helper (d - 1) count 