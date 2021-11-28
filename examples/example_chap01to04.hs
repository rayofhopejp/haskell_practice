doubleMe x = x+x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x> 100
                    then x 
                    else x*2
doubleSmallNumber' x = (if x> 100 then x else x*2) + 1
list1 = [1,2,3,4] ++ [5,6,7,8]
list2 = 5 : [1,2,3,4]
getElement = [0,1,2,3,4] !! 2
list3 = ['a'..'z']
list4 = [2,4..20]
list5 = take 10 $ cycle [1,2,3]
list6 = replicate 3 10

-- リスト内包表記
list7 = [x*2 | x<-[1..10], mod x 7 == 3]
boomBangs xs = [if x<10 then "BOOM!" else "BANG!" | x<-xs,odd x ]
list8 = [x+y| x<-[1,2,3] , y<-[100,200,300]]
list9 = [[1,3,5,2,3,1,2,4,5],[1..9],[1,2,4,2,1,5,3,1,3,2,3,6]]
list10 = [ [x | x<-xs, even x] | xs<-list9]
tuple1 = (1,"hello",9.0)
vector1=[(1,29),(3,4),(3,3)]
list11 = zip [1,2,3,4] [5,6,7,8,9] --[(1,5),(2,6),(3,7),(4,8)]
triangle = [(x,y,z)| x<-[1..10],y<-[1..x],z<-[1..y],x+y+z==24,y^2+z^2==x^2]

-- 関数
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z
factorial :: Integer -> Integer
factorial n = product [1..n]

-- パターンマッチ
rec_factorial :: Int -> Int
rec_factorial 0 = 1
rec_factorial x = x * rec_factorial (x-1)
addvectors :: (Double,Double) -> (Double,Double) -> (Double,Double)
addvectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)
list12 = [x*100+3 | (x,3) <- [(1,3),(2,2),(4,3)]]
head' :: [a] -> a
head' [] = error "error"
head' (x:_) = x
-- as pattern (name@(part))
firstletter "" = "Empty string"
firstletter all@(x:_) = "string is " ++ all ++ " first letter is " ++ [x]

-- guard
-- where
bmiTell weight height
    | bmi <= 18.5 = "You are underweight"
    | bmi <= 25.0 = "You are normal"
    | bmi <= 30.0 = "overweight"
    | otherwise = "too overweight"
    where bmi = weight/(height^2)

initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

--whereブロック内の関数
calcBmis :: [(Double,Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight/height ^ 2

-- let式(ガードの中でしか使えない変数)
cylinder ::  Double -> Double -> Double
cylinder r h = 
    let sideArea = 2*pi*r*h
        topArea=pi*r^2
    in sideArea+2*topArea

calcBmis' :: [(Double,Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <-xs, let bmi=w/h^2 , bmi>25.0]

--case式
head_case :: [a] -> a
head_case xs =  case xs of [] -> error "No head for empty list"
                           (x:_) -> x

--recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- fold(畳み込み)
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

