module Lecture05 where

import Data.List
import Data.Maybe
--import Data.Monoid
{-
  05: Ленивость

  - Пример ленивых вычислений
  - Ленивость в других языках (yield, ||, &&)
  - Что такое ленивость
    - thunk
    - lazy evaluation
    - whnf, hnf, nf
  - Почему ленивость это хорошо
    - бесконечные списки
    - ленивость помогает компилятору оптимизировать код
  - Почему ленивость это плохо
    - space leaks
  - Как с этим бороться
    - (!, seq)
    - foldl, foldl', foldr
    - records strict fields
    - LANGUAGE strict, LANGUAGE strictdata
  - newtype lifting https://wiki.haskell.org/Newtype

  Подробнее:
    - https://www.fpcomplete.com/blog/2017/09/all-about-strictness
    - https://wiki.haskell.org/Foldr_Foldl_Foldl'
    - https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
-}


-- <Задачи для самостоятельного решения>
{-
    Напишите функцию, вычисляющую n-ое простое число с помощью
    решета Эротосфена (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
    Используйте бесконечные списки.

    Функция `sieve` "просеивает" список, переданный в качестве аргумента,
    и возвращает другой список, но уже из простых чисел. Работает она так:
    берёт очередное число из списка и выкидывает из него все числа, которые делятся на это число.

    Чтобы с помощью `sieve` реализовать `nthPrime`, `sieve` должна работать на бесконечных списках.

    https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#/media/File:Sieve_of_Eratosthenes_animation.gif
-}
sieve :: [Integer] -> [Integer]
sieve = \case
 head : tail -> head : sieve (filter (\x -> x `mod` head /= 0) tail)
 []          -> []

-- Функция, возвращающая n-ое простое число. Для её реализации используйте функцию sieve
nthPrime :: Int -> Integer
nthPrime n = sieve [2..]!!(n - 1)

{-
    Недавно в интервью Forbes с Сергеем Гуриевым Андрей Мовчан решил показать, что он
    тоже в некотором смысле математик, но немного запутался в рассуждениях о ВВП Китая и США:

        30 секунд с привязкой по времени: https://youtu.be/bTnnTeXHp8w?t=1782

    Помогите математику Андрею как программисты. Используя бесконечные списки, напишите функцию,
    которая вычислит через сколько лет ВВП на душу населения Китая догонит ВВП США.

    ВВП Китая $10к на душу населения, растёт на 6% в год
    ВВП США   $66к на душу населения, растёт на 2% в год.

    Можете воспользоваться функцией [iterate](https://hoogle.haskell.org/?hoogle=iterate).
    Она возвращает бесконечный список, каждый элемент которого получен из предыдущего
    применением первого аргумента ко второму:

        iterate f x = [x, f(x), f(f(x)), f(f(f(f))), ...]
-}

-- Возвращает бесконечный список ВВП на годы и годы вперёд
-- yearGDP 100 0.1 ~> [100, 100.1, 100.20009(9), 100.3003.., ...]
yearGDP :: Double -> Double -> [Double]
yearGDP now percent = iterate (*(1 + percent / 100)) now

-- Возвращает количество лет, которые нужны Китаю, чтобы догнать США в текущих условиях
inHowManyYearsChinaWins :: Int
inHowManyYearsChinaWins = fromMaybe 0 (findIndex (\x -> fst x <= snd x) (zip (yearGDP 66 2) (yearGDP 10 6))) + 1

{-
  Пусть у нас есть некоторая лента событий, каждое сообщение в которой говорит,
  сколько людей заболело в очередной стране. Нужно посчитать, сколько больных
  в каждой из перечисленных стран:

    stat [(China, 1000), (Italy, 47), (Russia, 14), (Italy, 98), (China, 107)] ~>
      [(China, 1107), (Russia, 14), (Italy, 145), (USA, 0), (GreatBritain, 0)]

    stat [(China, 80026)] ~>
      [(China, 80026), (Russia, 0), (Italy, 0), (USA, 0), (GreatBritain, 0)]

    В тестах есть случай с большими списками. Поэтому обычное решение будет
    неэффективным и нужно придумать как его улучшить с помощью строгих вычислений.
-}

data Country = Country String Integer deriving (Eq, Show)

data CountryCounter = CountryCounter
  { china :: Integer
  , russia :: Integer
  , italy :: Integer
  , usa :: Integer
  , uk  :: Integer
  } deriving (Eq)

instance Semigroup CountryCounter where
  a <> b = CountryCounter {
            china = china a + china b,
            russia = russia a + russia b,
            italy = italy a + italy b,
            usa = usa a + usa b,
            uk = uk a + uk b
          }

instance Monoid CountryCounter where
  mempty = CountryCounter { china = 0, russia = 0, italy = 0, usa = 0, uk = 0}

fromCounter :: CountryCounter -> [Country]
fromCounter counter = [ Country "China" (china counter)
                       , Country "Russia" (russia counter)
                       , Country "Italy" (italy counter)
                       , Country "USA" (usa counter)
                       , Country "GreatBritain" (uk counter) ]


addCountry :: CountryCounter -> Country -> CountryCounter
addCountry acc next = case next of
  Country "China" !num        -> acc { china = china acc + num }
  Country "Russia" !num       -> acc { russia = russia acc + num }
  Country "Italy" !num        -> acc { italy = italy acc + num }
  Country "USA" !num          -> acc { usa = usa acc + num }
  Country "GreatBritain" !num -> acc { uk = uk acc + num }
  _                           -> acc

stat :: [Country] -> [Country]
stat events = fromCounter (foldl' addCountry mempty events)

-- </Задачи для самостоятельного решения>