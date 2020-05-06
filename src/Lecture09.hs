{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Lecture09 where

import qualified Data.ByteString as B
import Data.Serialize
import Data.List

import GHC.Generics

import System.Directory
import System.Random

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show, Serialize)

newtype Title = Title String deriving (Eq, Show, Serialize)

newtype Deadline = Deadline String deriving (Eq, Show, Serialize, Ord)

newtype Content = Content String deriving (Eq, Show, Serialize)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Generic)

instance Serialize Todo

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = TodoList rootFolder <$ (createDirectory rootFolder)

randomInt :: IO Int
randomInt = randomIO

randomId :: IO Id
randomId = fmap (Id . show) randomInt

todoPath :: FilePath -> Id -> FilePath
todoPath base (Id value) = base ++ "/" ++ value

writeTodo :: TodoList -> Id -> Todo -> IO ()
writeTodo (TodoList path) id todo = B.writeFile (todoPath path id) (encode todo)

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title text deadline = do
  id <- randomId
  writeTodo todoList id (Todo id title text deadline False)
  return $ id

eitherToIO :: Either String a -> IO a
eitherToIO = either fail pure

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList path) id = do
  byteString <- B.readFile (todoPath path id)
  todo       <- eitherToIO (decode byteString)
  return $ todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  todo <- readTodo todoList id
  putStrLn (show todo)

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList path) id = removeFile (todoPath path id)

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  todo <- readTodo todoList id
  writeTodo todoList id todo { title = title
                             , content = content
                             , deadline = deadline
                             }

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  todo <- readTodo todoList id
  writeTodo todoList id todo { isDone = True }

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo (TodoList path) = do
  files   <- (listDirectory path)
  todos   <- traverse (\file -> readTodo (TodoList path) (Id file)) files
  return $ (sortOn (deadline :: Todo -> Deadline) todos)

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = filter (not . isDone) `fmap` (readAllTodo todoList)

showTodos :: [Todo] -> IO ()
showTodos todos = () <$ traverse (\todo -> putStrLn (show todo)) todos

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = (readAllTodo todoList) >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readAllTodo todoList
  let unfinished = filter (not .isDone) todos
  showTodos unfinished

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  secret <- fmap (\i -> i `mod` 100) randomInt
  let step = do
        _     <- putStrLn "Guess number:"
        guess <- readLn
        _     <- putStrLn ("Your number is: " ++ show guess)
        case compare guess secret of
          LT -> putStrLn "Too small!" *> step
          GT -> putStrLn "Too big!" *> step
          EQ -> putStrLn "Yep, that's the number!"
  step

-- </Задачи для самостоятельного решения>