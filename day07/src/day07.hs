{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad            ((<=<))
import           Control.Monad.State.Lazy
import           Data.Char
import           Data.IntMap.Lazy         (IntMap, (!))
import qualified Data.IntMap.Lazy         as IntMap
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           Text.Read                (readMaybe)

type Register = IntMap.IntMap Int

type Address = Int

type Operation = Int -> Int -> Int

type Comp = Int -> Int -> Bool

data Status = Running | Stop
  deriving (Eq, Show)

type Execution a = StateT Program Maybe a

data Program = Program
  { counter  :: Address
  , register :: Register
  , inputs   :: [Int]
  , outputs  :: [Int]
  , status   :: Status
  } deriving (Eq, Show)


data Mode = Immediate | Position
  deriving (Eq, Show)


instance Show Op where
  show (Alu _ m1 m2)  = "Alu " ++ show m1 ++ " " ++ show m2
  show (Jump _ m1 m2) = "Jump " ++ show m1 ++ " " ++ show m2
  show Input          = "Input"
  show (Output m1)    = "Output " ++ show m1
  show Terminate      = "Terminate"

data Op
  = Alu Operation Mode Mode
  | Jump Comp Mode Mode
  | Input
  | Output Mode
  | Terminate


(##) :: [a] -> Int -> Maybe a
(x:_) ## 0  = Just x
[] ## _     = Nothing
(x:xs) ## n = xs ## (n - 1)


toModes :: Int -> Maybe Mode
toModes 0 = Just Position
toModes 1 = Just Immediate
toModes _ = Nothing

prependZeros :: String -> String
prependZeros xs
  | length xs >= 5 = xs
  | otherwise = prependZeros ('0' : xs)

decode :: Int -> Maybe Op
decode inst = do
  let (op', modes') = splitAt 2 . reverse . prependZeros $ show inst
  op <- readMaybe . reverse $ op'
  modes <- traverse (toModes . digitToInt) modes'
  let m1 = modes ## 0
  let m2 = modes ## 1
  let m3 = modes ## 2
  case op of
    1  -> liftA2 (Alu (+)) m1 m2
    2  -> liftA2 (Alu (*)) m1 m2
    3  -> Just Input
    4  -> fmap Output m1
    5  -> liftA2 (Jump (/=)) m1 m2
    6  -> liftA2 (Jump (==)) m1 m2
    7  -> liftA2 (Alu lt) m1 m2
    8  -> liftA2 (Alu eq) m1 m2
    99 -> Just Terminate
    _  -> Nothing

comp :: Comp -> Operation
comp op a b
  | a `op` b = 1
  | otherwise = 0

lt :: Operation
lt = comp (<)

eq :: Operation
eq = comp (==)

getReg :: Address -> Execution Int
getReg addr = do
  reg <- gets register
  lift (IntMap.lookup addr reg)

load :: Mode -> Address -> Execution Int
load Position addr  = load Immediate addr >>= getReg
load Immediate addr = (+ addr) <$> gets counter >>= getReg

store :: Address -> Int -> Execution ()
store addr val = modify insertVal
  where
    insertVal :: Program -> Program
    insertVal program = let r' = IntMap.insert addr val (register program)
                         in program{register=r'}

increaseCounter :: Int -> Execution ()
increaseCounter i = modify (\p -> p{counter=counter p + i})

setCounter :: Int -> Execution ()
setCounter i = modify (\p -> p{counter=i})

alu :: Operation -> (Mode, Mode) -> Execution ()
alu op (m1, m2) = do
  a <- load m1 1
  b <- load m2 2
  addr <- load Immediate 3
  increaseCounter 4
  store addr (a `op` b)

jump :: Comp -> (Mode, Mode) -> Execution ()
jump comp (m1, m2) = do
  val <- load m1 1
  addr <- load m2 2
  if val `comp` 0 then setCounter addr else increaseCounter 3

output :: Mode -> Execution ()
output mode = do
  x <- load mode 1
  modify (\p -> p{outputs = x : outputs p})
  increaseCounter 2

safeHead :: [Int] -> Maybe (Int, [Int])
safeHead (x:xs) = Just (x, xs)
safeHead _      = Nothing

input :: Execution ()
input = do
  (x, xs) <- gets inputs >>= lift . safeHead
  modify (\p -> p{inputs=xs})
  dest <- load Immediate 1
  store dest x
  increaseCounter 2

step :: Execution ()
step = do
  s <- gets status
  case s of
    Stop -> return ()
    _ -> do
      reg <- gets register
      op <- load Immediate 0 >>= lift . decode
      case op of
        Alu op m1 m2    -> alu op (m1, m2)
        Input           -> input
        Jump comp m1 m2 -> jump comp (m1, m2)
        Output mode     -> output mode
        Terminate       -> modify (\p -> p{status=Stop})

initRegister :: [Int] -> Register
initRegister = IntMap.fromList . zip [0 ..]

initProgram :: [Int] -> [Int] -> Program
initProgram memory inputs = Program
  { register=initRegister memory
  , inputs=inputs
  , outputs=[]
  , counter=0
  , status=Running
  }

run :: Program -> Maybe Program
run p@Program {status=Stop, ..} = Just p
run p                           = execStateT step p >>= run

verifyProg :: Monad m => Program -> [Int] -> m ()
verifyProg prog expected =
  assert ((register <$> run prog) == Just (initRegister expected)) return ()

verifyOutput :: Monad m => Program -> Int -> m ()
verifyOutput prog expected =
  assert ((outputs <$> run prog) == Just [expected]) return ()

loadFile :: String -> IO [Int]
loadFile = fmap (map read . splitOn ",") . readFile

runDiagnostic :: Program -> [Program]
runDiagnostic = recurse []
  where
    recurse :: [Program] -> Program -> [Program]
    recurse xs p@Program{status=Stop} = xs
    recurse xs p = let x = execStateT step p
                    in case x of
                         Just p  -> recurse (p:xs) p
                         Nothing -> xs
main :: IO ()
main = do
  day05 <- loadFile "day05.txt"
  prog <- return [1,9,10,3,2,3,11,0,99,30,40,50]
  let prog05 = initProgram day05 [5]
  print $ outputs <$> (run prog05)
