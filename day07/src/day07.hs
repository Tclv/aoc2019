{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Exception
import           Control.Monad       ((<=<))
import           Data.Char
import           Data.IntMap.Strict  (IntMap, (!))
import qualified Data.IntMap.Strict  as IntMap
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           Text.Read           (readMaybe)

type Register = IntMap.IntMap Int

type Address = Int

type Operation = Int -> Int -> Int

type Comp = Int -> Int -> Bool

data Status = Running | Stop
  deriving (Eq, Show)

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

getReg :: Program -> Address -> Maybe Int
getReg Program{..} = flip IntMap.lookup register

loadWord :: Program -> Mode -> Address -> Maybe Int
loadWord p@Program {..} Position  = getReg p <=< getReg p . (counter +)
loadWord p@Program {..} Immediate = getReg p . (counter +)

alu :: Program -> Operation -> (Mode, Mode) -> Maybe Program
alu p@Program {..} op (m1, m2) = do
  a <- loadWord p m1 1
  b <- loadWord p m2 2
  addr <- loadWord p Immediate 3
  let reg' = IntMap.insert addr (a `op` b) register
  return p{register=reg', counter=counter + 4}

jump :: Program -> Comp -> (Mode, Mode) -> Maybe Program
jump p@Program {..} comp (m1, m2) = do
  val <- loadWord p m1 1
  addr <- loadWord p m2 2
  let counter' = if val `comp` 0 then addr else counter + 3
  return p{counter=counter'}

output :: Program -> Mode -> Maybe Program
output p@Program{..} mode = do
  x <- loadWord p mode 1
  return p{outputs=x : outputs, counter=counter + 2}

safeHead :: [Int] -> Maybe (Int, [Int])
safeHead (x:xs) = Just (x, xs)
safeHead _      = Nothing

input :: Program -> Maybe Program
input p@Program{..} = do
  (x, xs) <- safeHead inputs
  dest <- loadWord p Immediate 1
  let reg' = IntMap.insert dest x register
  return p{register=reg', inputs=xs, counter=counter + 2}

step :: Program -> Maybe Program
step p@Program{status=Stop} = Just p
step p@Program{..} = do
  op <- loadWord p Immediate 0 >>= decode
  case op of
    Alu op m1 m2    -> alu p op (m1, m2)
    Input           -> input p
    Jump comp m1 m2 -> jump p comp (m1, m2)
    Output mode     -> output p mode
    Terminate       -> Just p{status=Stop}

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
run p                           = step p >>= run

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
    recurse xs p = let x = step p
                    in case x of
                         Just p  -> recurse (p:xs) p
                         Nothing -> xs
main :: IO ()
main = do
  day05 <- loadFile "day05.txt"
  let prog05 = initProgram day05 [5]
  print $ outputs <$> run prog05
