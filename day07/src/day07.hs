{-# LANGUAGE RecordWildCards #-}

import           Control.Exception
import           Control.Monad      ((>=>))
import           Data.Char
import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace

type Register = IntMap.IntMap Int

type Address = Int

type Op = Int -> Int -> Int

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

type Modes = [Int]

(##) :: [a] -> Int -> Maybe a
(x:_) ## 0  = Just x
[] ## _     = Nothing
(x:xs) ## n = xs ## (n - 1)


decode :: Int -> (Int, [Int])
decode inst = (read opcode, reverse $ map digitToInt paramModes)
  where
    s = show inst
    (paramModes, opcode) = splitAt (length s - 2) s


-- Volgens mij moet deze nog gerefactored worden naar gewoon Maybe
mode :: Int -> Modes -> Int
mode idx modes =  fromMaybe 0 $ modes ## idx

comp :: Comp -> Op
comp op a b
  | a `op` b = 1
  | otherwise = 0

lt :: Op
lt = comp (<)

eq :: Op
eq = comp (==)


getReg :: Program -> Address -> Maybe Int
getReg Program{..} = flip IntMap.lookup register . (counter +)

loadWord :: Program -> Modes -> Address -> Maybe Int
loadWord p@Program {..} modes x = (getReg p >=> load (mode (x - 1) modes)) x
  where
    load :: Int -> Address -> Maybe Int
    load mode val =
      case mode of
        0 -> IntMap.lookup val register
        1 -> Just val
        _ -> Nothing

alu :: Program -> Modes -> Op -> Maybe Program
alu p@Program {..} modes op = do
  a <- loadWord p modes 1
  b <- loadWord p modes 2
  addr <- getReg p 3
  let reg' = IntMap.insert addr (a `op` b) register
  return p{register=reg', counter=counter + 4}

jump :: Program -> Modes -> Comp -> Maybe Program
jump p@Program {..} modes comp = do
  val <- loadWord p modes 1
  addr <- loadWord p modes 2
  let counter' = if val `comp` 0 then addr else counter + 3
  return p{counter=counter'}

store :: Program -> Modes -> Maybe Program
store p@Program{..} modes = do
  x <- loadWord p modes 1
  return p{outputs=x : outputs, counter=counter + 2}

safeHead :: [Int] -> Maybe (Int, [Int])
safeHead (x:xs) = Just (x, xs)
safeHead _      = Nothing

load :: Program -> Maybe Program
load p@Program{..} = do
  (x, xs) <- safeHead inputs
  dest <- getReg p 1
  let reg' = IntMap.insert dest x register
  return p{register=reg', inputs=xs, counter=counter + 2}

step :: Program -> Maybe Program
step p@Program{status=Stop} = Just p
step p@Program{..} = do
  (opcode, modes) <- decode <$> getReg p 0
  let alu' = alu p modes
      jump' = jump p modes
  case opcode of
    1  -> alu' (+)
    2  -> alu' (*)
    3  -> load p
    4  -> store p modes
    5  -> jump' (/=) -- jump-if-true
    6  -> jump' (==) -- jump-if-false
    7  -> alu' lt -- less than
    8  -> alu' eq -- equals
    99 -> Just p{status=Stop}
    _  -> Nothing

initRegister :: [Int] -> Register
initRegister = IntMap.fromList . zip [0 ..]

initProgram :: [Int] -> [Int] -> Program
initProgram memory inputs = Program
  { register=initRegister memory
  , inputs=reverse inputs
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
  print $ run $ prog05
