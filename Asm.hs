{-

Hi Kashyap,

At BAE I've implemented an assembly DSL for an new processor where designing.
Here is the approach I took.  Hope this helps.

-Tom

-}

module Main (main) where

import Control.Monad.State (State, execState, get, put, modify)
import Data.Maybe          (mapMaybe)

-- A labeling operator.  See below.
infixr 0 -:

-- First define datatypes to capture the desired information of your DSL.
-- This most likely will look different than the end result, e.g. it will
-- contain address labels, where the end result program has them removed.

type AsmProgram = [AsmStmt]

data Label = Label Int deriving (Show, Eq)

data AsmStmt
  = AI1   Int Int
  | AI2
  | AI3   Int
  | AI4
  | AGoto Label
  | AMark Label  -- Marks an address point with a label.
  deriving Show


-- Now define a monad to capture the program information.
-- It's usually best to use the State monad.
-- In this case the state needs to hold the AsmProgram
-- and a rolling counter to create fresh (unique) labels.

type Asm = State (Int, AsmProgram)

-- A helper function to add new statements.
addStmt :: AsmStmt -> Asm ()
addStmt a = modify $ \ (n, p) -> (n, p ++ [a])

-- Now build up a library of combinators to serve as the language constructs.

i1 :: Int -> Int -> Asm ()
i1 a b = addStmt $ AI1 a b

i2 :: Asm ()
i2 = addStmt $ AI2

i3 :: Int -> Asm ()
i3 a = addStmt $ AI3 a

i4 :: Asm ()
i4 = addStmt $ AI4

-- | Create a fresh label.
label :: Asm Label
label = do
  (n, p) <- get
  put (n + 1, p)
  return $ Label n

-- | Marks an address location with a label.  A syntactic trick to make it look like an assembly language.
(-:) :: Label -> Asm a -> Asm a
a -: b = do
  addStmt $ AMark a
  b

goto :: Label -> Asm ()
goto a = addStmt $ AGoto a


-- Make a function to evaluate the DSL and return an AsmProgram.
assemble :: Asm () -> AsmProgram
assemble program = snd $ execState program (0, [])



-- Next you need a new set of datatypes that is closer to the machine
-- level, i.e. with labels replaced with addresses.

type MachineProgram = [MachineStmt]

data MachineStmt
  = MI1   Int Int
  | MI2
  | MI3   Int
  | MI4
  | MGoto Int  -- Note that AMarks have been removed and gotos have an absolute address instead of a label.
  deriving Show

-- A function to compute the size of an instruction.
size :: AsmStmt -> Int
size a = case a of
  AI1   _ _ -> 3
  AI2       -> 1
  AI3   _   -> 2
  AI4       -> 1
  AGoto _   -> 2
  AMark _   -> 0

-- Traverse an AsmProgram to map labels to addresses.
labelAddresses :: AsmProgram -> Label -> Int
labelAddresses program label = case lookup label addresses of
  Just a -> a
  Nothing -> error $ "Label not found: " ++ show label
  where
  addresses :: [(Label, Int)]
  addresses = snd $ foldl f (0, []) program
    where
    f :: (Int, [(Label, Int)]) -> AsmStmt -> (Int, [(Label, Int)])
    f (address, table) a = case a of
      AMark label -> (address, (label, address) : table)
      a -> (address + size a, table)

-- With the addresses known for labels, convert an AsmProgram into a MachineProgram.
elaborate :: AsmProgram -> MachineProgram
elaborate program = mapMaybe f program
  where
  labelAddress = labelAddresses program
  f :: AsmStmt -> Maybe MachineStmt
  f instr = case instr of
    AI1   a b -> Just $ MI1 a b
    AI2       -> Just $ MI2
    AI3   a   -> Just $ MI3 a
    AI4       -> Just $ MI4
    AGoto a   -> Just $ MGoto $ labelAddress a  -- Replace label references with addresses.
    AMark _   -> Nothing                        -- Throw away the address labels.
    

-- An example program.
testProgram :: Asm ()
testProgram = do
  begin <- label
  loop  <- label
  begin -: do
    i1 10 20
    i2
    i3 5
  loop -: do
    i4
    goto loop


-- Assemble the program and print it out.
main :: IO ()
main = do
  putStrLn ""
  putStrLn "The AsmProgram:"
  putStrLn ""
  mapM_ print asmProgram

  putStrLn ""
  putStrLn "The MachineProgram:"
  putStrLn ""
  mapM_ print machineProgram

  where
  asmProgram     = assemble  testProgram
  machineProgram = elaborate asmProgram

