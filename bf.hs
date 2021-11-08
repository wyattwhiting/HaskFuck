{-|
  Author: Wyatt D. Whiting
    Date: 20211107
    Desc: A naive implementation of BrainFuck in Haskell. This implementation is merely an interpreter, and performs no optimizations on the input program.

    TODO
    Mode to run the program where it draws inputs from a given string rather than from direct user input
    Script verifier -> check for balanced brackets before execution
    Refactor code to work with `MaybeT` monad transformer directly rather than with the `andThen` hack
    Version for compilation with GHC rather than staying with GHCi mode.
    Deal with return characters in program string
-}


import Control.Monad.Trans.Maybe
import Data.Char


-- The State of a BrainFuck machine is best represented as a left list, a current cell, and a right list
data BrainFuckState = BFS [Int] Int [Int] deriving Show


-- A BrainFuck program is made of commands. A NULL constructor encodes non-command characters which have no effect.
data Cmd = LEFT  -- '<'
         | RIGHT -- '>'
         | INC   -- '+'
         | DEC   -- '-'
         | IN    -- '.'
         | OUT   -- ','
         | LB    -- '['
         | LE    -- ']'
         | NULL  --  *
         deriving Show


-- Archtypical program for testing
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."


-- a script is a possibly-empty list of commands of arbitrary length
type Script = [Cmd]


-- the starting state of a BrainFuck machine is an empty tape 
start = BFS [] 0 []


{-|
    Then `andThen` function is an analog of `>>=` (monadic bind) for a `Maybe a` in the context of an IO monad
    thanks to ErikR on stackoverflow
-} 
andThen :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
andThen a b = runMaybeT $ MaybeT a >>= (\x -> MaybeT (b x))


-- Convert character to a BrainFuck command
charToCmd :: Char -> Cmd
charToCmd c = case c of '>' -> RIGHT
                        '<' -> LEFT
                        '+' -> INC
                        '-' -> DEC
                        '.' -> OUT
                        ',' -> IN
                        '[' -> LB
                        ']' -> LE
                        _   -> NULL

-- convert a program to a script, but point-free!
transString :: [Char] -> Script
transString = map charToCmd 


{-|
    The `runProgram` functon takes a list of characters and returns an IO monad contaning possibly a BrainFuckState
    The list of characters is first converted to a sequence of `Cmd`s, which is then applied to the initial BrainFuckState
-}
runProgram :: [Char] -> IO (Maybe BrainFuckState)
runProgram s = applySeq (transString s) start

-- The `getValAtPtr` function extracts the integer value of the cell currented pointed at in a BrainFuckState
getValAtPtr :: BrainFuckState -> Int
getValAtPtr (BFS _ r _) = r


{-|
    The `applySeq` function applies a Script to a BrainFuckState, returning an IO (Maybe BrainFuckState)
-}
applySeq :: Script -> BrainFuckState -> IO (Maybe BrainFuckState)
applySeq []         s   = return $ Just s               -- There are no more commands to apply, so just skip the state
applySeq (LB:ls)    s   = if   getValAtPtr s == 0       -- if value at data pointer is 0...
                          then applySeq rest s          -- then skip the loop entirely 
                                                        -- otherwise, apply to the code to be looped, then 
                                                        -- with that state, test the jump condition again
                          else (andThen (applySeq (tail loopBlock) s) (applySeq (LB:ls))) 
                              where loopBlock = getLoopSubScript (-1) (LB:ls)
                                    rest      = drop (length loopBlock) (LB:ls)
applySeq (l:ls)     s   = andThen (applyCmd l s) (applySeq ls)  -- in all other cases, apply the current command followed by the rest


{-|
    The `getLoopSubScript` function takes a script which is known to contain a looping section, and return just that looping section
    For example, the Script [NULL, LB, INC, INC, DEC, LE, NULL] becomes [LB, INC, INC, DEC, LE]
-}
getLoopSubScript :: Int -> Script -> Script
getLoopSubScript _ []   = []
getLoopSubScript 0 l    = case head l of --nonempty list
                            LE -> [LE]
                            LB -> (LB:(getLoopSubScript 1 (tail l)))
                            _  -> ((head l):(getLoopSubScript 0 (tail l)))
getLoopSubScript n l    = case head l of 
                            LE -> (LE:(getLoopSubScript (n - 1) (tail l)))
                            LB -> (LB:(getLoopSubScript (n + 1) (tail l)))
                            _  -> ((head l):(getLoopSubScript n (tail l)))



{-|
    The `applyCmd` function takes a command and BrainFuckState, and returns and IO Maybe BrainFuckState
    Essentially, if the command is valid, it gets wrapped in a Just constructor and returned in an IO.
    If the command is invalid, we can wrap Nothing in an IO and notify the user as a side effect. 
-}
applyCmd :: Cmd -> BrainFuckState -> IO (Maybe BrainFuckState)
applyCmd RIGHT (BFS l c [])     = return $
                                    Just $
                                        BFS (l ++ [c]) 0 []
applyCmd RIGHT (BFS l c (r:rs)) = return $
                                    Just $
                                        BFS (l ++ [c]) r rs
applyCmd LEFT  (BFS [] _ _)     = do
                                    putStrLn "ERROR: Ran out of leftward tape. Check your pointer arithmetic!"
                                    return $ Nothing --only place where this can fail :)
applyCmd LEFT  (BFS l c r)      = return $
                                    Just $
                                        BFS (init l) (last l) (c:r)
applyCmd INC   (BFS l c r)      = return $
                                    Just $
                                        BFS l (c + 1) r
applyCmd DEC   (BFS l c r)      = return $
                                    Just $
                                        BFS l (c - 1) r
applyCmd IN    s                = bindInputToState userInput s
applyCmd OUT   (BFS l c r)      = do
                                    putStr [chr c]
                                    return $ Just $ BFS l c r
applyCmd NULL  s                = return $ Just $ s
applyCmd _     s                = applyCmd NULL s


{-|
    `userInput` is an IO Monad containing an integer. 
    When evaluated, it returns an Int representing the first character of the user's input, interpreted as an ASCII character
-} 
userInput :: IO Int
userInput = do 
            putStr "Input: "
            x <- getLine
            return $ ord . head $ x


-- `bindInputToState` takes an IO Int and BrainFuckState, putting the Int into the state.
bindInputToState :: IO Int -> BrainFuckState -> IO (Maybe BrainFuckState)
bindInputToState n (BFS l _ r) = n >>= (\x -> return $ Just $ BFS l x r)