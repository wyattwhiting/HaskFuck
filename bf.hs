import Control.Monad.Trans.Maybe
import Data.Char

data BrainFuckState = BFS [Int] Int [Int] deriving Show
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


helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

type Script = [Cmd]
validChars = "<>+-,.[]"

start = BFS [] 0 []

-- thanks to ErikR on stackoverflow
andThen :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
andThen a b = runMaybeT $ MaybeT a >>= (\x -> MaybeT (b x))

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

--parser for scripts 
transString :: [Char] -> Script
transString = map charToCmd 


-- main function: fundamentally runs *ONCE*
-- it takes a script and runs it. That's it
runProgram :: [Char] -> IO (Maybe BrainFuckState)
runProgram s = applySeq (transString s) start

-- extract the value at the data pointer from a BrainFuckState
getValAtPtr :: BrainFuckState -> Int
getValAtPtr (BFS _ r _) = r


-- apply a script to a state, including loops
applySeq :: Script -> BrainFuckState -> IO (Maybe BrainFuckState)
applySeq []         s   = return $ Just s --case where the program terminates
applySeq (LB:ls)    s   = if   getValAtPtr s == 0 --if value at data pointer is 0, skip the loop entirely
                          then applySeq rest s
                          else (andThen (applySeq (tail loopBlock) s) (applySeq (LB:ls)))
                              where loopBlock = getLoopSubScript (-1) (LB:ls)
                                    rest      = drop (length loopBlock) (LB:ls)
applySeq (l:ls)     s   = andThen (applyCmd l s) (applySeq ls)



--given a script array starting with LB, return the script only between that
--LB and the matching LE
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

-- take user input from terminal, return an IO Int with the ascii value for the first character of the user's input
userInput :: IO Int
userInput = do 
            putStr "Input: "
            x <- getLine
            return $ ord . head $ x

bindInputToState :: IO Int -> BrainFuckState -> IO (Maybe BrainFuckState)
bindInputToState n (BFS l _ r) = n >>= (\x -> return $ Just $ BFS l x r)