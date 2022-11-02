import System.IO.Error
import System.IO

type Q  = String

data QA = Person String | Question QA Q QA deriving (Show, Read)

getTree :: FilePath -> IO QA
getTree n = do
    tree <- readFile n
    return (read tree :: QA)

readQAFile :: IO QA
readQAFile = do
    x <- tryIOError (getTree "famous.qa")
    case x of
        Right a -> return a 
        Left  _ -> return defaultTree

defaultTree :: QA
defaultTree = Question
    (Question (Person "Marie Curie")    "Is this person a scientist?"
    (Question (Person "Therese May")    "Is this person a politician?"
    (Person "Queen Elizabeth")))        "Is this person from Europe?"
    (Question (Person "Marilyn Monroe") "Is this person an actress?"
    (Person "Hillary Clinton"))

question :: String -> IO String
question s = do
    putStrLn s
    hFlush stdout
    getLine

yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
    answer <- question s
    case answer of
        "yes" -> return True
        "no"  -> return False
        _     -> do
            putStrLn "Please answer yes or no!"
            hFlush stdout
            yesNoQuestion s

play :: QA -> IO QA
play (Person p) = do 
    ansBool <- yesNoQuestion ("My guess: Is it " ++ p ++ "?")
    case ansBool of 
        True -> do 
            putStrLn "OK - I won this time."
            hFlush stdout
            return (Person p)
        False -> do 
            putStrLn "OK - You won this time."
            hFlush stdout
            putStrLn "Just curious: Who was your famous person?"
            hFlush stdout
            newP <- getLine
            putStrLn ("Give me a question for which the answer for " ++ newP ++  " is yes and the answer for " ++ p ++  " is no:")
            hFlush stdout
            newQ <- getLine
            return (Question (Person newP) newQ (Person p))
play (Question left q right) = do 
    answer <- yesNoQuestion q
    case answer of
        True -> do
            leftTree <- play left
            return (Question leftTree q right)
        False -> do
            rigthTree <- play right
            return (Question left q rigthTree)

main :: IO ()
main = do
    putStrLn "Think of a famous person! I will ask you questions about this person."
    hFlush stdout
    tree <- readQAFile
    newTree <- play tree
    writeFile "famous.qa" (show newTree)
