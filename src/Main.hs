module Main where
import qualified Data.Map.Strict as M

entities:: (M.Map (String, String, String) String)
entities = M.singleton ("users", "_id", "1") "Kittens"

data AppState = File | Field String | Value String String
data Output = Silent AppState | Output AppState String

prompts:: AppState -> String
prompts File = "Which file would you like to search?"
prompts (Field _) = "Which field should we look in?"
prompts (Value _ _) = "What's the value you'd like?"

tick:: AppState -> String -> Output
tick File file = Silent (Field file)
tick (Field file) field = Silent (Value file field)
tick (Value file field) value = Output File "<output>"

exitableRepl:: AppState -> IO ()
exitableRepl state = do
    putStrLn (prompts state)
    line <- getLine
    case line of
      "exit" -> putStrLn "Goodbye!"
      text -> case (tick state text) of
          Silent state -> exitableRepl state
          Output state output -> do
            putStrLn output
            exitableRepl state

main:: IO ()
main = do
  exitableRepl File
