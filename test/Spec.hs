import DependencyGraph

main :: IO ()
main = do
    putStrLn "Graph from simple.txt:"
    graphFromFile "test/simple.txt" >>= putStrLn
