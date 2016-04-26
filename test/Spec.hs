import DependencyGraph

main :: IO ()
main = putStrLn "Graph from simple.txt:" >> graphFromFile "test/simple.txt"
