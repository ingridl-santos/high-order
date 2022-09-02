-- Ingrid Lima dos Santos

-- q1. Escreva uma função chamada fatorialn que usando operador range e a função foldr devolva o fatorial de n.
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1 .. n]

-- q2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos inteiros listados.
quadradoReal :: [Float] -> [Float]
quadradoReal n = map quadrado n
  where
    quadrado x = x * x

-- q3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map length

-- q4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter (\x -> x `mod` 29 == 0) [1 .. 100000])

-- q5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum (filter (\x -> x `mod` n == 0) [1 .. 100000])

-- q6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1² +2² +3² +4²...+𝑛².
somaQuadrados :: [Int] -> Int
somaQuadrados n = foldl (+) 0 (map (^ 2) n)

-- q7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
comprimento :: [a] -> Int
comprimento = foldl (\n _ -> n + 1) 0

-- q8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
exCurry = curry (\(x, y) -> 2 * x + y)

exUncurry = uncurry (*)

main = do
  putStrLn $ "\nFunc. 1: entrada: 7; resultado: " ++ show (fatorialn 7)
  putStrLn $ "\nFunc. 2: entrada: [-1, 3, -4, 5]; resultado: " ++ show (quadradoReal [-1, 3, -4, 5])
  putStrLn $ "\nFunc. 3: entrada: ['hello', 'world', 'haskell', 'python']; resultado: " ++ show (comprimentoPalavras ["hello", "world", "haskell", "python"])
  putStrLn $ "\nFunc. 4: entrada: [1 .. 100000]; resultado: " ++ show maiorMultiploDe29
  putStrLn $ "\nFunc. 5: entrada: 3; resultado: " ++ show (maiorMultiploDe 3)
  putStrLn $ "\nFunc. 6: entrada: [1, 2, 3, 4]; resultado: " ++ show (somaQuadrados [1, 2, 3, 4])
  putStrLn $ "\nFunc. 7: entrada: [a, ab, help]; resultado: " ++ show (comprimento ["a", "ab", "help"])
  putStrLn $ "\nFunc. 8: entrada: 1 2; resultado: " ++ show (flip (/) 1 2)
  putStrLn $ "\nFunc. 8: entrada: 3 6; resultado: " ++ show (flip mod 3 12)
  putStrLn $ "\nFunc. 8: entrada: 13; resultado: " ++ show (odd 13)
  putStrLn $ "\nFunc. 8: entrada: 12; resultado: " ++ show (odd 12)
  putStrLn $ "\nFunc. 8: entrada: 10 3; resultado: " ++ show (max 10 3)
  putStrLn $ "\nFunc. 8: entrada: 1 2; resultado: " ++ show (max 1 2)
  putStrLn $ "\nFunc. 8: entrada: 10 3; resultado: " ++ show (min 10 3)
  putStrLn $ "\nFunc. 8: entrada: 1 2; resultado: " ++ show (min 1 2)
  putStrLn $ "\nFunc. 8: entrada: 2 3; resultado: " ++ show (curry fst 2 3)
  putStrLn $ "\nFunc. 8: entrada: 4 3; resultado: " ++ show (exCurry 4 3)
  putStrLn $ "\nFunc. 8: entrada: 5 4; resultado: " ++ show (uncurry mod (5, 4))
  putStrLn $ "\nFunc. 8: entrada: 3 2; resultado: " ++ show (exUncurry (3, 2))