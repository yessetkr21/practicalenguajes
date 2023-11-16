main :: IO ()
main = do
    putStrLn "Ingrese el código ISBN de 10 dígitos:"
    numeros <- pedirNumeros
    esValido <- funcionISBN numeros
    if esValido
        then putStrLn "Es un código ISBN válido."
        else putStrLn "No es un código ISBN válido. Revise si escribió el código correctamente e inténtelo de nuevo."
pedirNumeros :: IO [Int]
pedirNumeros = do
    entrada <- getLine
    let numerosStr = funcionSeparadora entrada
    let numeros = convertirLista numerosStr

    if length numeros == 10
        then return numeros
        else do
            putStrLn "Debe ingresar exactamente 10 números para que el código sea correcto."
            return []
funcionSeparadora :: String -> [String]
funcionSeparadora = filtrarLista (\c -> c == '-')

filtrarLista :: (Char -> Bool) -> String -> [String]
filtrarLista p s = case dropWhile p s of
    "" -> []
    s' -> w : filtrarLista p s''
        where (w, s'') = break p s'

funcionISBN :: [Int] -> IO Bool
funcionISBN numeros = do
    if length numeros == 10
        then do
            let listaPre = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
            let resultado = zipWith (*) numeros listaPre
            funcionValidadora resultado
        else do
            putStrLn "Debe ingresar exactamente 10 números para que el código sea correcto."
            return False

funcionValidadora :: [Int] -> IO Bool
funcionValidadora resultado = do
    let suma = sum resultado
    return (suma `mod` 11 == 0)
convertirLista :: [String] -> [Int]
convertirLista = map read . filter todosSonDigitos
todosSonDigitos :: String -> Bool
todosSonDigitos str = all esDigito str

esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'
