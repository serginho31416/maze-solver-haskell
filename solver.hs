{- TIPOS DE DATOS PARA LABERINTOS -}

-- Tipo nodo con dos opciones, una como lo hemos dado en clase y otra que es más similar a lo de python y teoricamente funciona.
data Node = Node {
    state :: (Int, Int), --celda en la que estamos
    parent :: (Bool, Node), --celda de la que venimos
    action :: (Bool, String), --accion a tomar (es un movimiento "right", "left", "up", "down")
    cost :: Int
} deriving (Show)

--Necesito un nodo vacio para meterle al comienzo
emptyNode :: Node
emptyNode = Node { state = (-1, -1), parent = (False, emptyNode), action = (False, ""), cost = 0 }


-- Tipo laberinto, este es importante para poder despues dibujar las cosas

data Maze = Maze {
    height :: Int, --altura del laberinto
    width :: Int, --anchura del laberinto
    start :: (Int, Int), --celda inicial
    goal :: (Int, Int), -- celda final
    walls :: [[Bool]], -- es una lista de listas de booleanos, una por fila del laberinto que tiene False si en esa posicion (i,j) no hay pared y True si si la hay
    solution :: (Bool, [(Int, Int)]) --puede no haber solucion y la solucion es una lista de tuplas (celdas) que hay que ir recorrriendo para llegar
} deriving (Show)

{- FUNCIONES AUXILIARES PARA A* -}

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2) --distancia manhattan clasica

costHeuristic :: Maze -> Node -> Int --Calculamos h para cualquier nodo en cualquier laberinto usando métrica manhattan
costHeuristic maze node  = manhattan (state node) (goal maze)

totalCost :: Node -> Maze -> Int 
totalCost cell maze = (cost cell) + (costHeuristic maze cell)

{- AHORA VAMOS A DEFINIR EL TIPO FRONTERA Y LAS DISTINTAS FUNCIONES -}
type Frontier = [Node]

stackAdd :: Maze -> Node -> Frontier ->  Frontier 
stackAdd _ cell frontier = cell : frontier

queueAdd :: Maze -> Node -> Frontier -> Frontier 
queueAdd _ cell frontier = frontier ++ [cell]

astarAdd :: Maze -> Node -> Frontier -> Frontier 
astarAdd maze cell [] = [cell]
astarAdd maze cell (first:frontier)
    | (totalCost cell maze) < (totalCost first maze) = cell:first:frontier
    | otherwise = first : (astarAdd maze cell frontier)



{-VAMOS A POR LAS FUNCIONES QUE NOS DAN LOS POSIBLES MOVIMIENTOS (LAS CELDAS VECINAS QUE SON VALIDAS)-}

neighbors :: Maze -> (Int, Int) -> [(String, (Int, Int))] -- dado un laberinto y una celda me devuelve todos los posibles movimientos a aplicar a esa celda y devuelva una lista de tuplas tipo ("up", (1,2)), con el movimiento "up" nos vamos a la celda (1,2)
neighbors maze (row, col) = filter valid [ --filtro por las que sean validas
    ("up", (row - 1, col)),
    ("down", (row + 1, col)),
    ("left", (row, col - 1)),
    ("right", (row, col + 1))
    ]
  where
    valid (_, (r, c)) = r >= 0 && r < height maze && c >= 0 && c < width maze && not ((walls maze !! r) !! c) -- (walls maze !! r)!!c accede al elemento de la fila r y columna c de walls, si no hay pared esto vale False y por tanto not False=True
    --tengo que comprobar no salirme del laberinto
{-Voy a necesitar un tipo de datos que sea conjuntos, vamos a crearlo-}

data Set a = Empty | Root a (Set a) (Set a)  -- Raiz con un valor y dos subárboles (para optimizar las operacinoes scomo en edat)
           deriving (Show, Eq)
           
insert :: Ord a => a -> Set a -> Set a
insert x Empty = Root x Empty Empty
insert x (Root y left right)
  | x < y     = Root y (insert x left) right
  | x > y     = Root y left (insert x right)
  | otherwise = Root y left right  -- Si ya está, no se duplica
  
member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Root y left right)
  | x < y     = member x left
  | x > y     = member x right
  | otherwise = True
  
empty :: Set a
empty = Empty

{-Bien ya tenemos los conjuntos-}

{-FUNCIÓN GENERAL PARA RESOLVER-}
{-La funcion solve recibe como parámetros:
    Maze : laberinto
    Funcion: la funcion que añade elementos a la frontera determinada por el modo de resoucion
  Devuelve:
    Una tupla donde la primera componente es una tupla que contiene primero un verdadero o falso dependiendo de si hay o no solucion, en la segunda componente estaría la Solucion
        la segunda componente es el conjunto de nodos explorados-}

solve :: Maze -> (Maze -> Node -> Frontier -> Frontier) -> ((Bool, [(String, (Int,Int))]), Set (Int,Int))
solve maze f = search (f maze startNode []) f empty
    where
        startNode = Node { state = start maze , parent = (False, emptyNode), action = (False,""), cost = 0 }
        search :: Frontier -> (Maze -> Node -> Frontier -> Frontier) -> Set (Int, Int) -> ((Bool, [(String, (Int, Int))]), Set (Int,Int)) --puede no devolver nada, el conjunto es de celdas ya exploradas para no estar repitiendo
        search [] _ _ = ((False, []), empty)  --si no hay nadaa en la frontera a tomar por culo
        search (node:frontier) f explored
            | state node == goal maze = ((True, reconstructPath node), explored)  --si hemos llegado a la meta reconstruimos el camino
            | member (state node) explored = search frontier f explored --si ya lo hemos explorado continuamos con el resto de la frontera
            | otherwise = search newFrontier f (insert (state node) explored) --si no lo metemos en los explorados y actualizamos la frontera
          where
            newFrontier = foldr (f maze) frontier childNodes --añadimos todos los vecinos
            childNodes = [Node { state = s, parent = (True, node), action = (True, a), cost = ((cost node) +1) } 
                          | (a, s) <- neighbors maze (state node), not (member s explored)]
        
        reconstructPath :: Node -> [(String, (Int, Int))] --dado un nodo devuelve la lista de momvimientos para llegar hasta alli
        reconstructPath node = case parent node of
            (False,_) -> []
            (True, p) -> (snd(action node), state node) : reconstructPath p



printMaze :: Maze -> [(Int, Int)] -> IO ()
printMaze maze solution = putStrLn (unlines rows) 
  where
    rows = map makeRow [0 .. height maze - 1]
    makeRow i = [if (i, j) == start maze then 'A'
                 else if (i, j) == goal maze then 'B'
                 else if (walls maze !! i !! j) then '#'
                 else if (i, j) `elem` solution then '*' -- `elem` es una funcion que verifica si un elemento pertenece a una lsita
                 else ' ' | j <- [0 .. width maze - 1]]
      
writeMaze :: Maze -> [(Int, Int)] -> String 
writeMaze maze solution = unlines [printRow i | i <- [0 .. height maze - 1]] --hago un unlines para que sea un  unico string con los saltos de linea entre filas
  where
    printRow i = concatMap printCell [if (i, j) == start maze then 'A'
                                      else if (i, j) == goal maze then 'B'
                                      else if (walls maze !! i !! j) then '#'
                                      else if (i, j) `elem` solution then '*'
                                      else ' ' | j <- [0 .. width maze - 1]]
    printCell c
      | c == 'A'  = [c]  -- 'A' (start) 
      | c == 'B'  = [c]  -- 'B' (goal) tengo que hacer esta movida porque tengo que realmente pintar las cosas
      | c == '*'  = [c]  -- '*' (solution)
      | c == '#'  = [c]  -- '#' (wall)
      | otherwise = [c]  -- ' '
      
saveMaze :: String -> Maze -> [(Int,Int)] -> IO() --intentar quitar el maybe
saveMaze name maze solution = do
    let drawing = writeMaze maze solution 
    writeFile name drawing
    putStrLn ("Solucion guardada en: " ++ name)
      
-- Función auxiliar para descrifrar el laberinto, las paredes son paredes, la salida es 'A' y la meta es 'B'
decriptMaze :: [String] -> ((Int, Int), (Int, Int), [[Bool]])
decriptMaze rows = (start, goal, walls)
  where
    -- Encuentra las posiciones de inicio ('A') y meta ('B')
    -- Primero mapeamos cada fila con su indice (i,row) despuess cada caracter de cada fila con su indice y nos uedamos unicamente con aquel que coincida con el buscado
    -- Así formamos el indice (i,j) donde la i es el indice de finla y la j el de la columna o caracter
    findChar c = head [(i, j) | (i, row) <- zip [0..] rows, (j, cell) <- zip [0..] row, cell == c] --pongo el head por que necesito el elemento en si
    start = findChar 'A'
    goal = findChar 'B'
    -- Construye las paredes: '#' es True, todo lo demás es False
    walls = [[cell == '#' | cell <- row] ++ replicate (maxWidth - length row) False | row <- rows]
    maxWidth = maximum (map length rows)
    -- walls es una lista en la que sale True si la celda de la fila es # y False en caso contrario, despues añade los False
    -- que falten para completar el ancho del laberinto

-- Prueba con html

-- Convertir laberinto con colores a HTML
writeMazeToHTML :: Maze -> [(Int, Int)] -> Set (Int,Int) -> String
writeMazeToHTML maze solution explored = "<html><body><pre>" ++ unlines [printRow i | i <- [0 .. height maze - 1]] ++ "</pre></body></html>" --Lo meto en etiquetas html
  where
    printRow i = concatMap printCell [if (i, j) == start maze then "<span style='color:black; background-color:green;padding:5px; display:inline-block;'>A</span>"
                                      else if (i, j) == goal maze then "<span style='color:black; background-color:green;padding:5px; display:inline-block;'>B</span>"
                                      else if (walls maze !! i !! j) then "<span style='color:black; background-color:black;padding:5px; display:inline-block;'>#</span>"
                                      else if (i, j) `elem` solution then "<span style='color:blue; background-color:yellow;padding:5px; display:inline-block;'>*</span>"
                                      else if (member (i,j) explored) then "<span style='color:red; background-color:red;padding:5px; display:inline-block;'>·</span>"
                                      else "<span style='background-color:gray;padding:5px; display:inline-block;'> </span>" | j <- [0 .. width maze - 1]]

    printCell c = c

-- Guardar el laberinto con la solución en un archivo HTML
saveMazeToHTML :: String -> Maze -> [(Int, Int)] -> Set (Int,Int) -> IO () --intentar quitar el maybe
saveMazeToHTML filename maze solution explored = do
    let mazeHTML = writeMazeToHTML maze solution explored--lo convierto
    writeFile filename mazeHTML --lo guardo
    putStrLn ("Laberinto con solución guardado en HTML: " ++ filename)
    
play :: IO()
play = do 
    putStr "Dame el nombre del fichero del laberinto: "
    game <- getLine
    putStrLn " "
    content <- readFile game
    let rows = lines content --el laberinto va por filas
        height = length rows --el alto es la cantidad de filas
        width = maximum (map length rows) --el ancho será el máximo del largo de las filas
        (start, goal, walls) = decriptMaze rows
        maze = Maze { height = height, width = width, start = start, goal = goal, walls = walls, solution = (False, []) }
    printMaze maze [] --sin solución lista vacía 
    putStrLn " "
    putStrLn "¿Qué tipo de búsqueda quieres usar?"
    putStrLn "1. Búsqueda en anchura BFS (QueueFrontier)"
    putStrLn "2. Búsqueda en profundidad DFS (StackFrontier)"
    putStrLn "3. Búsqueda Greedy A* (A*Frontier)"
    putStr "Elige una opción (1, 2 o 3): "
    mode <- getLine
    let solutionfull = case mode of
            "1" -> solve maze queueAdd -- Si elige "1", usa la función de BFS
            "2" -> solve maze stackAdd -- Si elige "2", usa la función de DFS
            "3" -> solve maze astarAdd -- Si elige "3", usa la función de A*
            _   -> ((False, []), empty) -- Si la opción no es válida, no hay solución
    
    if fst (fst solutionfull) then do
                                    let solution = map snd (snd (fst solutionfull))
                                    let visited = snd solutionfull
                                    putStrLn " "
                                    putStrLn "¡Tengo una solución!"
                                    putStrLn " "
                                    printMaze maze solution
                                    putStrLn " "
                                    
                                    putStrLn "¿Quieres guardar la solucion?"
                                    putStrLn "1. Sí, en formato texto"
                                    putStrLn "2. Sí, en formato HTML"
                                    putStrLn "3. No deseo guardar la solución"
                                    putStr "Escribe tu opción (1, 2 o 3): "
                                    decision <- getLine
                                    case decision of
                                        "1" -> do
                                            putStr "Escriba el nombre del fichero para guardar la solucion (con extensión .txt): "
                                            name <- getLine 
                                            saveMaze name maze solution 
                                        "2" -> do 
                                            putStr "Escriba el nombre del fichero para guardar la solucion (con extensión .html): "
                                            name <- getLine 
                                            putStrLn "¿Quieres ver los caminos explorados?"
                                            putStrLn "1. Sí"
                                            putStrLn "2. No"
                                            putStr "Escribe tu opción (1 o 2): "
                                            see <- getLine
                                            case see of
                                                "1" -> do
                                                    saveMazeToHTML name maze solution visited
                                                "2" -> do
                                                    saveMazeToHTML name maze solution empty
                                        "3" -> putStrLn "Genial! Ha sido un placer"
                                        _ -> putStrLn "Opción no válida, no se guardó la solución."
        else putStrLn " No he encontrado ninguna solución para este laberinto, lo siento :("