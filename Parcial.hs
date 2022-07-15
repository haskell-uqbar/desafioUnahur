-- 1
-- sumaOResta [(2,5),(3,7),(6,-2),(0,3)]
-- [7,-4,4,3]

sumaOResta:: [(Int, Int)] -> [Int]
sumaOResta = map sOR

sOR (x,y)
    | even x = x + y
    | otherwise = x - y

-- 2
-- mapMany [(+1),(*3)] [1,2,3]
-- [6,9,12]

mapMany funciones = map (foldl (.) id (reverse funciones))

mapMany' funciones = map (\x -> foldl (\e f -> f e) x funciones)
mapMany'' funciones = map (\x -> foldl (flip ($)) x funciones)

-- 3
-- sumaYProd [1,3,5]
-- (9,15)

sumaYProd::[Int]->(Int,Int)
sumaYProd = foldr (\e (x,y) -> (e + x, e * y)) (0,1)  

-- 4
-- esValida menosUnTercio
-- True
-- esPositiva menosUnTercio
-- False

data N = Uno | S N
data Z = Cero | Pos N | Neg N
data Q = Fraccion Z Z

menosUnTercio = Fraccion (Pos Uno) (Neg (S (S Uno)))

esValida (Fraccion denominador Cero) = False
esValida _ = True

esPositiva (Fraccion (Pos _) (Pos _)) = True
esPositiva (Fraccion (Neg _) (Neg _)) = True
esPositiva _ = False

-- 5
-- maximum unArbol
-- 10
-- maximum otroArbol
-- 8

data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a) (Arbol a)

foldrArbol:: ( a-> b-> b ) -> b -> Arbol a ->  b 
foldrArbol f semilla (Hoja elemento) = f x semilla 
foldrArbol f semilla (Nodo x arbol1 arbol2 arbol3) = 
    f x (foldrArbol f (foldrArbol f (foldrArbol f semilla arbol3) arbol2) arbol1) 

instance Foldable Arbol
    where foldr = foldrArbol

unArbol = Nodo 10 unaHoja otroArbol otraHoja
unaHoja = Hoja 3
otraHoja = Hoja 8
otroArbol = Nodo 5 unaHoja unaHoja otraHoja


--foldlArbol:: ( b-> a-> b ) -> b -> Arbol a ->  b 
--foldlArbol f semilla (Hoja x) = f semilla x
--foldlArbol f semilla (Nodo x arbol1 arbol2 arbol3) = 
--    foldlArbol f (foldlArbol f (foldlArbol f (f semilla x) arbol3) arbol2) arbol1




-- foldArbol (\x y -> max x (f (valor y)) ) (valor unArbol) unArbol





