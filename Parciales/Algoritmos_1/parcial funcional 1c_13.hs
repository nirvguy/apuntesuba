type Nombre = String
type Momento = Int
data NotaMusical = Do | Re | Mi | Fa | Sol | La | Si deriving (Show,Eq)
type Partitura = [(Momento, NotaMusical)]

data Musico = M Nombre Partitura deriving Show
data Banda = Base | AgregarM Musico Banda deriving Show
data Ingrediente = Tomate | Jamon | Queso | Hamburgesa | Aderezo | Pan deriving (Eq,Show)
data MacSandwichito = Caja | Mc Ingrediente MacSandwichito deriving (Eq, Show)


mu1= M "cacho" [(1,Do),(2,Re),(4,Mi),(7,Do)]
mu2= M "tito" [(1,Mi),(3,Si),(4,Mi),(7,Do),(11,Sol)]
mu3= M "chongo" [(1,Do),(2,Re),(6,Mi),(7,Do)]
mu4= M "grasa" [(1,Do),(2,Re),(4,Mi),(6,Do)]
mu5= M "lechuga" [(1,Do),(2,Sol),(4,Mi),(5,Fa),(8,Fa),(9,Sol),(10,Si)]

b1 = AgregarM mu1 (AgregarM mu2 (AgregarM mu3 (AgregarM mu4 (AgregarM mu5 Base))))

apodoM :: Musico -> Nombre
apodoM (M n _) = n

servilletaM :: Musico -> Partitura
servilletaM (M _ p) = p

integrantesB :: Banda -> [Musico]
integrantesB Base = []
integrantesB (AgregarM m b) = (m:(integrantesB b))



--- eje 4


sacarN:: Int -> [Int] -> [Int]
sacarN _ [] = []
sacarN x (y:ys) 
		| x== y = sacarN x ys
		| otherwise = (y: (sacarN x ys))

subListaDeN:: Int -> [Int] ->[Int]
subListaDeN _ [] = []
subListaDeN x (y:ys) 
			|x==y = (y:(subListaDeN x ys))
			|otherwise = subListaDeN x ys
			
			
subListas:: [Int] -> [[Int]]
subListas [] = []
subListas (x:xs)  = ((subListaDeN x (x:xs)):( subListas (sacarN x xs)))

sLO::[[Int]] ->[[Int]]
sLO [] = []
sLO ((x:xs):ys)
	|x== minimo ((x:xs):ys) = ((x:xs):(sLO ys))
	| otherwise = sLO (ys ++ [(x:xs)])


minimo:: [[Int]] -> Int
minimo [(x:xs)] = x
minimo ((x:xs): ys)
		|x> (minimo ys) = minimo ys
		|otherwise = x
		
subListasOrdenadas :: [Int] -> [[Int]]
subListasOrdenadas xs = sLO (subListas xs)
		
-- Eje 3

m1= (Mc Pan (Mc Tomate (Mc Hamburgesa (Mc Tomate (Mc Pan Caja)))))
m2= (Mc Pan (Mc Tomate (Mc Hamburgesa (Mc Queso (Mc Pan Caja)))))

pisos:: MacSandwichito -> [Ingrediente]
pisos Caja = []
pisos (Mc i m ) = (i:(pisos m))

macCapicua:: MacSandwichito -> Bool
macCapicua m = (capicua m (length(pisos m)-1) 0)

capicua:: MacSandwichito -> Int -> Int -> Bool
capicua m a i
		|a== i = True
		|(pisos(m))!!(a-i) == (pisos(m))!!i = (capicua m a (i+1))
		|otherwise = False
		
		
		


--Eje 2
m3= (Mc Pan (Mc Tomate (Mc Hamburgesa (Mc Hamburgesa (Mc Queso ( Mc Tomate (Mc Tomate (Mc Pan Caja))))))))
m4= (Mc Pan (Mc Queso (Mc Hamburgesa (Mc Queso (Mc Pan Caja)))))

esCarne:: Ingrediente -> Bool
esCarne i = (i== Hamburgesa) || (i== Jamon) || (i== Pan)
 
esVegetariano:: Ingrediente -> Bool
esVegetariano i = (i== Tomate) || (i== Queso) || (i== Aderezo) || (i== Pan)

mcVegetariano :: MacSandwichito -> MacSandwichito
mcVegetariano Caja = Caja
mcVegetariano (Mc i m) 
			|esVegetariano i = (Mc i (mcVegetariano m))
			|otherwise = mcVegetariano m
			
mcCarne:: MacSandwichito -> MacSandwichito
mcCarne Caja = Caja
mcCarne (Mc i m)
		|esCarne i = (Mc i (mcCarne m))
		|otherwise = mcCarne m
		
		
sacarConsecutivos:: MacSandwichito -> MacSandwichito
sacarConsecutivos (Mc i (Mc i2 Caja))
					| i==i2 = (Mc i2 Caja)
					| otherwise = (Mc i (Mc i2 Caja))
sacarConsecutivos (Mc i (Mc i2 m))					
					| i== i2 = (sacarConsecutivos (Mc i2 m))
					| otherwise = (Mc i (sacarConsecutivos (Mc i2 m)))
					
uyParejaVegetariana:: MacSandwichito -> (MacSandwichito, MacSandwichito)
uyParejaVegetariana m = ((sacarConsecutivos (mcVegetariano m)), (sacarConsecutivos (mcCarne m)))
					 
--Eje 1
--a)
todosLosMomentos:: [Musico] -> [Momento]
todosLosMomentos [] = []
todosLosMomentos (x:xs) = (momentoM (servilletaM x)) ++ (todosLosMomentos xs)

momentoM :: Partitura -> [Momento]
momentoM  [] = []
momentoM  ((x,y):xs) = (x:(momentoM xs))

notasPorMomento:: Momento -> [Musico] -> [NotaMusical]
notasPorMomento _ [] = []
notasPorMomento n (x:xs) = sacarRepetidos((notasMxM n (servilletaM x) ++ (notasPorMomento n xs)))

notasMxM :: Momento -> Partitura -> [NotaMusical]
notasMxM _ [] = []
notasMxM n ((x,y):xs) 
		| n==x = (y:(notasMxM n xs))
		| otherwise = notasMxM n xs

sacarRepetidos:: Eq a => [a] -> [a]
sacarRepetidos [] = []
sacarRepetidos (x:xs)
			| x `elem`xs = sacarRepetidos xs
			| otherwise = (x: (sacarRepetidos xs))

piezaMusical:: Banda -> [(Momento, [NotaMusical])]
piezaMusical b = pMA b (sacarRepetidos (todosLosMomentos (integrantesB b)))					


pMA:: Banda -> [Momento] -> [(Momento, [NotaMusical])]
pMA _ [] = []
pMA b (x:xs)  = ((x, (notasPorMomento x (integrantesB b))): (pMA b xs ))
--b)


notasPorMomento2:: Momento -> [Musico] -> [NotaMusical]
notasPorMomento2 _ [] = []
notasPorMomento2 n (x:xs) = (notasMxM n (servilletaM x) ++ (notasPorMomento2 n xs))

soloMusical:: Banda -> Musico -> Partitura
soloMusical b m = sMA b (servilletaM m)

sMA :: Banda -> Partitura -> Partitura
sMA _ [] = []
sMA b ((x,y):xs)
		|length((notasPorMomento2 x (integrantesB b))) == 1 = ((x,y):(sMA b xs))
		|otherwise 										    = sMA b xs

--c)

jefeDeLaBarra :: Banda -> Musico
jefeDeLaBarra b = jefeA b (integrantesB b)

jefeA:: Banda -> [Musico] -> Musico
jefeA _ [x] = x
jefeA b (x:xs)
		| length(soloMusical b x) == maximoS b (integrantesB b) = x  
		| otherwise 						   					= jefeA b xs
		

maximoS:: Banda -> [Musico] -> Int
maximoS _ [] = 0
maximoS b (x:xs) 
		| length(soloMusical b x) < maximoS b xs = maximoS b xs
		| otherwise 							 = length(soloMusical b x)
		
		


					









