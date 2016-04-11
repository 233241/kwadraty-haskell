-- Michal Przystupa 233241 zagadka nr 1: kwadraty

import Data.List	
import Control.Monad
import System.Environment

type Punkt = (Int,Int,Int)
type Kwadrat = (Int,Int,Int)

--funkcja glowna
--kolejne rozwiazania beda rozdzielone pusta linia
--oczywiscie jesli nie ma rozwiazan zostanie wypisana pusta linia
main :: IO ()
main = do
					--pobieranie danych
					argumenty <- getArgs
					dane <- readFile $ head argumenty
					--usuwanie niepozadanych znakow i podzial na odpowiednie slowa
					let wynik = lines $ filter (\x -> x/=' ' && x/='.') dane
					let xMax = read $ wynik!!0
					let yMax = read $ wynik!!1
					let punkty = read $ wynik!!2
					--wywolanie wlasciwego programu, posortowanie wynikow oraz wypisanie rozwiazania na wyjscie
					sequence_ $ map (putStrLn.(++"\n").show) $ sort $ map sort $ kwadraty (xMax,yMax) punkty
					
--program wlasciwy, generuje mozliwe rozwiazania i przetwarza je w petli
kwadraty :: (Int,Int) -> [Punkt] -> [[Kwadrat]]
kwadraty (xMax,yMax) punkty = petla $ map (gen (xMax,yMax) punkty) punkty

--generuje wszystkie mozliwe kwadraty dla danego punktu
gen :: (Int,Int) -> [Punkt] -> Punkt -> [Kwadrat]
gen (xMax,yMax) punkty (x,y,i) = 	do
										a <- delete x [1..xMax]
										b <- [x+y-a,a-x+y]
										guard $ 0<b && b<=yMax
										let [p,q] = sort[x,a]
										let [r,s] = sort[y,b]
										guard $ ile (p,r,q-p) punkty i
										return (p,r,q-p)

--buduje koncowe rozwiazania
petla :: [[Kwadrat]] -> [[Kwadrat]]
petla [] = return []
petla (m:ms) =	do
					x <- m
					--odrzuca wyniki ,ktore pokrywaja sie z elementem x
					--sortL optymalizuje dzialanie programu
					xs <- petla $ sortL $ map (filter $ not.pokrywaja x) ms
					return (x:xs)

--sprawdza czy wewnatrz kwadratu lezy dokladnie punktow
ile :: Kwadrat -> [Punkt] -> Int -> Bool
ile kwadrat punkty = (==).length.(filter id) $ map (zawiera kwadrat) punkty
	where 
		--sprawdza czy punkt zawiera sie w kwadracie
		zawiera :: Kwadrat -> Punkt -> Bool
		zawiera (a,b,dl) (x,y,_) = and[a<x,x<a+dl,b<y,y<b+dl]

--sprawdza czy 2 kwadraty pokrywaja sie w okreslony sposob
pokrywaja :: Kwadrat -> Kwadrat -> Bool
pokrywaja (a,b,dl_1) (x,y,dl_2) = or [b==y,b==y+dl_2,b+dl_1==y,b+dl_1==y+dl_2] && x<=a+dl_1 && x+dl_2>=a || or [a==x,a==x+dl_2,a+dl_1==x,a+dl_1==x+dl_2] && y<=b+dl_1 && y+dl_2>=b

--sortuje liste ze wgledu na dlugosc elementow (list)
--potrzebne do optymalizacji
sortL :: [[Kwadrat]] -> [[Kwadrat]]
sortL = sortBy (\xs ys -> compare (length xs) (length ys))
