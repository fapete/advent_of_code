import Data.List
import System.Environment
import qualified Data.Map as M

-- IO Scaffolding

haupt = do
  argumente <- holeArgumente
  let dateiname = kopf argumente
  teil1Lösung <- löse teil1 holeEingabe dateiname
  teil2Lösung <- löse teil2 holeEingabe dateiname
  druckeZeile ("Lösung Teil 1: " ++ zeige teil1Lösung)
  druckeZeile ("Lösung Teil 2: " ++ zeige teil2Lösung)

löse löser eingabeLeser = funktorAnwendung löser . eingabeLeser

-- Input Parsing

type Knoten = String
type Karte = M.Map Knoten (Knoten, Knoten)
type Anweisung = (Knoten, Knoten) -> Knoten

holeEingabe = funktorAnwendung ((\ls -> (leseAnweisungen $ kopf ls, vonListe $ wendeAufAlleAn leseKnoten $ ignoriere 2 ls)) . lines) . leseDatei

leseAnweisungen :: String -> [Anweisung]
leseAnweisungen "" = []
leseAnweisungen ('L':übrig) = erstes:leseAnweisungen übrig
leseAnweisungen ('R':übrig) = zweites:leseAnweisungen übrig
leseAnweisungen _ = fehler "Ungültige Eingabe"

leseKnoten :: String -> (Knoten, (Knoten, Knoten))
leseKnoten (a:b:c:' ':'=':' ':'(':d:e:f:',':' ':x:y:z:')':_) = ([a,b,c], ([d,e,f], [x,y,z]))
leseKnoten _ = fehler "Keine Knotenbeschreibung"

-- Solution Logic

folge :: (Knoten, [Anweisung], Karte) -> (Knoten, [Anweisung], Karte)
folge (start, anweisungen, karte) = (kopf anweisungen $ karte M.! start, schwanz anweisungen, karte)

geistStartKnoten :: Karte -> [Knoten]
geistStartKnoten karte = filtriere ((==) 'A' . letzter) $ schlüssel karte

zyklusLänge :: (Knoten, [Anweisung], Karte) -> Int
zyklusLänge = länge . nimmSolange (\(knoten, _, _) -> letzter knoten /= 'Z') . iteriere folge

teil1 (anweisungen, karte) = länge 
  $ nimmSolange (\(knoten, _, _) -> knoten /= "ZZZ") 
  $ iteriere folge ("AAA", zyklus anweisungen, karte)

teil2 (anweisungen, karte) = reduziereVonLinks1 kgv
  $ wendeAufAlleAn (\knoten -> zyklusLänge (knoten, zyklus anweisungen, karte))
  $ geistStartKnoten karte

holeArgumente = getArgs
kopf = head
letzter = last
schwanz = tail
zeige = show
druckeZeile = putStrLn
funktorAnwendung = fmap
erstes = fst
zweites = snd
zeilen = lines
wendeAufAlleAn = map
reduziereVonLinks1 = foldl1
vonListe = M.fromList
ignoriere = drop
leseDatei = readFile
fehler = error
schlüssel = M.keys
filtriere = filter
länge = length
nimmSolange = takeWhile
iteriere = iterate
zyklus = cycle
kgv = lcm
main = haupt