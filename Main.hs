module Main where

--import qualified Data.ByteString as B

import qualified Text.Regex.PCRE.Light.Char8 as R

import Data.List as L
import Control.Applicative
import Control.Monad
import System

data Arch = Stable String | Masked String deriving (Eq,Show)
fromArch (Stable a) = a
fromArch (Masked a) = a

toArch str =
    case str of
        ('~':arch) -> Masked arch
        _          -> Stable str

sameArch a b = (fromArch a) == (fromArch b)

showArch (Stable a) = a
showArch (Masked a) = '~' : a

instance Ord Arch where
    compare a b = compare (fromArch a) (fromArch b)

arches :: [Arch]
arches = map toArch . sort . words $ "alpha amd64 hppa ia64 ppc ppc64 sparc x86" 

prettyColumns = 35 : map (\arch -> - (2 + length (fromArch arch))) arches
prettyHeader = "package" : map fromArch arches

main :: IO ()
main = do
    [arg,wd] <- getArgs
    packages <- lines <$> readFile arg
    let ebuilds = map extractCPV packages
    pretty prettyColumns prettyHeader
    pretty prettyColumns (map (map (const '-')) prettyHeader)

    forM_ ebuilds $ \(package, package_name) -> do
        let ebuild_file = cpvToEbuild package
        content <- readFile (wd </> ebuild_file)
        let as = sort $ extractKeywords content
            arches0 = doit arches as
            doit (areal:arealrest) (a:arest) | sameArch areal a = a : doit arealrest arest
                                             | otherwise = toArch "" : doit arealrest (a:arest)
            doit [] [] = []
        pretty prettyColumns (package_name : map showArch arches0)

packageRegex = R.compile "^(.*)/(.*)-([\\d.]+)$" []
keywordRegex = R.compile "^KEYWORDS=\"(.*)\"$" [R.multiline]

pretty :: [Int] -> [String] -> IO ()
pretty padding text = do
    mapM_ (uncurry pretty') (L.intersperse (1," ") (zip padding text))
    putStrLn ""
    where
    pretty' p t | p > 0 = do
                    putStr t
                    putStr $ replicate (p - length t) ' '
                | otherwise = do
                    putStr $ replicate (abs p - length t) ' '
                    putStr t

extractCPV text = ((c,p,v), text)
    where (Just [_,c,p,v]) = R.match packageRegex text []

extractKeywords text = map toArch $ words k
    where (Just [_,k]) = R.match keywordRegex text []

cpvToEbuild (c,p,v) = c </> p </> p <-> v ++ ".ebuild"

(</>) :: String -> String -> String
b </> n = b ++ '/':n

(<->) :: String -> String -> String
b <-> n = b ++ '-':n
