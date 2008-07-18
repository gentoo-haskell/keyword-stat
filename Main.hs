module Main where

--import qualified Data.ByteString as B

import qualified Text.Regex.PCRE.Light.Char8 as R

import Data.List as L
import Control.Applicative
import Control.Monad
import System
import System.Directory

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

data Alignment
        = LeftAlign   {                   fromAlign :: Int }
        | CenterAlign { fillChar :: Char, fromAlign :: Int }
        | RightAlign  {                   fromAlign :: Int }

prettyColumns = LeftAlign 35 : map (\arch -> RightAlign (2 + length (fromArch arch))) arches
prettyHeader = "package" : map fromArch arches

main :: IO ()
main = do
    [arg,wd] <- getArgs
    packages <- lines <$> readFile arg
    let ebuilds = zip (map extractCPV packages) packages
    pretty prettyColumns prettyHeader
    pretty prettyColumns (map (map (const '-')) prettyHeader)

    forM_ ebuilds $ \(package, package_name) -> do
        let ebuild_file = wd </> cpvToEbuild package
        exists <- doesFileExist ebuild_file
        if not exists
         then doesNotExist package_name
         else printIt package_name ebuild_file
    where
    doesNotExist package_name = do
        let nums = map fromAlign (tail prettyColumns)
        let width = sum nums + length nums - 1 -- yes, magic, but it works
        pretty [head prettyColumns, CenterAlign '-' width] [package_name, " NOT FOUND "]
    printIt package_name ebuild_file = do
        content <- readFile ebuild_file
        let as = sort $ extractKeywords content
            arches0 = doit arches as
            doit (areal:arealrest) (a:arest) | sameArch areal a = a : doit arealrest arest
                                             | otherwise = toArch "" : doit arealrest (a:arest)
            doit [] [] = []
        pretty prettyColumns (package_name : map showArch arches0)


packageRegex = R.compile "^(.*)/(.*)-([\\d.]+)$" []
keywordRegex = R.compile "^KEYWORDS=\"(.*)\"$" [R.multiline]

pretty :: [Alignment] -> [String] -> IO ()
pretty padding text = do
    mapM_ (uncurry pretty') (L.intersperse (LeftAlign 1," ") (zip padding text))
    putStrLn ""
    where
    pretty' (LeftAlign p) t | p > 0 = do
                            putStr t
                            putStr $ replicate (p - length t) ' '
    pretty' (RightAlign p) t | p > 0 = do
                            putStr $ replicate (p - length t) ' '
                            putStr t
    pretty' (CenterAlign c p) t | p > 0 = do
                            let both = max 0 (p - length t)
                                left = both `div` 2
                                right = both - left
                            putStr $ replicate left c
                            putStr t
                            putStr $ replicate right c

extractCPV text = (c,p,v)
    where (Just [_,c,p,v]) = R.match packageRegex text []

extractKeywords text = map toArch $ words k
    where (Just [_,k]) = R.match keywordRegex text []

cpvToEbuild (c,p,v) = c </> p </> p <-> v ++ ".ebuild"

(</>) :: String -> String -> String
b </> n = b ++ '/':n

(<->) :: String -> String -> String
b <-> n = b ++ '-':n
