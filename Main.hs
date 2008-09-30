module Main where

--import qualified Data.ByteString as B

import qualified Text.Regex.PCRE.Light.Char8 as R

import Data.List as L
import Control.Applicative
import Control.Monad
import System
import System.Directory

main :: IO ()
main = do
    args <- getArgs
    let (packages_file, wd) =
            case args of
                []      -> (Nothing, ".")
                [wd]    -> (Nothing, wd)
                [pf,wd] -> (Just pf, wd)
    packages <-
        case packages_file of
            Just pf -> filter (not . L.isPrefixOf "#") . lines <$> readFile pf
            Nothing -> findPackages wd
    let ebuilds = zip (map extractCPVR packages) packages
    pretty prettyColumns prettyHeader
    pretty prettyColumns (map (\c -> replicate (fromAlign c) '-') prettyColumns)

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
            doit _ _ = []
        pretty prettyColumns (package_name : map showArch arches0)

findPackages :: FilePath -> IO [String]
findPackages portdir = do
    categories <- sort <$> getDirectories portdir
    concat <$> (forM categories $ \cat -> do
        packages <- sort <$> getDirectories (portdir </> cat)
        concat <$> (forM packages $ \pkg -> do
            files <- sort <$> getDirectoryContents (portdir </> cat </> pkg)
            return [ cat </> (reverse (drop 7 (reverse file)))
                   | file <- files
                   , ".ebuild" `L.isSuffixOf` file
                   ]))
    where
    getDirectories :: FilePath -> IO [String]
    getDirectories fp = do
        files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fp
        filterM (doesDirectoryExist . (fp </>)) files

packageRegex = R.compile "^(.*)/(.*?)-([\\d.]+)([-_].*?)?$" []
keywordRegex = R.compile "^KEYWORDS=\"(.*)\".*" [R.multiline]
versionRegex name = R.compile ("^" ++ name ++ "-(.*).ebuild$") []

extractCPVR_m text =
    case R.match packageRegex text [] of
        Just [_,c,p,v] -> Just (c,p,v,"")
        Just [_,c,p,v,r] -> Just (c,p,v,r)
        Nothing -> Nothing

extractCPVR text =
    case extractCPVR_m text of
        Just x -> x
        Nothing -> error text

extractKeywords text = map toArch $ words k
    where (Just [_,k]) = R.match keywordRegex text []

cpvToEbuild (c,p,v,r) = c </> p </> p <-> v ++ r ++ ".ebuild"

(</>) :: String -> String -> String
b </> n = b ++ '/':n

(<->) :: String -> String -> String
b <-> n = b ++ '-':n

-----------------------------------------------------------------------
-- Arches
-----------------------------------------------------------------------

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

-- the arches where we have ghc
arches :: [Arch]
arches = map toArch . sort . words $ "alpha amd64 hppa ia64 ppc ppc64 sparc x86" 


-----------------------------------------------------------------------
-- Pretty columns
-----------------------------------------------------------------------

data Alignment
        = LeftAlign   {                   fromAlign :: Int }
        | CenterAlign { fillChar :: Char, fromAlign :: Int }
        | RightAlign  {                   fromAlign :: Int }

prettyColumns = LeftAlign 35 : map (\arch -> RightAlign (2 + length (fromArch arch))) arches
prettyHeader = "package" : map fromArch arches

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
