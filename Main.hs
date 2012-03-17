module Main ( main ) where

--import qualified Data.ByteString as B

import qualified Text.Regex.PCRE.Light.Char8 as R

import Data.List as L
import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath ( splitDirectories )
import System.IO.Unsafe ( unsafeInterleaveIO )

main :: IO ()
main = do
    args <- getArgs
    let (packages_file, wd) =
            case args of
                []      -> (Nothing, ".")
                [wd]    -> (Nothing, wd)
                [pf,wd] -> (Just pf, wd)
    ebuilds <-
        case packages_file of
            Just pf -> do pkgs <- filter (not . L.isPrefixOf "#") . lines <$> readFile pf
			  let ps = map extractCPVR_pkgLine pkgs
			      name (cat,pkg,ver) = cat </> pkg <-> ver
			      path (cat,pkg,ver) = cat </> pkg </> pkg <-> ver
                          return $ zip (map name ps) (map path ps)
            Nothing -> do pkgs <- findPackages wd
                          return $ zip (map extractCPVR pkgs) pkgs
    pretty prettyColumns prettyHeader
    pretty prettyColumns (map (\c -> replicate (fromAlign c) '-') prettyColumns)

    forM_ ebuilds $ \(package_name, package_path) -> do
        let ebuild_file = package_path ++ ".ebuild"
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
        let as = [ (fromArch a, a) | a <- extractKeywords content ]
            arches0 = doit (map fromArch arches)
            doit (areal:arealrest) | Just a <- lookup areal as = a : doit arealrest
                                   | otherwise = toArch "" : doit arealrest
            doit _ = []
        pretty prettyColumns (package_name : map showArch arches0)

findPackages :: FilePath -> IO [FilePath]
findPackages dir = do
    (dirs,files) <- getContent dir
    let locals = sort (cwdEbuilds files)
    rec <- concat <$> mapM (unsafeInterleaveIO . findPackages) dirs
    return (locals ++ rec)
    where
    cwdEbuilds :: [FilePath] -> [FilePath]
    cwdEbuilds files =
            [ reverse (drop 7 (reverse file))
            | file <- files
            , ".ebuild" `L.isSuffixOf` file
            ]
    getContent :: FilePath -> IO ([FilePath], [FilePath])
    getContent fp = do
        items0 <- filter (`notElem` [".", "..", "_darcs"]) <$> getDirectoryContents fp
        let items = map (fp </>) items0
        dirs <- filterM doesDirectoryExist items
        files <- filterM doesFileExist items
        return (dirs,files)

packageRegex = R.compile "^(.*)/(.*?)-([\\d.]+)([-_].*?)?$" []
keywordRegex = R.compile "^KEYWORDS=\"(.*)\".*" [R.multiline]
versionRegex name = R.compile ("^" ++ name ++ "-(.*).ebuild$") []

extractCPVR_pkgLine pkg_line =
    case R.match packageRegex pkg_line [] of
      Just [_, cat,pkg,ver] -> (cat, pkg, ver)
      Just [_, cat,pkg,ver,suf] -> (cat, pkg, ver ++ suf)
      x -> error (show x)
 
extractCPVR_m text =
    case reverse (splitDirectories text) of
      -- ../gentoo-haskell/x11-wm/xmonad/xmonad-0.8
      -- ( xmonad-0.8 : xmonad : x11-wm : .. : .. )
      (pvr:_package:category:_:_) -> Just (category </> pvr)
      -- ./xmonad/xmonad-0.8
      -- [ xmonad-0.8 , xmonad , . ]
      [pvr, _package, _] -> Just pvr
      -- ./xmonad-0.8
      -- [ xmonad-0.8 , . ]
      [pvr, _] -> Just pvr
      -- ??
      x -> Just ("?:" ++ text)

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

data Arch
  = Stable String
  | Masked String
  | Unavailable String deriving (Eq,Show)
fromArch (Stable a) = a
fromArch (Masked a) = a
fromArch (Unavailable a) = a

toArch str =
    case str of
        ('~':arch) -> Masked arch
        ('-':arch) -> Unavailable arch
        _          -> Stable str

sameArch a b = (fromArch a) == (fromArch b)

showArch (Stable a) = a
showArch (Masked a) = '~' : a
showArch (Unavailable a) = '-' : a

instance Ord Arch where
    compare a b = compare (fromArch a) (fromArch b)

-- the arches where we have ghc
arches :: [Arch]
arches = map toArch . sort . words $ "alpha amd64 ia64 ppc ppc64 sparc x86 x86-fbsd"


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
