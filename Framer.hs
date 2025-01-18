-- Framer.hs
--

module Main where 
import System.Environment
import Debug.Trace
import Codec.Picture
import Data.Array

data Frame = Frame
    { top :: Int
    , left :: Int
    , bottom :: Int
    , right :: Int
    } deriving (Show, Eq)

niptic :: Int -> Frame -> Int -> Int -> Int -> Int -> ((Int , Int), [Frame])
niptic count margin spacing hstep width height = let
    lm      = left margin
    rm      = right margin
    tm      = top margin
    bm      = bottom margin
    width'  = width - lm - rm
    height' = height - tm - bm
    fs      = (map (adjust lm tm) (niptic' count spacing width' height'))
    fs'     = calculateHeights hstep fs
    in ((width, height), (trace ("fs'" ++ show fs')) (fs'))

niptic' :: Int -> Int -> Int -> Int -> [Frame]
niptic' count spacing width height = let
    fw = (width - spacing * (count - 1)) `div` count
    fh = height
    in map (calculateFrame fw fh spacing) [0, 1 .. (count - 1)]

adjust :: Int -> Int -> Frame -> Frame
adjust l t f = Frame (t + top f) (l + left f) (t + bottom f) (l + right f)

calculateFrame w h s i = let
    top = 0
    left = i * (w + s)
    bottom = h
    right = left + w
    in Frame top left bottom right

generateFrame :: ((Int, Int), [Frame]) -> FilePath -> IO ()
generateFrame frames path = let
    ((w, h), fs) = frames
    pixArray = createPixelArray w h fs
    in writePng path (generateImage (\x y -> (pixArray ! (x, y))) w h)

createPixelArray w h fs = let
    indices = [(i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
    mappings = map (\i -> (i, PixelRGBA8 255 255 255 255)) indices
    seed = array ((0, 0), (w - 1, h - 1)) mappings
    in foldr (\curr accum -> accum // frameMappings curr) seed fs

frameMappings f = let
    tf = top f
    lf = left f
    bf = bottom f
    rf = right f
    indices = [(i, j) | i <- [lf .. rf - 1], j <- [tf .. bf - 1]]
    mapper i = (i, PixelRGBA8 255 255 255 0)
    in map mapper indices

calculateHeights hstep fs = let 
    l = length fs
    f i = abs (i - (l `div` 2))
    i = map f [0 .. l - 1]
    fis = zip fs i
    in map (changeHeight hstep) fis

changeHeight h fi = let
    h' = (snd fi) * h
    t  = top (fst fi) + h'
    l  = left (fst fi)
    b  = bottom (fst fi) - h'
    r  = right (fst fi)
    in Frame t l b r
