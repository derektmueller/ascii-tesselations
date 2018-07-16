module Lib where

import Safe
import Data.List as L
import Data.Map as M

data Rotation = Ninety | Zero | NegativeNinety | OneEighty deriving (Show, Eq, Ord)
type Width = Int
type Height = Int

type AsciiRotationMap = Map Char (Map Rotation Char)

rotationMap = fromList [
    ('-', fromList [(Ninety, '|'), (NegativeNinety, '|')]),
    ('|', fromList [(Ninety, '_'), (NegativeNinety, '_')]),
    ('/', fromList [(Ninety, '\\'), (NegativeNinety, '\\')]),
    ('\\', fromList [(Ninety, '/'), (NegativeNinety, '/')]),
    ('>', fromList [
      (Ninety, 'v'), 
      (NegativeNinety, '^'),
      (OneEighty, '<')
      ]),
    ('v', fromList [
      (Ninety, '<'), 
      (NegativeNinety, '>'),
      (OneEighty, '^')
      ]),
    ('^', fromList [
      (Ninety, '>'), 
      (NegativeNinety, '<'),
      (OneEighty, 'v')
      ]),
    ('<', fromList [
      (Ninety, '^'), 
      (NegativeNinety, 'v'),
      (OneEighty, '>')
      ])
  ] :: AsciiRotationMap

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose.reverse

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse.transpose

rotateTile :: Rotation -> [String] -> [String]
rotateTile Ninety = rotateRight.((fmap.fmap) $ rotateCharacter Ninety)
rotateTile Zero = id
rotateTile NegativeNinety = rotateLeft.((fmap.fmap) $ rotateCharacter NegativeNinety)
rotateTile OneEighty = rotateRight.rotateRight.((fmap.fmap) $ rotateCharacter OneEighty)

rotateCharacter :: Rotation -> Char -> Char
rotateCharacter r c = maybe c id $ do
  charMap <- M.lookup c rotationMap
  rotated <- M.lookup r charMap
  return rotated

rotationCycle :: Rotation -> [Rotation]
rotationCycle Zero = cycle [Zero]
rotationCycle Ninety = cycle [Zero, Ninety, OneEighty, NegativeNinety]
rotationCycle OneEighty = cycle [Zero, OneEighty]
rotationCycle NegativeNinety = cycle [Zero, NegativeNinety, OneEighty, Ninety]

flattenTileMatrix :: Int -> Width -> Height -> [[String]] -> [String]
flattenTileMatrix tileW w h matrix = do
  tileRow <- fmap getTileRow [0..(h - 1)]
  getRowN <- fmap getRow [0..(tileW - 1)]
  return $ concat $ fmap getRowN tileRow
  where
    getTileRow n = L.take w $ L.drop (n * w) matrix
    getRow n arr = head $ L.drop n arr

tesselate :: Width -> Height -> Rotation -> [String] -> [String]
tesselate w h r tile = flattenTileMatrix tileW w h tileMatrix
  where
    sequence = fmap (\r -> rotateTile r tile) (rotationCycle r)
    row n = L.take w $ L.drop n sequence
    tileMatrix = concat $ L.take h $ fmap row [0..]
    tileW = length tile

tesselate2x2 :: Rotation -> [String] -> [String]
tesselate2x2 = tesselate 2 2

