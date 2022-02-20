module Helper where

import Data.Bifunctor (second)

-- this is available in Data.List, but use a Set instead
differ :: Eq a => [a] -> [a] -> [a]
-- differ from [] = from
-- differ from (x:xs)
--     | x `elem` from = differ (removeAll x from) xs
--     | otherwise = differ from xs

differ [] _ = []
differ (x : xs) list
  | x `elem` list = differ xs list
  | otherwise = x : differ xs list

-- this is fmap fmap
-- or map second, if you prefer
mapToSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapToSecond f = map (second f)

-- use Sets instead
-- also, this is just a filter
joinDistinctLeft :: Eq a => [a] -> [a] -> [a]
joinDistinctLeft [] y = y
joinDistinctLeft (x : xs) y
  | x `elem` y = joinDistinctLeft xs y
  | otherwise = x : joinDistinctLeft xs y

-- this is available as lookup, also, use a Map instead
findMaybe :: Eq a => a -> [(a, b)] -> Maybe b
findMaybe _ [] = Nothing
findMaybe x ((k, v) : rest)
  | x == k = Just v
  | otherwise = findMaybe x rest
