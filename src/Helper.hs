module Helper where

differ :: Eq a => [a] -> [a] -> [a]
-- differ from [] = from
-- differ from (x:xs)
--     | x `elem` from = differ (removeAll x from) xs
--     | otherwise = differ from xs

differ [] that = []
differ (x:xs) list
    | x `elem` list = differ xs list
    | otherwise = x : differ xs list

mapToSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapToSecond _ [] = []
mapToSecond f ((x', x''):xs) = (x', f x'') : mapToSecond f xs

joinDistinctLeft :: Eq a => [a] -> [a] -> [a]
joinDistinctLeft [] y = y
joinDistinctLeft (x:xs) y
    | x `elem` y = joinDistinctLeft xs y
    | otherwise = x : joinDistinctLeft xs y

findMaybe :: Eq a => a -> [(a, b)] -> Maybe b
findMaybe x [] = Nothing
findMaybe x ((k, v):rest) 
    | x == k = Just v
    | otherwise = findMaybe x rest

