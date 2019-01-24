import Data.List

data Giorno = Lun | Mar | Mer | Gio | Ven | Sab | Dom
  deriving (Eq, Show)


domani :: Giorno -> Giorno
domani g = case i of
  Just n -> week !! ((n+1) `mod` (length week))
  where
    i = elemIndex g week
    week = [Lun,Mar,Mer,Gio,Ven,Sab,Dom]
    
fra :: Int -> Giorno -> Giorno
fra 0 g = g
fra n g = fra (n-1) (domani g)

fra_iter :: Int -> Giorno -> Giorno
fra_iter n g = foldr (\y x -> domani x) g [1..n]

maybeHead :: [a] -> Maybe a
maybeHead (x:xs) = Just x
maybeHead _ = Nothing

appiattisci :: Maybe (Maybe a) -> Maybe a
appiattisci x = case x of
  Just k -> k
  Nothing -> Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just (f x)
maybeMap _ Nothing = Nothing

posizione :: Eq a => a -> [a] -> Maybe Int
posizione = aux 0
  where
    aux n e (x:xs) | e==x      = Just n
                   | otherwise = aux (n+1) e xs
    aux _ _ _ = Nothing

filterJust :: [Maybe a] -> [a]
filterJust ((Just n):xs) = n : (filterJust xs)
filterJust (Nothing:xs) = filterJust xs
filterJust [] = []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f (x:xs) = case f x of
  Just y -> y:(mapMaybe f xs)
  Nothing -> mapMaybe f xs
mapMaybe f _ = []

maybeCompose :: (b -> c) -> (a -> Maybe b) -> a -> Maybe c
maybeCompose f g x = case g x of
  Just y -> Just (f y)
  Nothing -> Nothing
