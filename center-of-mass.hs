
-- types for variables
type Mass = Double
type Pos = (Double, Double, Double)
type Obj = (Mass, Pos)

-- centerOfMass :: [Obj] -> Pos
-- -- centerOfMass objList = 

centerOfMass :: [Obj] -> [Double]
centerOfMass objs = [xval, yval, zval]
    where
        massList = map fst objs
        mass = sum massList
        xList = map first (map snd objs)
        yList = map second (map snd objs)
        zList = map third (map snd objs)
        xval = (sum $ zipWith (*) massList xList) / mass
        yval = (sum $ zipWith (*) massList yList) / mass
        zval = (sum $ zipWith (*) massList zList) / mass


-- Functions to access values in triplet
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z