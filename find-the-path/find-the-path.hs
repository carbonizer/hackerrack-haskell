import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Sequence ((<|),(><),(|>))
import System.CPUTime
import System.IO


-- A location on the grid which is a row/column pair
type Loc = (Int, Int)

-- A Path is a series of Locs and has a weight which is the sum of the weights
-- of each location.
data Path = Path {
    locs :: Seq.Seq Loc,
    weight :: Int
} deriving (Show)

-- Tried MinWeights as IntMap Int, but it was slower
type MinWeights = Seq.Seq Int


-- Get n lines of stdin
getNLines :: Int -> IO [String]
getNLines n
    | n == 0 = return []
    | otherwise = do
        line <- getLine
        others <- getNLines (n-1)
        return (line:others)
        

-- Convert a string of space delimited Ints to a list of Ints
lineToInts :: (String -> [Int])
lineToInts = map (read :: String -> Int) . words


-- Split a sequence into a sequence of n-length sub-sequences
splitEvery :: Int -> Seq.Seq Loc -> Seq.Seq (Seq.Seq Loc)
splitEvery n s
        | Seq.length s == 0 = Seq.empty
        | otherwise = a <| splitEvery n b
    where
        (a,b) = Seq.splitAt n s


-- Create a string representation of a Path on the grid
showPath :: (Int,Int) -> Loc -> Loc -> Path -> String
showPath tableSize firstLoc finalLoc p = unlines
        . Foldable.toList
        . fmap
            (unwords
                . Foldable.toList
                . fmap (\ loc ->
                    case () of
                        _ | loc == firstLoc -> "O"
                          | loc == finalLoc -> "X"
                          | loc `Foldable.elem` pLocs -> "*"
                          | otherwise -> "."
                    )
                )
        $ splitEvery m allLocs
    where
        (n,m) = tableSize
        allLocs = Seq.fromList [(r,c) | r <- [0..(n-1)], c <- [0..(m-1)]]
        pLocs = locs p


main :: IO ()
main = do
        nMLine <- getLine
        let [n, m] = lineToInts nMLine
        weightLines <- getNLines n
        let weights = listArray (0,n*m - 1) (concatMap lineToInts weightLines)
        _ <- getLine
        processQueries n m weights
    where
    -- Recursively process the quieries that make up the rest of the input
    processQueries :: Int -> Int -> Array Int Int -> IO ()
    processQueries n m weights = do
        isEOF' <- isEOF
        unless isEOF' (do
            query <- getLine
            -- Create first and last location from the query
            let (firstLoc,finalLoc) = (\[a,b,c,d] -> ((a,b),(c,d)))
                    . lineToInts
                    $ query

            -- Preset all min weights to the max possible weight
            let maxWeight = sum weights
            let minWeights = Seq.fromList
                    [maxWeight | _ <- [1..(length weights)]]

            let startingPath = prependLoc
                    Path { locs = Seq.empty, weight = 0 } finalLoc 

            -- Construct all complete paths
            let (_,allPaths) = buildPaths firstLoc
                    (minWeights,Seq.singleton startingPath)

            -- Find the best path
            let bestPath = Seq.index
                    (Seq.sortBy weightComparator allPaths)
                    0

            -- Print all paths
            when False $
                putStrLn
                    . unlines
                    . Foldable.toList
                    . fmap (showPath (n,m) firstLoc finalLoc)
                    $ allPaths

            -- Print the best path
            when True $
                putStrLn
                    . showPath (n,m) firstLoc finalLoc
                    $ bestPath

            -- Output the weight of the most efficient path
            print
                . weight
                $ bestPath

            -- Process the next query.  Using 'when False' allows testing a
            -- single query.
            when False
                $ processQueries n m weights
            )
        where
        -- Comparator for sorting by weights
        weightComparator :: Path -> Path -> Ordering
        weightComparator a b
                | weightA < weightB = LT
                | weightA > weightB = GT
                | otherwise = EQ
            where
                weightA = weight a
                weightB = weight b


        -- Build paths, eventually leading to complete paths
        buildPaths ::  Loc -> (MinWeights,Seq.Seq Path)
                            -> (MinWeights,Seq.Seq Path)
        buildPaths firstLoc (minWeights,paths)
            | Foldable.all isComplete paths = (minWeights,paths)
            | otherwise = Seq.foldlWithIndex (\(minWeights',accPaths) _ p ->
                if isComplete p
                    -- No need to do anything if path is already complete
                    then (minWeights',accPaths |> p)

                    -- Alter the second component of the pair while allowing
                    -- the first component to pass through
                    else second (accPaths ><)

                        -- Build from paths that are one step closer
                        . buildPaths firstLoc

                        -- Remove paths that have been bettered
                        . updateMinWeights (<)

                        -- Make a pair again
                        . (\nextPaths' -> (minWeights',nextPaths'))

                        -- Remove next paths that would go where other paths
                        -- have already gone
                        -- . Seq.filter (\p -> Foldable.all
                        --     (Foldable.notElem $ Seq.index p 0)
                        --     paths)

                        . nextPaths
                        $ p) (minWeights,Seq.empty)
                paths
            where
            -- A complete path starts with firstLoc because paths are built from
            -- back to front
            isComplete :: Path -> Bool
            isComplete p = Seq.index (locs p) 0 == firstLoc


            -- Iterate paths, keeping them and updating minWeights if better
            updateMinWeights :: (Int -> Int -> Bool)
                                -> (MinWeights,Seq.Seq Path)
                                -> (MinWeights,Seq.Seq Path)
            updateMinWeights cmpFunc (minWeights,paths) =
                Seq.foldlWithIndex
                        (\ (accMinWeights,accPaths) i p ->
                            let (r,c) = Seq.index (locs p) 0 in
                            let iLoc = (r*m + c) in
                            let pWeight = weight p in
                            let minWeight = Seq.index minWeights (r*m + c) in
                            if cmpFunc pWeight minWeight
                                then (Seq.update (r*m + c) pWeight accMinWeights,
                                    accPaths |> p)
                                else (accMinWeights,accPaths)
                                
                            )
                        (minWeights,Seq.empty)
                        paths
                        

        -- Create next paths based on next locations of current location
        nextPaths :: Path -> Seq.Seq Path
        nextPaths p
            -- At the beginning, you can go in any direction
            | length (locs p) == 1 = fmap (prependLoc p)
                . getNextLocs
                $ headLoc

            -- Otherwise, remove certain locations
            | otherwise = fmap (prependLoc p)
                    -- Remove doubling back
                    -- . Seq.filter (all (`Foldable.notElem` pWithoutHead)
                    --         . getNextLocs)
                    
                    -- Remove locations that has already been
                    -- touched by this path
                    -- . Seq.filter (`Foldable.notElem` p)
                    
                    -- Remove the location you just came from
                    . Seq.filter (/= prevLoc)

                    . getNextLocs
                    $ headLoc
            where
            headLoc = Seq.index (locs p) 0
            prevLoc = Seq.index (locs p) 1
    

        -- Get possible next locations from precache
        getNextLocs :: Loc -> Seq.Seq Loc
        getNextLocs (r,c) = Seq.index allNextLocs (r*m + c)
        

        -- Precache of all uses of nextLocs
        allNextLocs :: Seq.Seq (Seq.Seq Loc)
        allNextLocs = Seq.fromList
            . map (Seq.fromList . nextLocs)
            $ [(r,c) | r <- [0..(n-1)], c <- [0..(m-1)]]


        -- Determine possible next locations from the given location
        -- This is only used to build allNextLocs
        nextLocs :: Loc -> [Loc]
        nextLocs (r,c) = filter (\(r',c') ->
                all (uncurry (<=)) [(0,r'),(r',n-1),(0,c'),(c',m-1)])
            . map ((+) r *** (+) c)
            $ [(-1,0),(0,-1),(0,1),(1,0)]
        
    
        -- Prepend a location to a path, updating the weight accordingly
        prependLoc :: Path -> Loc -> Path
        prependLoc p (r,c) = Path {
            locs = (r,c) <| locs p,
            weight = weight p + weights ! (r*m + c)
        }

        -- Calculate the weight of a path
        calcWeight :: (Path -> Int)
        calcWeight = Foldable.sum
            . fmap (\ (r,c) -> weights ! (r*m + c))
            . locs
        
