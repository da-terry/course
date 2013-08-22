import Test.QuickCheck
import Data.List

data ActiveBoard = ActiveBoard [Move] deriving (Eq, Show)
data FinishedBoard = FinishedBoard [Move] deriving (Eq, Show)
data Position = NW | N | NE | W | C | E | SW | S | SE deriving (Eq, Show, Ord)
data Player = O | X deriving (Eq, Show)
data Move = Move Player Position deriving (Eq, Show)
data InvalidMove = InvalidMove String deriving (Eq, Show)
data Outcome = Open ActiveBoard | Done FinishedBoard | Failed InvalidMove deriving (Eq, Show)

class Positioned a where
  at :: a -> Position -> Maybe Player
  back :: a -> ActiveBoard

instance Positioned ActiveBoard where
  at (ActiveBoard ms) = findPos ms  
  back a@(ActiveBoard []) = a
  back (ActiveBoard (_:t)) = ActiveBoard t

instance Positioned FinishedBoard where
  at (FinishedBoard ms) = findPos ms
  back (FinishedBoard (_:t)) = ActiveBoard t
  back (FinishedBoard []) = ActiveBoard []

findPos :: [Move] -> Position -> Maybe Player
findPos ms pos = 
  fmap (\(Move p _) -> p) (find (\(Move _ p) -> p == pos) ms)

move :: ActiveBoard -> Move -> Outcome
move b@(ActiveBoard ms) m@(Move player pos) =
  case (playerAt b pos) of
    Just p  -> Failed (InvalidMove ("position already taken by " ++ (show p)))
    Nothing -> if (isDoubleMove ms player) then
    	         Failed (InvalidMove ("illegal double move by " ++ (show player)))
               else if (finished (m : ms)) then
			     Done (FinishedBoard (m : ms))
			   else
    			 Open (ActiveBoard (m : ms)) 

isDoubleMove :: [Move] -> Player -> Bool
isDoubleMove [] _ = False
isDoubleMove (h:_) p = p == (toPlayer h) 
				     				    	
whoWon :: FinishedBoard -> Maybe Player
whoWon (FinishedBoard ms) = 
	if (hasWon ms X) then Just X
	else if (hasWon ms O) then Just O
	else Nothing

hasWon :: [Move] -> Player -> Bool
hasWon ms p =
		let 
		  conditions = [
			  [NW, N, NE],
			  [W, C, E],
			  [SW, S, SE],
			  [NW, W, SW],
			  [N, C, S],
			  [NE, E, SE],
			  [NW, C, SE],
			  [NE, C, SW]
			]
		in case find (\w -> (length (intersect (playerPositions ms p) w)) == 3) conditions of
			 Just _  -> True
			 Nothing -> False

takeBack :: Positioned b => b -> ActiveBoard
takeBack  = back

playerAt :: Positioned b => b -> Position -> Maybe Player
playerAt = at

finished :: [Move] -> Bool
finished ms = (sort (map toPosition ms)) == [NW, N, NE, W, C, E, SW, S, SE] 

toPlayer :: Move -> Player
toPlayer (Move p _) = p

toPosition :: Move -> Position
toPosition (Move _ p) = p 

playerPositions :: [Move] -> Player -> [Position]
playerPositions ms p = map toPosition (filter ((p ==) . toPlayer) ms)

-- tests

instance Arbitrary Player where
  arbitrary = elements [X, O]

instance Arbitrary Position where
  arbitrary = elements [NW, N, NE, W, C, E, SW, S, SE]

instance Arbitrary Move where
  arbitrary = do
  	    player   <- arbitrary
  	    pos      <- arbitrary
  	    return (Move player pos)

instance Arbitrary ActiveBoard where
  arbitrary = do
  		positions <- arbitrary
  		moves 	  <- choose (0, 9)
  		return (populateBoard positions O (ActiveBoard []) moves)

populateBoard :: [Position] -> Player -> ActiveBoard -> Int -> ActiveBoard
populateBoard [] _ _ _ = ActiveBoard []
populateBoard (h:t) player b@(ActiveBoard ms) i =
	if (length ms) == i then b
 	else populateBoard t (switch player) (ActiveBoard ((Move (switch player) h) : ms)) (i-1)

switch :: Player -> Player
switch X = O
switch O = X

prop_toPlayer :: Move -> Bool
prop_toPlayer m@(Move p _) = p == toPlayer m

prop_toPosition :: Move -> Bool
prop_toPosition m@(Move _ p) = p == toPosition m

--prop_move :: ActiveBoard -> Bool
--prop_move b = move b (Move X N) == Open

test :: IO ()
test = do
	quickCheck prop_toPlayer
	quickCheck prop_toPosition
--	quickCheck prop_move
