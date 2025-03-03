module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = CreateMeta Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

getSize :: Meta -> Int
getSize (CreateMeta sz _) = sz

getHeight :: Meta -> Int
getHeight (CreateMeta _ height) = height

tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch meta _ _ _) = getSize meta

tdepth :: Tree a -> Int
tdepth Leaf                = -1
tdepth (Branch meta _ _ _) = getHeight meta

calcBalance :: Tree a -> Int
calcBalance (Branch _ l _ r) = tdepth l - tdepth r
calcBalance Leaf             = 0

recalcMeta :: Tree a -> Tree a -> Meta
recalcMeta l r = CreateMeta (1 + tsize l + tsize r) (1 + max (tdepth l) (tdepth r))

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l y r)
  | x == y = True
  | x < y = tmember x l
  | otherwise = tmember x r

rotateLeft :: Tree a-> Tree a
rotateLeft (Branch _ p valA (Branch _ q valB r)) =
  let newLeft = Branch (recalcMeta p q) p valA q
  in Branch (recalcMeta newLeft r) newLeft valB r
rotateLeft a = a

rotateRight :: Tree a-> Tree a
rotateRight (Branch _ (Branch _ r valB q) valA p) =
  let newRight = Branch (recalcMeta q p) q valA p
  in Branch (recalcMeta r newRight) r valB newRight
rotateRight a = a

bigRotateLeft :: Tree a-> Tree a
bigRotateLeft (Branch _ l valT r) =
  let newRight = rotateRight r
  in rotateLeft (Branch (recalcMeta l newRight) l valT newRight)
bigRotateLeft a = a


bigRotateRight :: Tree a-> Tree a
bigRotateRight (Branch _ l valT r) =
  let newLeft = rotateLeft l
  in rotateRight (Branch (recalcMeta newLeft r) newLeft valT r)
bigRotateRight a = a

balanceRightBig :: Tree a -> Tree a
balanceRightBig (Branch meta l val r)
  | calcBalance r <= 0 = rotateLeft (Branch meta l val r)
  | otherwise = bigRotateLeft (Branch meta l val r)
balanceRightBig a = a

balanceLeftBig :: Tree a -> Tree a
balanceLeftBig (Branch meta l val r)
  | calcBalance l >= 0 = rotateRight (Branch meta l val r)
  | otherwise = bigRotateRight (Branch meta l val r)
balanceLeftBig a = a

balance :: Tree a -> Tree a
balance t
  | calcBalance t > 1 = balanceLeftBig t
  | calcBalance t < -1 = balanceRightBig t
  | otherwise = t

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch (CreateMeta 1 0) Leaf x Leaf
tinsert x (Branch sz l y r)
  | x == y = Branch sz l y r
  | x < y = let newLeft = tinsert x l
            in balance (Branch (recalcMeta newLeft r) newLeft y r)
  | otherwise = let newRight = tinsert x r
                in balance (Branch (recalcMeta l newRight) l y newRight)

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
