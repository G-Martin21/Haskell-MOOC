-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
-- valAtRoot t = todo
valAtRoot Empty = Nothing
valAtRoot (Node a _ _) = Just a

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
-- treeSize t = todo
treeSize Empty = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
-- treeMax = todo
treeMax Empty = 0
-- treeMax (Node a l r) = max a (max (treeMax l) (treeMax r))
treeMax (Node a l r) = maximum [a, treeMax l, treeMax r]

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
-- allValues condition tree = todo
allValues _ Empty = True
allValues c (Node a l r) = c a && (allValues c l) && (allValues c r)

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree f t = todo
mapTree _ Empty = Empty
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
-- cull val tree = todo
cull _ Empty = Empty
cull v (Node v' l r) 
  | v == v'   = Empty
  | otherwise = Node v' (cull v l) (cull v r)

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True

isOrdered :: Ord a => Tree a -> Bool
-- isOrdered = todo
-- Proposed solution using the function from exercise 4 allValues
--isOrdered Empty = True
--isOrdered (Node v l r) =
--  allValues (<v) l && allValues (>v) r && isOrdered l && isOrdered r

-- My solution
isOrdered Empty = True
isOrdered (Node v Empty Empty) = True 
-- refactor https://www.perplexity.ai/search/refactor-this-haskell-O0phpsckT3KeqeJzQtZb3Q?s=c
isOrdered (Node v Empty rTree@(Node rv _ _)) = rv > v && isOrdered rTree
isOrdered (Node v lTree@(Node lv _ _) Empty) = lv < v && isOrdered lTree
isOrdered (Node v lTree@(Node lv _ _) rTree@(Node rv _ _)) = lv < v && rv > v && isOrdered lTree && isOrdered rTree

-- option 2
--isOrdered (Node v lTree@(Node lv _ _) rTree@(Node rv _ _))
--  | lv < v && rv > v = isOrdered lTree && isOrdered rTree
--  | otherwise = False

-- test 1
-- isOrdered (Node v Empty rTree@(Node rv l' r')) = if rv > v then isOrdered rTree else False
-- isOrdered (Node v lTree@(Node lv l r) Empty) = if lv < v then isOrdered lTree else False
-- isOrdered (Node v lTree@(Node lv l r) rTree@(Node rv l' r')) = (if lv < v then isOrdered lTree else False) &&
--                                             (if rv > v then isOrdered rTree else False)

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk [] (Node n _ _) = Just n 
walk _ Empty = Nothing 
walk (x:xs) (Node n l r)
  | x == StepL = walk xs l 
  | x == StepR = walk xs r

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

set :: [Step] -> a -> Tree a -> Tree a
-- set path val tree = todo
set [] v (Node n l r) = Node v l r 
set _ _ Empty = Empty
set (x:xs) v (Node n l r) 
  | x == StepL = Node n (set xs v l) r
  | x == StepR = Node n l (set xs v r)

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty)
--                            (Node 1 Empty Empty))
--                    (Node 5 Empty Empty))                     ==>  Just [StepL,StepR]

-- proposed solution
search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search v (Node v' l r)
  | v==v' = Just []
  | otherwise = case search v l of
                  Just xs -> Just (StepL:xs)
                  Nothing -> case search v r of
                               Just xs -> Just (StepR:xs)
                               Nothing -> Nothing


-- my solution
search :: Eq a => a -> Tree a -> Maybe [Step]
search v t = search' [] v t 

search' :: Eq a => [Step] -> a -> Tree a -> Maybe [Step]
search' path v Empty = Nothing 
search' path v (Node n l r)
  | v == n = Just (reverse path)
  | otherwise = case search' (StepL:path) v l of
                  Just p -> Just p
                  Nothing -> search' (StepR:path) v r
                `orElse` Nothing
  where
    Nothing `orElse` r = r
    l `orElse` _ = l

-- https://www.perplexity.ai/search/fix-this-haskell-WNB3715CRTCjg3TBJGYV2g?s=c    
--  | otherwise = search' (StepL:path) v l <|> search' (StepR:path) v r


-- https://www.perplexity.ai/search/fix-this-haskell-WNB3715CRTCjg3TBJGYV2g?s=c  