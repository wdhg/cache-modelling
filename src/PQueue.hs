{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module PQueue where

-- Pairing Heap implementation
data PQueue :: (* -> *) where
  Nil :: Ord a => PQueue a
  Node :: Ord a => a -> [PQueue a] -> PQueue a

merge :: Ord a => PQueue a -> PQueue a -> PQueue a
merge Nil rhs = rhs
merge lhs Nil = lhs
merge queue1@(Node v1 children1) queue2@(Node v2 children2)
  | v1 < v2 = Node v1 (queue2 : children1)
  | otherwise = Node v2 (queue1 : children2)

mergeChildren :: Ord a => [PQueue a] -> PQueue a
mergeChildren [] = Nil
mergeChildren [q] = q
mergeChildren (q1 : q2 : qs) = merge (merge q1 q2) (mergeChildren qs)

queue :: Ord a => a -> PQueue a -> PQueue a
queue x = merge (Node x [])

dequeue :: Ord a => PQueue a -> (a, PQueue a)
dequeue Nil = error "Cannot dequeue element in empty queue"
dequeue (Node x children) = (x, mergeChildren children)
