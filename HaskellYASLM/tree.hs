module Main where

data TreeNode k v = Fork (TreeNode k v) (TreeNode k v) k v Int
        | Empty

isEmpty :: TreeNode k v -> Bool
isEmpty Empty = True
isEmpty _     = False

left :: TreeNode k v -> TreeNode k v
left (Fork x _ _ _ _) = x
left Empty          = undefined

right :: TreeNode k v -> TreeNode k v
right (Fork _ x _ _ _) = x
right Empty          = undefined

key :: TreeNode k v -> k
key (Fork _ _ x _ _) = x
key Empty          = undefined

value :: TreeNode k v -> v
value (Fork _ _ _ x _) = x
value Empty          = undefined

height :: TreeNode k v -> Int
height (Fork _ _ _ _ x) = x
height Empty          = 0

upd :: TreeNode k v -> TreeNode k v -> Int
upd l r = max (height l) (height r) + 1     

fork :: TreeNode k v -> TreeNode k v -> k -> v -> TreeNode k v
fork l r k v = Fork l r k v (upd l r)

left_rotate :: TreeNode k v -> TreeNode k v
left_rotate (Fork l (Fork l2 r2 k2 v2 _) k v _) = fork (fork l l2 k v) r2 k2 v2
left_rotate _                                     = undefined

right_rotate :: TreeNode k v -> TreeNode k v
right_rotate (Fork (Fork l2 r2 k2 v2 _) r k v _) = fork l2 (fork r2 r k v)  k2 v2
right_rotate _                                     = undefined

balance :: TreeNode k v -> TreeNode k v
balance Empty              = Empty
balance t@(Fork l r k v _) =
    if height r - height l > 1 then 
        left_rotate $ if not (height (left r) <= height (right r) )  
        then fork l (right_rotate r) k v
        else t
    else if height l - height r > 1 then
        right_rotate $ if not (height (right l) <= height(left l) ) 
        then fork (left_rotate l) r k v
        else t
    else t


add :: Ord k => k -> v -> TreeNode k v -> TreeNode k v
add k v Empty = Fork Empty Empty k v 1
add k v (Fork l r k1 v1 _) =
            balance $ if k < k1 then 
                fork (add k v l) r k1 v1
            else 
                fork l (add k v r) k1 v1

count :: TreeNode k v -> Int
count (Fork l r _ _ _) = count l + 1 + count r
count Empty            = 0
        
mostLeft :: TreeNode k v -> TreeNode k v        
mostLeft Empty                   = undefined
mostLeft t@(Fork Empty _ _ _ _)  = t
mostLeft (Fork x _ _ _ _)        = mostLeft x
    
mostRight :: TreeNode k v -> TreeNode k v        
mostRight Empty                   = undefined
mostRight t@(Fork Empty _ _ _ _)  = t
mostRight (Fork _ x _ _ _)        = mostRight x

find :: (Ord k) => k => TreeNode k v -> TreeNode k v        
find _ Empty = Empty
find k f@(Fork l r k1 _ _)
    | k == k1   = f
    | k < k1    = find k l
    | otherwise = find k r


main :: IO()
main = putStrLn "aaa"