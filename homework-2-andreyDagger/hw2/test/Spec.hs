import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4

import Data.List.NonEmpty as NE (NonEmpty(..))
import Data.Monoid (Sum(..), getSum)

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []

main :: IO ()
main = do
    let a = Branch 3 (Branch 1 Leaf "0" Leaf) "1" (Branch 1 Leaf "2" Leaf)
    print (treeToList a)
    print (splitOn '/' "path/with/trailing/slash/")
    print ("import " ++ joinWith '.' ("Data" :| "List" : "NonEmpty" : []))
    print (mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"])
    print (Data.Monoid.getSum $ mcat [Nothing, Just (Sum (2::Int)), Nothing, Just (Sum (40::Int))])
    print (epart [Left (Sum (3::Int)), Right [(1::Int),(2::Int),(3::Int)], Left (Sum (5::Int)), Right [(4::Int),(5::Int)]])
    print (DS "person" <> DS "address" <> DS "city")
    print (splitOn 'a' "aba")
    print (joinWith 'a' ("" :| ["b",""]))
    print ((joinWith 'a' . splitOn 'a') "aba")