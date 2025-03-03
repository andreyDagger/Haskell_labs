import           HW1.T3

numbers :: [Int]
numbers = [1..10000]

main :: IO ()
main = do
    let t = tFromList numbers
    print (tdepth t)
    print (tmember (123::Int) t)
    -- print t
    -- print (tdepth t)
    -- print (tsize t)
    -- print t
    -- print (show (tmember 6 t))
    -- print (show (tmember 1 t))
    -- print (show (tmember 2 t))
    -- print (show (tmember 3 t))
    -- print (show (tmember 4 t))
    -- print (show (tmember 5 t))
    -- let tt = tinsert 9 (tinsert 8 (tinsert 7 t))
    -- let kek = tmember 7 tt
    -- print kek
    -- print (tdepth tt)
    -- print (tsize tt)
