import HW6.T1 (putCHT, getCHT, newCHT, sizeCHT)
main :: IO ()
main = do
    cht <- newCHT
    putCHT (1::Int) "11" cht
    res1 <- getCHT (1::Int) cht
    print res1
    putCHT (2::Int) "22" cht
    res2 <- getCHT (2::Int) cht
    print res2
    putCHT (3::Int) "33" cht
    res3 <- getCHT (3::Int) cht
    print res3
    putCHT (4::Int) "44" cht
    res4 <- getCHT (4::Int) cht
    print res4
    putCHT (5::Int) "55" cht
    res5 <- getCHT (5::Int) cht
    print res5

    res11 <- getCHT (1::Int) cht
    print res11
    res22 <- getCHT (2::Int) cht
    print res22
    res33 <- getCHT (3::Int) cht
    print res33
    res44 <- getCHT (4::Int) cht
    print res44
    res55 <- getCHT (5::Int) cht
    print res55

    sz <- sizeCHT cht
    print sz