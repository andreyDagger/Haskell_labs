import HW4.T1
import HW4.T2
import HW4.Types

main :: IO ()
main = do
    -- print ((myRead "123" "456") + (myRead "0" "500"))
    -- let p = (parseError <|> pChar)
    print (parseExpr "1+2")
    print (parseExpr "24 + Hello")
    print (parseExpr "3.14 + 1.618 * 2")
    print (parseExpr "2   * (1 \t\t       + 3)")
    print (parseExpr "1/0")
    print (runES (eval (Op (Div (Val 1.3333333333333333) (Val 0.0)))) [])
    print "qwe"
