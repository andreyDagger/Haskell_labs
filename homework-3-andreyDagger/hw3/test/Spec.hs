
import HW3.T4

main :: IO ()
main = do
    let expr = (Val 0.0 * Val 0.0) - Val 0.0
    let s = runS (eval expr) []

    print s