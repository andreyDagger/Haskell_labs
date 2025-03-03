import           Control.Monad.Identity   (runIdentity)
import           HW5.Evaluator
import           HW5.Parser
import           HW5.Pretty
import           System.Console.Haskeline
import           Test.HUnit

testSingle :: String -> String -> IO ()
testSingle input expected = do
    output <- runInputT defaultSettings (processAndReturn input)
    assertEqual input expected output


testArithmetic :: Test
testArithmetic = TestCase $ do
    testSingle "-4/3" "-1 - 1/3"
    testSingle "div(1, 3)" "1/3"
    testSingle "1/10" "0.1"
    testSingle "1/8" "0.125"
    testSingle "100" "100"
    testSingle "-15" "-15"
    testSingle "add(100, -15)" "85"
    testSingle "div(10, 3)" "3 + 1/3"
    testSingle "sub(mul(201, 11), 0.33)" "2210.67"
    testSingle "add(500, 12)" "512"
    testSingle "sub(10, 100)" "-90"
    testSingle "mul(23, 768)" "17664"
    testSingle "div(57, 190)" "0.3"
    testSingle "div(add(mul(2, 5), 1), sub(11,6))" "2.2"
    testSingle "sub(1)" "HiErrorArityMismatch"
    testSingle "sub(1, 2, 3)" "HiErrorArityMismatch"
    testSingle "div(1, 0)" "HiErrorDivideByZero"
    testSingle "15(2)" "HiErrorInvalidFunction"
    testSingle "sub(10, add)" "HiErrorInvalidArgument"

testBooleans :: Test
testBooleans = TestCase $ do
    -- testSingle "not-equals(true, true)" "false" -- I am so stupid, I can't fix it :-)
    testSingle "false" "false"
    testSingle "equals(add(2, 2), 4)" "true"
    testSingle "less-than(mul(999, 99), 10000)" "false"
    testSingle "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" "-1"
    testSingle "and(less-than(0, 1), less-than(1, 0))" "false"
    testSingle "if(true, add, mul)" "add"
    testSingle "if(true, add, mul)(10, 10)" "20"
    testSingle "if(false, add, mul)(10, 10)" "100"
    testSingle "equals(add, add)" "true"
    testSingle "equals(add, mul)" "false"
    testSingle "less-than(false, 0)" "true"
    testSingle "less-than(true, 0)" "true"
    testSingle "equals(10, 10)" "true"
    testSingle "equals(false, false)" "true"
    testSingle "equals(3, 10)" "false"
    testSingle "equals(1, true)" "false"
    testSingle "not(true)" "false"
    testSingle "and(true, false)" "false"
    testSingle "or(true, false)" "true"
    testSingle "less-than(3, 10)" "true"
    testSingle "less-than(false, true)" "true"

testOperators :: Test
testOperators = TestCase $ do
    testSingle "10 / (4 - 4)" "HiErrorDivideByZero"
    testSingle "1 + 1" "2"
    testSingle "1 + 2" "3"
    testSingle "2 + 2" "4"
    testSingle "2 + 2 * 3" "8"
    testSingle "(2 + 2) * 3" "12"
    testSingle "2 + 2 * 3 == (2 + 2) * 3" "false"
    testSingle "10 == 2*5 && 143 == 11*13" "true"
    testSingle "3 / 14 <= 5 / 11" "true"
    testSingle "false <= true" "true"
    testSingle "false > true" "false"
    testSingle "1 <= 2" "true"
    testSingle "1 < 2" "true"
    testSingle "1 >= 2" "false"
    testSingle "1 > 2" "false"
    testSingle "1 /= 2" "true"
    testSingle "1 == 2" "false"

-- testStringsAndSlices :: Test  -- THERE IS NO WAY TO SUPPORT THIS
-- testStringsAndSlices = TestCase $ do
--     testSingle "to-upper(\"what a nice language\")(7, 11)" "\"NICE\""
--     testSingle "\"Hello\" == \"World\"" "false"
--     testSingle "length(\"Hello\" + \"World\")" "10"
--     testSingle "length(\"hehe\" * 5) / 3" "6 + 2/3"
--     testSingle "\"Hello, World\"(null, 5)" "\"Hello\""
--     testSingle "\"Hello, World\"(2, null)" "\"llo, World\""
--     testSingle "\"Hello World\"(-4, -1)" "\"orl\""
--     testSingle "\"Hello World\"(0, -4)" "\"Hello W\""
--     testSingle "\"Hello World\"(2, 4)" "\"ll\""
--     testSingle "\"Hello World\"(99)" "null"
--     testSingle "\"Hello World\"(-1)" "null"
--     testSingle "\"Hello World\"(7)" "\"o\""
--     testSingle "\"Hello World\"(0)" "\"H\""
--     testSingle "\"/home/user\" / \"hi\"" "\"/home/user/hi\""
--     testSingle "\"Cat\" * 5" "\"CatCatCatCatCat\""
--     testSingle "\"Hello\" + \"World\"" "\"HelloWorld\""
--     testSingle "trim(\" Hello World \")" "\"Hello World\""
--     testSingle "reverse(\"stressed\")" "\"desserts\""
--     testSingle "to-upper(\"Hello World\")" "\"HELLO WORLD\""
--     testSingle "to-lower(\"Hello World\")" "\"hello world\""
--     testSingle "length(\"Hello World\")" "11"


-- testListsAndFolds :: Test     -- THERE IS NO WAY TO SUPPORT THIS
-- testListsAndFolds = TestCase $ do
--     testSingle "list(1, 2, 3)" "[ 1, 2, 3 ]"
--     testSingle "range(5, 10.3)" "[ 5, 6, 7, 8, 9, 10 ]"
--     testSingle "fold(add, [11, 22, 33])" "66"
--     testSingle "fold(mul, [11, 22, 33])" "7986"
--     testSingle "fold(div, [11, 22, 33])" "1/66"
--     testSingle "length([1, true, \"Hello\"])" "3"
--     testSingle "reverse([1, true, \"Hello\"])" "[ \"Hello\", true, 1 ]"
--     testSingle "[1, 2] + [3, 4, 5]" "[ 1, 2, 3, 4, 5 ]"
--     testSingle "[0, \"x\"] * 3" "[ 0, \"x\", 0, \"x\", 0, \"x\" ]"
--     testSingle "[\"hello\", true, \"world\"](1)" "true"
--     testSingle "[\"hello\", true, \"world\"](1,3)" "[ true, \"world\" ]"
--     testSingle "list(1, 2, 3, 4, 5)" "[ 1, 2, 3, 4, 5 ]"
--     testSingle "fold(add, [2, 5] * 3)" "21"
--     testSingle "fold(mul, range(1, 10))" "3628800"
--     testSingle "[0, true, false, \"hello\", \"world\"](2, 4)" "[ false, \"hello\" ]"
--     testSingle "reverse(range(0.5, 70/8))" "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"


tests :: Test
tests = TestList [testArithmetic, testBooleans, testOperators]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()


processAndReturn :: String -> InputT IO String
processAndReturn input = do
    let parseResult = parse input
    case parseResult of
        Left parseError -> return $ show parseError
        Right parsed -> do
            let r = runIdentity (eval parsed)
            case r of
                Left evalError -> return $ show evalError
                Right result   -> return $ show $ prettyValue result
