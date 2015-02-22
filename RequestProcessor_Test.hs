module RequestProcessor_Test where

import Test.HUnit
import RequestProcessor (processQuery)
import Data.Text.Lazy as D


tests = TestList [testAddTwoNumbers,testfindLargeinTwoNumbers,testfindLargeinThreeNumbers,testfindLargeinFourNumbers,testSquareCubeEmpty,testSquareCube,testMultiply,testPrimeSingleNumber,testPrimeDoubleNumber,testBritainPrimeMinister,testSpainCurrency,testEiffelTower,testJamesBond,testBananaColour,testFibonacci,testSubtraction,testSubtractionNegative,testPower,testPowerHighRange,testPlusPlus,testMultiplyPlus,testPlusMultiply,testAnagramDictionary,testAnagramAdmirer,testScrabbleBanana,testScrabbleRuby]

testAddTwoNumbers = TestCase $ assertEqual "Addition answer" (D.pack "19") (( processQuery $D.pack "09046f30: what is 7 plus 12" ))
testfindLargeinTwoNumbers = TestCase $ assertEqual "Large number in two" (D.pack "428") (( processQuery $D.pack "a94f8fb0: which of the following numbers is the largest: 428, 79" ))
testfindLargeinThreeNumbers = TestCase $ assertEqual "Large number in three numbers" (D.pack "234") (( processQuery $D.pack "a94f8fb0: which of the following numbers is the largest: 234, 88, 179" ))
testfindLargeinFourNumbers = TestCase $ assertEqual "Large number in four numbers" (D.pack "204") (( processQuery $D.pack "a94f8fb0: which of the following numbers is the largest: 204, 88, 179, 23" ))
testSquareCubeEmpty = TestCase $ assertEqual "Find Number both square Cube Empty" (D.pack "") (( processQuery $D.pack "a0d099c0: which of the following numbers is both a square and a cube: 752, 2209" ))
testSquareCube = TestCase $ assertEqual "Find Number both square Cube" (D.pack "1000000") (( processQuery $D.pack "a0d099c0: which of the following numbers is both a square and a cube: 752, 1000000" ))
testMultiply = TestCase $ assertEqual "Multiply" (D.pack "13") (( processQuery $D.pack "9f9e5630: what is 13 multiplied by 1" ))
testPrimeSingleNumber = TestCase $ assertEqual "Prime Check Single Number" (D.pack "281") (( processQuery $D.pack "a9a40c80: which of the following numbers are primes: 281, 955" ))
testPrimeDoubleNumber = TestCase $ assertEqual "Prime Check Two Number" (D.pack "251, 79") (( processQuery $D.pack "9e432120: which of the following numbers are primes: 42, 251, 79, 705" ))
testBritainPrimeMinister = TestCase $ assertEqual "Prime Minister check" (D.pack "David Cameron") (( processQuery $D.pack "a1417070: who is the Prime Minister of Great Britain" ))
testSpainCurrency = TestCase $ assertEqual "Spain Currency check" (D.pack "peseta") (( processQuery $D.pack "97c45c40: what currency did Spain use before the Euro" ))
testEiffelTower = TestCase $ assertEqual "Where is eiffel tower" (D.pack "Paris") (( processQuery $D.pack "8234e920: which city is the Eiffel tower in" ))
testJamesBond = TestCase $ assertEqual "james bond" (D.pack "Sean Connery") (( processQuery $D.pack "11f51120: who played James Bond in the film Dr No" ))
testBananaColour = TestCase $ assertEqual "banana colour" (D.pack "Yellow") (( processQuery $D.pack "0d2a1e80: what colour is a banana" ))
testFibonacci = TestCase $ assertEqual "Fibonacci Sequence" (D.pack "34") (( processQuery $D.pack "95c8f110: what is the 9th number in the Fibonacci sequence" ))
testSubtraction = TestCase $ assertEqual "Subtraction" (D.pack "7") (( processQuery $D.pack "c1b68a20: what is 11 minus 4" ))
testSubtractionNegative = TestCase $ assertEqual "Subtraction Negative" (D.pack "-11") (( processQuery $D.pack "c11d0db0: what is 5 minus 16" ))
testPower = TestCase $ assertEqual "Power of 1" (D.pack "1") (( processQuery $D.pack "3a3dd280: what is 1 to the power of 12" ))
testPowerHighRange = TestCase $ assertEqual "Result High Number" (D.pack "3486784401") (( processQuery $D.pack "6f4b26c0: what is 9 to the power of 10" ))
testPlusPlus = TestCase $ assertEqual "Three number Plus" (D.pack "28") (( processQuery $D.pack "707f0530: what is 7 plus 4 plus 17" ))
testMultiplyPlus = TestCase $ assertEqual "Multiply Plus" (D.pack "48") (( processQuery $D.pack "f04d73b0: what is 13 multiplied by 3 plus 9" ))
testPlusMultiply = TestCase $ assertEqual "Plus Multiply" (D.pack "9") (( processQuery $D.pack "f04d73b0: what is 8 plus 1 multiplied by 1" ))
testAnagramDictionary = TestCase $ assertEqual "Anagram" (D.pack "indicatory") (( processQuery $D.pack "23a32380: which of the following is an anagram of \"dictionary\": butterfly, abdication, indicatory, incendiary" ))
testAnagramAdmirer = TestCase $ assertEqual "Anagram" (D.pack "married") (( processQuery $D.pack "24d5d630: which of the following is an anagram of \"admirer\": dairy, forgot, married, random, border" ))
testScrabbleBanana = TestCase $ assertEqual "Scrabble score banana" (D.pack "8") (( processQuery $D.pack "226f8460: what is the english scrabble score of banana" ))
testScrabbleRuby = TestCase $ assertEqual "Scrabble score banana" (D.pack "9") (( processQuery $D.pack "226f8460: what is the english scrabble score of ruby" ))






main=runTestTT tests