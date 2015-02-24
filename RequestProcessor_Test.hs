module RequestProcessor_Test where

import Test.HUnit
import RequestProcessor (processQuery)
import Data.Text.Lazy as D
default(String)

tests = TestList [testAddTwoNumbers,testfindLargeinTwoNumbers,testfindLargeinThreeNumbers,testfindLargeinFourNumbers,testSquareCubeEmpty,testSquareCube,testSquareCubeTwoNumbers,testMultiply,testPrimeSingleNumber,testPrimeDoubleNumber,testBritainPrimeMinister,testSpainCurrency,testEiffelTower,testJamesBond,testBananaColour,testFibonacci,testSubtraction,testSubtractionNegative,testPower,testPowerHighRange,testPlusPlus,testMultiplyPlus,testPlusMultiply,testAnagramDictionary,testAnagramAdmirer,testScrabbleBanana,testScrabbleRuby]

testAddTwoNumbers = TestCase $ assertEqual "Addition answer" (D.pack "19") (( processQuery $ "09046f30: what is 7 plus 12" ))

testfindLargeinTwoNumbers = TestCase $ assertEqual "Large number in two" (D.pack "428") (( processQuery $ "a94f8fb0: which of the following numbers is the largest: 428, 79" ))

testfindLargeinThreeNumbers = TestCase $ assertEqual "Large number in three numbers" (D.pack "234") (( processQuery $ "a94f8fb0: which of the following numbers is the largest: 234, 88, 179" ))

testfindLargeinFourNumbers = TestCase $ assertEqual "Large number in four numbers" (D.pack "204") (( processQuery $ "a94f8fb0: which of the following numbers is the largest: 204, 88, 179, 23" ))

testSquareCubeEmpty = TestCase $ assertEqual "Find Number both square Cube Empty" (D.pack "") (( processQuery $ "a0d099c0: which of the following numbers is both a square and a cube: 752, 2209" ))

testSquareCube = TestCase $ assertEqual "Find Number both square Cube" (D.pack "1000000") (( processQuery $ "a0d099c0: which of the following numbers is both a square and a cube: 752, 1000000" ))

testSquareCubeTwoNumbers = TestCase $ assertEqual "Find Number both square Cube two" (D.pack "1000000, 729") (( processQuery $ "a0d099c0: which of the following numbers is both a square and a cube: 752, 1000000, 729" ))

testMultiply = TestCase $ assertEqual "Multiply" (D.pack "13") (( processQuery $ "9f9e5630: what is 13 multiplied by 1" ))

testPrimeSingleNumber = TestCase $ assertEqual "Prime Check Single Number" (D.pack "281") (( processQuery $ "a9a40c80: which of the following numbers are primes: 281, 955" ))

testPrimeDoubleNumber = TestCase $ assertEqual "Prime Check Two Number" (D.pack "251, 79") (( processQuery $ "9e432120: which of the following numbers are primes: 42, 251, 79, 705" ))

testBritainPrimeMinister = TestCase $ assertEqual "Prime Minister check" (D.pack "David Cameron") (( processQuery $ "a1417070: who is the Prime Minister of Great Britain" ))

testSpainCurrency = TestCase $ assertEqual "Spain Currency check" (D.pack "peseta") (( processQuery $ "97c45c40: what currency did Spain use before the Euro" ))

testEiffelTower = TestCase $ assertEqual "Where is eiffel tower" (D.pack "Paris") (( processQuery $ "8234e920: which city is the Eiffel tower in" ))

testJamesBond = TestCase $ assertEqual "james bond" (D.pack "Sean Connery") (( processQuery $ "11f51120: who played James Bond in the film Dr No" ))

testBananaColour = TestCase $ assertEqual "banana colour" (D.pack "Yellow") (( processQuery $ "0d2a1e80: what colour is a banana" ))

testFibonacci = TestCase $ assertEqual "Fibonacci Sequence" (D.pack "34") (( processQuery $ "95c8f110: what is the 9th number in the Fibonacci sequence" ))

testSubtraction = TestCase $ assertEqual "Subtraction" (D.pack "7") (( processQuery $ "c1b68a20: what is 11 minus 4" ))

testSubtractionNegative = TestCase $ assertEqual "Subtraction Negative" (D.pack "-11") (( processQuery $ "c11d0db0: what is 5 minus 16" ))

testPower = TestCase $ assertEqual "Power of 1" (D.pack "1") (( processQuery $ "3a3dd280: what is 1 to the power of 12" ))

testPowerHighRange = TestCase $ assertEqual "Result High Number" (D.pack "205891132094649") (( processQuery $ "6f4b26c0: what is 9 to the power of 15" ))

testPlusPlus = TestCase $ assertEqual "Three number Plus" (D.pack "28") (( processQuery $ "707f0530: what is 7 plus 4 plus 17" ))

testMultiplyPlus = TestCase $ assertEqual "Multiply Plus" (D.pack "48") (( processQuery $ "f04d73b0: what is 13 multiplied by 3 plus 9" ))

testPlusMultiply = TestCase $ assertEqual "Plus Multiply" (D.pack "16") (( processQuery $ "f04d73b0: what is 8 plus 1 multiplied by 8" ))

testAnagramDictionary = TestCase $ assertEqual "Anagram" (D.pack "indicatory") (( processQuery $ "23a32380: which of the following is an anagram of \"dictionary\": butterfly, abdication, indicatory, incendiary" ))

testAnagramAdmirer = TestCase $ assertEqual "Anagram" (D.pack "married") (( processQuery $ "24d5d630: which of the following is an anagram of \"admirer\": dairy, forgot, married, random, border" ))

testScrabbleBanana = TestCase $ assertEqual "Scrabble score banana" (D.pack "8") (( processQuery $ "226f8460: what is the english scrabble score of banana" ))

testScrabbleRuby = TestCase $ assertEqual "Scrabble score banana" (D.pack "9") (( processQuery $ "226f8460: what is the english scrabble score of ruby" ))





main=runTestTT tests