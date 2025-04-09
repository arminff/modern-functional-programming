import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function HaskellModule1() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Haskell Module 1: Fundamentals | Modern Functional Programming</title>
        <meta name="description" content="Learn the fundamentals of Haskell programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/haskell">← Back to Haskell</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>λ</div>
          <h1 className={styles.title}>Haskell Module 1: Fundamentals</h1>
          <p className={styles.description}>
            Introduction to functional programming with Haskell
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '25%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Introduction to Functional Programming</h2>
            <p>Haskell is a purely functional programming language that emphasizes immutability, pure functions, and strong type safety. It's designed to make reasoning about code easier and prevent many common programming errors.</p>
            
            <h3>Core Concepts</h3>
            <ul>
              <li><strong>Pure Functions:</strong> Functions that always return the same output for the same input and have no side effects</li>
              <li><strong>Immutability:</strong> Values cannot be modified after they are created</li>
              <li><strong>Referential Transparency:</strong> Expressions can be replaced with their values without changing program behavior</li>
              <li><strong>Lazy Evaluation:</strong> Expressions are only evaluated when their results are needed</li>
            </ul>

            <h3>Example of Pure Functions</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Pure function: always returns the same output for the same input
add :: Int -> Int -> Int
add x y = x + y

-- Not a pure function (has side effects)
-- print :: String -> IO ()
-- print str = putStrLn str`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Basic Syntax and Types</h2>
            <p>Haskell has a powerful type system that helps catch errors at compile time.</p>
            
            <h3>Type Signatures</h3>
            <p>Functions in Haskell often have explicit type signatures:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Function that takes two integers and returns an integer
add :: Int -> Int -> Int
add x y = x + y

-- Function that takes a string and returns a string
reverse :: String -> String
reverse str = reverse str`}
                </code>
              </pre>
            </div>

            <h3>Basic Types</h3>
            <ul>
              <li><strong>Numeric Types:</strong> Int, Integer, Float, Double</li>
              <li><strong>Boolean:</strong> Bool (True, False)</li>
              <li><strong>Character:</strong> Char</li>
              <li><strong>String:</strong> [Char] (a list of characters)</li>
              <li><strong>Unit:</strong> () (similar to void in other languages)</li>
            </ul>

            <h3>Type Inference</h3>
            <p>Haskell can often infer types without explicit declarations:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Type inference: Haskell knows this is Int -> Int -> Int
add x y = x + y

-- Type inference: Haskell knows this is [a] -> [a]
reverse xs = reverse xs`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Functions and Pattern Matching</h2>
            <p>Haskell uses pattern matching to define functions with multiple cases.</p>
            
            <h3>Function Definition</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Simple function definition
square :: Int -> Int
square x = x * x

-- Function with multiple parameters
add :: Int -> Int -> Int
add x y = x + y`}
                </code>
              </pre>
            </div>

            <h3>Pattern Matching</h3>
            <p>Pattern matching allows different function behavior based on input structure:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Pattern matching on lists
head :: [a] -> a
head (x:_) = x
head [] = error "head of empty list"

-- Pattern matching on tuples
fst :: (a, b) -> a
fst (x, _) = x`}
                </code>
              </pre>
            </div>

            <h3>Guards</h3>
            <p>Guards allow conditional execution based on boolean expressions:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Function with guards
abs :: Int -> Int
abs x | x >= 0 = x
     | otherwise = -x`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Lists and List Comprehensions</h2>
            <p>Lists are a fundamental data structure in Haskell, and list comprehensions provide a powerful way to create and transform lists.</p>
            
            <h3>List Operations</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Creating lists
numbers = [1, 2, 3, 4, 5]

-- List operations
head numbers      -- 1
tail numbers      -- [2, 3, 4, 5]
length numbers    -- 5
reverse numbers   -- [5, 4, 3, 2, 1]`}
                </code>
              </pre>
            </div>

            <h3>List Comprehensions</h3>
            <p>List comprehensions provide a concise way to generate lists:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Squares of numbers from 1 to 10
squares = [x^2 | x <- [1..10]]

-- Even numbers from 1 to 20
evens = [x | x <- [1..20], even x]

-- Cartesian product
pairs = [(x, y) | x <- [1,2,3], y <- ['a','b']]`}
                </code>
              </pre>
            </div>

            <h3>Higher-Order Functions</h3>
            <p>Haskell has powerful higher-order functions for working with lists:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Map: apply a function to each element
map (*2) [1,2,3,4]  -- [2,4,6,8]

-- Filter: keep elements that satisfy a predicate
filter even [1,2,3,4,5,6]  -- [2,4,6]

-- Fold: reduce a list to a single value
foldl (+) 0 [1,2,3,4]  -- 10`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://www.haskell.org/documentation" target="_blank" rel="noopener noreferrer">
                  Official Haskell Documentation
                </a>
              </li>
              <li>
                <a href="https://www.haskell.org/ghc/" target="_blank" rel="noopener noreferrer">
                  GHC - The Glasgow Haskell Compiler
                </a>
              </li>
              <li>
                <a href="https://hackage.haskell.org/" target="_blank" rel="noopener noreferrer">
                  Hackage - Haskell Package Repository
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/haskell" className={styles.secondaryButton}>Back to Haskell</a>
            <a href="/haskell/module2" className={styles.primaryButton}>Next Module: Advanced Types</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 