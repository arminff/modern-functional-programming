import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function HaskellModule2() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Haskell Module 2: Advanced Concepts | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced concepts in Haskell programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/haskell">← Back to Haskell</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>λ</div>
          <h1 className={styles.title}>Haskell Module 2: Advanced Concepts</h1>
          <p className={styles.description}>
            Advanced Haskell programming concepts and techniques
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '50%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Type Classes and Type Constraints</h2>
            <p>Type classes provide a powerful way to define shared behavior across different types.</p>
            
            <h3>Defining Type Classes</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)`}
                </code>
              </pre>
            </div>

            <h3>Implementing Type Classes</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Point = Point Int Int

instance Eq Point where
  (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2`}
                </code>
              </pre>
            </div>

            <h3>Type Constraints</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Monads and Applicatives</h2>
            <p>Monads and applicatives provide a structured way to handle effects in pure functional code.</p>
            
            <h3>The Maybe Monad</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Maybe a = Nothing | Just a

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just x >>= f = f x`}
                </code>
              </pre>
            </div>

            <h3>The Either Monad</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Either a b = Left a | Right b

instance Monad (Either e) where
  return = Right
  Left e >>= _ = Left e
  Right x >>= f = f x`}
                </code>
              </pre>
            </div>

            <h3>Using Monads</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

compute :: Int -> Int -> Int -> Maybe Int
compute x y z = do
  a <- divide x y
  b <- divide a z
  return b`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Advanced Type System Features</h2>
            <p>Haskell's type system is powerful and expressive, allowing for sophisticated abstractions.</p>
            
            <h3>Type Families</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`type family Element a where
  Element [a] = a
  Element (Maybe a) = a
  Element (Either a b) = a`}
                </code>
              </pre>
            </div>

            <h3>GADTs (Generalized Algebraic Data Types)</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a`}
                </code>
              </pre>
            </div>

            <h3>Type Applications</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`{-# LANGUAGE TypeApplications #-}

-- Using type applications
read @Int "123"  -- Explicitly specify the type`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Lazy Evaluation and Performance</h2>
            <p>Understanding lazy evaluation is crucial for writing efficient Haskell code.</p>
            
            <h3>Strict vs. Lazy Evaluation</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Lazy evaluation (default)
sum [1..1000000]  -- Only evaluates what's needed

-- Strict evaluation with BangPatterns
{-# LANGUAGE BangPatterns #-}
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (!x:xs) = x + sum' xs`}
                </code>
              </pre>
            </div>

            <h3>Memory Management</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Avoiding space leaks
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = let acc' = f acc x in acc' `seq` foldl' f acc' xs`}
                </code>
              </pre>
            </div>

            <h3>Stream Processing</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- Efficient stream processing
import Data.ByteString.Lazy as B

processFile :: FilePath -> IO ()
processFile path = do
  contents <- B.readFile path
  let result = B.filter (/= 0) contents
  B.writeFile (path ++ ".processed") result`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://wiki.haskell.org/Typeclassopedia" target="_blank" rel="noopener noreferrer">
                  Typeclassopedia
                </a>
              </li>
              <li>
                <a href="https://wiki.haskell.org/Monad" target="_blank" rel="noopener noreferrer">
                  Haskell Wiki: Monad
                </a>
              </li>
              <li>
                <a href="https://wiki.haskell.org/Performance" target="_blank" rel="noopener noreferrer">
                  Haskell Wiki: Performance
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/haskell/module1" className={styles.secondaryButton}>Previous Module: Fundamentals</a>
            <a href="/haskell/module3" className={styles.primaryButton}>Next Module: Advanced Applications</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 