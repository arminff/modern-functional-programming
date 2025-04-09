import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function HaskellModule3() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Haskell Module 3: Advanced Applications | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced applications and real-world projects in Haskell" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/haskell">← Back to Haskell</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>λ</div>
          <h1 className={styles.title}>Haskell Module 3: Advanced Applications</h1>
          <p className={styles.description}>
            Real-world applications and advanced Haskell projects
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '75%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Web Development with Haskell</h2>
            <p>Haskell has several powerful web frameworks for building robust, type-safe web applications.</p>
            
            <h3>Yesod Framework</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1>Hello World
    <p>Welcome to Yesod!
|]

main :: IO ()
main = warp 3000 HelloWorld`}
                </code>
              </pre>
            </div>

            <h3>Scotty</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Aeson (object, (.=))

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    json $ object ["message" .= ("Hello, World!" :: String)]
  
  get "/users/:id" $ do
    id <- param "id"
    json $ object ["userId" .= (id :: Int)]`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Concurrent and Parallel Programming</h2>
            <p>Haskell provides powerful abstractions for concurrent and parallel programming.</p>
            
            <h3>Software Transactional Memory (STM)</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`import Control.Concurrent.STM

type Account = TVar Int

transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
  fromBal <- readTVar from
  toBal <- readTVar to
  writeTVar from (fromBal - amount)
  writeTVar to (toBal + amount)

main :: IO ()
main = do
  alice <- atomically $ newTVar 100
  bob <- atomically $ newTVar 0
  atomically $ transfer alice bob 50
  aliceBal <- atomically $ readTVar alice
  bobBal <- atomically $ readTVar bob
  putStrLn $ "Alice: " ++ show aliceBal ++ ", Bob: " ++ show bobBal`}
                </code>
              </pre>
            </div>

            <h3>Parallel Processing</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`import Control.Parallel.Strategies

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parallelFib :: Int -> Int -> [Integer]
parallelFib start end = 
  map fib [start..end] \`using\` parList rdeepseq`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Domain-Specific Languages (DSLs)</h2>
            <p>Haskell is excellent for creating embedded domain-specific languages.</p>
            
            <h3>Parser Combinators</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`import Text.Parsec
import Text.Parsec.String

data Expr = Number Int
          | Add Expr Expr
          | Subtract Expr Expr
          deriving Show

number :: Parser Expr
number = Number . read <$> many1 digit

expr :: Parser Expr
expr = term \`chainl1\` addop

term :: Parser Expr
term = factor \`chainl1\` mulop

factor :: Parser Expr
factor = number <|> parens expr

addop :: Parser (Expr -> Expr -> Expr)
addop = (Add <$ char '+') <|> (Subtract <$ char '-')

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (Multiply <$ char '*') <|> (Divide <$ char '/')

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""`}
                </code>
              </pre>
            </div>

            <h3>Monadic DSLs</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`-- A simple configuration DSL
data Config = Config
  { name :: String
  , port :: Int
  , debug :: Bool
  } deriving Show

type ConfigM = State Config

setName :: String -> ConfigM ()
setName n = modify $ \\c -> c { name = n }

setPort :: Int -> ConfigM ()
setPort p = modify $ \\c -> c { port = p }

setDebug :: Bool -> ConfigM ()
setDebug d = modify $ \\c -> c { debug = d }

-- Usage
config :: ConfigM Config
config = do
  setName "MyApp"
  setPort 8080
  setDebug True
  get`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Data Processing and Analysis</h2>
            <p>Haskell is powerful for data processing, analysis, and scientific computing.</p>
            
            <h3>Working with CSV Data</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!))

data Person = Person
  { name :: String
  , age :: Int
  , email :: String
  } deriving Show

instance FromRecord Person where
  parseRecord v
    | length v == 3 = Person <$>
                        v .! 0 <*>
                        v .! 1 <*>
                        v .! 2
    | otherwise = mzero

processCSV :: FilePath -> IO (Vector Person)
processCSV path = do
  csvData <- BL.readFile path
  case decode NoHeader csvData of
    Left err -> error $ "Error parsing CSV: " ++ err
    Right people -> return people`}
                </code>
              </pre>
            </div>

            <h3>Statistical Analysis</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`import Statistics.Sample
import Statistics.Math

-- Calculate basic statistics
stats :: [Double] -> (Double, Double, Double)
stats xs = (mean, stdDev, median)
  where
    mean = meanUnbiased $ fromList xs
    stdDev = stdDevUnbiased $ fromList xs
    median = medianUnbiased $ fromList xs`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Compiler and Language Implementation</h2>
            <p>Haskell is excellent for implementing compilers and interpreters.</p>
            
            <h3>Simple Interpreter</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Expr = Lit Int
           | Add Expr Expr
           | Sub Expr Expr
           | Mul Expr Expr
           | Div Expr Expr

eval :: Expr -> Either String Int
eval (Lit n) = Right n
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ v1 + v2
eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ v1 - v2
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ v1 * v2
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 == 0
    then Left "Division by zero"
    else return $ v1 \`div\` v2`}
                </code>
              </pre>
            </div>

            <h3>Type Checker</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`data Type = TInt
           | TBool
           | TFun Type Type
           deriving (Eq, Show)

data Term = Var String
          | App Term Term
          | Abs String Type Term
          | TInt Int
          | TBool Bool

type Context = [(String, Type)]

typeCheck :: Context -> Term -> Either String Type
typeCheck ctx (Var x) = 
  case lookup x ctx of
    Just t -> Right t
    Nothing -> Left $ "Unbound variable: " ++ x
typeCheck ctx (App t1 t2) = do
  ty1 <- typeCheck ctx t1
  ty2 <- typeCheck ctx t2
  case ty1 of
    TFun a b | a == ty2 -> Right b
    _ -> Left "Type error: expected function"`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://www.yesodweb.com/" target="_blank" rel="noopener noreferrer">
                  Yesod Web Framework
                </a>
              </li>
              <li>
                <a href="https://hackage.haskell.org/package/scotty" target="_blank" rel="noopener noreferrer">
                  Scotty Web Framework
                </a>
              </li>
              <li>
                <a href="https://wiki.haskell.org/Applications_and_libraries/Web_frameworks" target="_blank" rel="noopener noreferrer">
                  Haskell Web Frameworks
                </a>
              </li>
              <li>
                <a href="https://wiki.haskell.org/Applications_and_libraries/Compilers_and_interpreters" target="_blank" rel="noopener noreferrer">
                  Haskell Compilers and Interpreters
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/haskell/module2" className={styles.secondaryButton}>Previous Module: Advanced Concepts</a>
            <a href="/haskell" className={styles.primaryButton}>Back to Haskell</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 