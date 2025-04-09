import Head from 'next/head'
import Link from 'next/link'
import styles from '../styles/Home.module.css'

export default function Haskell() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Learn Haskell | Modern Functional Programming</title>
        <meta name="description" content="Learn Haskell - A pure functional programming language with a strong type system" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/">← Back to Home</Link>
        </div>

        <div className={styles.courseHeader}>
          <div className={styles.languageIcon}>λ</div>
          <h1 className={styles.title}>Learn Haskell</h1>
          <p className={styles.description}>
            Master the art of pure functional programming with Haskell
          </p>
        </div>

        <div className={styles.courseContent}>
          <section className={styles.section}>
            <h2>What is Haskell?</h2>
            <p>
              Haskell is a purely functional programming language with a strong type system and lazy evaluation. 
              It's designed for handling complex data, concurrent programming, and building robust applications.
            </p>
          </section>

          <section className={styles.section}>
            <h2>Course Modules</h2>
            <div className={styles.moduleGrid}>
              <div className={styles.moduleCard}>
                <h3>Module 1: Fundamentals</h3>
                <ul>
                  <li>Introduction to functional programming</li>
                  <li>Basic syntax and types</li>
                  <li>Functions and pattern matching</li>
                  <li>Lists and list comprehensions</li>
                </ul>
                <a href="/haskell/module1" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 2: Advanced Types</h3>
                <ul>
                  <li>Type classes and instances</li>
                  <li>Higher-order functions</li>
                  <li>Monads and functors</li>
                  <li>Type families and GADTs</li>
                </ul>
                <a href="/haskell/module2" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 3: Practical Haskell</h3>
                <ul>
                  <li>Working with IO</li>
                  <li>Concurrent programming</li>
                  <li>Testing and debugging</li>
                  <li>Building real-world applications</li>
                </ul>
                <a href="/haskell/module3" className={styles.button}>Start Module</a>
              </div>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Sample Code</h2>
            <div className={styles.codeBlock}>
              <pre>
                <code>
{`-- A simple Haskell function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Using higher-order functions
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- List comprehension
squares = [x^2 | x <- [1..10]]

-- Type class example
class Show a where
  show :: a -> String`}
                </code>
              </pre>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Why Learn Haskell?</h2>
            <div className={styles.benefitsGrid}>
              <div className={styles.benefitCard}>
                <h3>Strong Type System</h3>
                <p>Catch errors at compile time, not runtime</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Pure Functions</h3>
                <p>Easier to reason about and test your code</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Concurrent Programming</h3>
                <p>Built-in support for parallel execution</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Academic Excellence</h3>
                <p>Learn concepts used in advanced computer science</p>
              </div>
            </div>
          </section>
        </div>

        <div className={styles.cta}>
          <h2>Ready to start learning Haskell?</h2>
          <p>Begin your journey into functional programming today!</p>
          <div className={styles.buttonGroup}>
            <a href="/haskell/module1" className={styles.primaryButton}>Start Learning</a>
            <a href="/courses" className={styles.secondaryButton}>View All Courses</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 