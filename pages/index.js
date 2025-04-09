import Head from 'next/head'
import styles from '../styles/Home.module.css'

export default function Home() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Modern Functional Programming</title>
        <meta name="description" content="Learn Haskell, Elixir, and Rust - Modern functional programming languages" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <h1 className={styles.title}>
          Learn Modern <span className={styles.highlight}>Functional Programming</span>
        </h1>

        <p className={styles.description}>
          Master three powerful programming languages: Haskell, Elixir, and Rust
        </p>

        <div className={styles.grid}>
          <div className={styles.card}>
            <div className={styles.cardHeader}>
              <h2>Haskell</h2>
              <div className={styles.languageIcon}>λ</div>
            </div>
            <p>Pure functional programming with strong type system and lazy evaluation.</p>
            <ul className={styles.features}>
              <li>Type-safe programming</li>
              <li>Immutable data structures</li>
              <li>Powerful type system</li>
              <li>Concurrent programming</li>
            </ul>
            <a href="/haskell" className={styles.button}>Learn Haskell</a>
          </div>

          <div className={styles.card}>
            <div className={styles.cardHeader}>
              <h2>Elixir</h2>
              <div className={styles.languageIcon}>⚡</div>
            </div>
            <p>Dynamic, functional language designed for building scalable applications.</p>
            <ul className={styles.features}>
              <li>Concurrent programming</li>
              <li>Fault tolerance</li>
              <li>Hot code reloading</li>
              <li>Phoenix web framework</li>
            </ul>
            <a href="/elixir" className={styles.button}>Learn Elixir</a>
          </div>

          <div className={styles.card}>
            <div className={styles.cardHeader}>
              <h2>Rust</h2>
              <div className={styles.languageIcon}>🦀</div>
            </div>
            <p>Systems programming language focused on safety, concurrency, and performance.</p>
            <ul className={styles.features}>
              <li>Memory safety</li>
              <li>Zero-cost abstractions</li>
              <li>Concurrent programming</li>
              <li>Modern tooling</li>
            </ul>
            <a href="/rust" className={styles.button}>Learn Rust</a>
          </div>
          
          <div className={styles.card}>
            <div className={styles.cardHeader}>
              <h2>Practice Projects</h2>
              <div className={styles.languageIcon}>🛠️</div>
            </div>
            <p>Apply your knowledge with hands-on projects in Haskell and Elixir.</p>
            <ul className={styles.features}>
              <li>Todo List Application</li>
              <li>URL Shortener</li>
              <li>Polarity Placement Puzzle</li>
              <li>Real-world implementations</li>
            </ul>
            <a href="/practice-projects" className={styles.button}>View Projects</a>
          </div>
        </div>

        <div className={styles.cta}>
          <h2>Ready to start your journey?</h2>
          <p>Choose a language and begin learning today!</p>
          <div className={styles.buttonGroup}>
            {/* Removed buttons that don't link to existing pages */}
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
}
