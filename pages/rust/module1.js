import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function RustModule1() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Rust Module 1: Fundamentals | Modern Functional Programming</title>
        <meta name="description" content="Learn the fundamentals of Rust programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/rust">← Back to Rust</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>🦀</div>
          <h1 className={styles.title}>Rust Module 1: Fundamentals</h1>
          <p className={styles.description}>
            Introduction to Rust programming
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '25%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Introduction to Rust</h2>
            <p>Rust is a systems programming language focused on safety, concurrency, and performance. It was designed to prevent segmentation faults and ensure thread safety while maintaining high performance.</p>
            
            <h3>Key Features</h3>
            <ul>
              <li><strong>Memory Safety:</strong> Rust's ownership system prevents common memory-related bugs at compile time</li>
              <li><strong>Zero-Cost Abstractions:</strong> High-level abstractions compile to efficient low-level code</li>
              <li><strong>Concurrency:</strong> Built-in support for safe concurrent programming</li>
              <li><strong>Modern Tooling:</strong> Cargo package manager and integrated testing</li>
            </ul>
          </div>

          <div className={styles.moduleSection}>
            <h2>Basic Syntax and Data Types</h2>
            <p>Rust's syntax is clean and expressive, with a strong type system that helps catch errors early.</p>
            
            <h3>Basic Types</h3>
            <ul>
              <li><strong>Integers:</strong> i8, i16, i32, i64, i128, u8, u16, u32, u64, u128</li>
              <li><strong>Floating-point:</strong> f32, f64</li>
              <li><strong>Boolean:</strong> bool</li>
              <li><strong>Character:</strong> char (Unicode scalar value)</li>
            </ul>

            <h3>Variables and Mutability</h3>
            <p>Variables in Rust are immutable by default. Use the <code>mut</code> keyword to make them mutable.</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`let x = 5; // immutable
let mut y = 10; // mutable
y = 15; // OK
x = 20; // Error: cannot assign twice to immutable variable`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Ownership and Borrowing</h2>
            <p>Rust's ownership system is its most distinctive feature, providing memory safety without garbage collection.</p>
            
            <h3>Ownership Rules</h3>
            <ul>
              <li>Each value has a variable that owns it</li>
              <li>There can only be one owner at a time</li>
              <li>When the owner goes out of scope, the value is dropped</li>
            </ul>

            <h3>References and Borrowing</h3>
            <p>Instead of transferring ownership, you can borrow a value using references:</p>
            <ul>
              <li><code>&T</code> - Immutable reference (multiple allowed)</li>
              <li><code>&mut T</code> - Mutable reference (only one allowed)</li>
            </ul>
          </div>

          <div className={styles.moduleSection}>
            <h2>Structs and Enums</h2>
            <p>Rust provides powerful ways to create custom data types.</p>
            
            <h3>Structs</h3>
            <p>Structs allow you to group related data together:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`struct Person {
    name: String,
    age: u32,
}`}
                </code>
              </pre>
            </div>

            <h3>Enums</h3>
            <p>Enums let you define a type that can be one of several variants:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`enum Option<T> {
    Some(T),
    None,
}`}
                </code>
              </pre>
            </div>

            <h3>Pattern Matching</h3>
            <p>Rust's pattern matching is powerful and exhaustive:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`match value {
    Some(x) => println!("Got: {}", x),
    None => println!("Nothing"),
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://doc.rust-lang.org/book/" target="_blank" rel="noopener noreferrer">
                  The Rust Programming Language Book
                </a>
              </li>
              <li>
                <a href="https://doc.rust-lang.org/rust-by-example/" target="_blank" rel="noopener noreferrer">
                  Rust by Example
                </a>
              </li>
              <li>
                <a href="https://crates.io/" target="_blank" rel="noopener noreferrer">
                  Crates.io - Package Registry
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/rust" className={styles.secondaryButton}>Back to Rust</a>
            <a href="/rust/module2" className={styles.primaryButton}>Next Module: Advanced Concepts</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 