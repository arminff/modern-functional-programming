import Head from 'next/head'
import Link from 'next/link'
import styles from '../styles/Home.module.css'

export default function Rust() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Learn Rust | Modern Functional Programming</title>
        <meta name="description" content="Learn Rust - A systems programming language focused on safety, concurrency, and performance" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/">← Back to Home</Link>
        </div>

        <div className={styles.courseHeader}>
          <div className={styles.languageIcon}>🦀</div>
          <h1 className={styles.title}>Learn Rust</h1>
          <p className={styles.description}>
            Master the art of safe and efficient systems programming with Rust
          </p>
        </div>

        <div className={styles.courseContent}>
          <section className={styles.section}>
            <h2>What is Rust?</h2>
            <p>
              Rust is a systems programming language focused on safety, concurrency, and performance. 
              It prevents segmentation faults and ensures thread safety without a garbage collector.
            </p>
          </section>

          <section className={styles.section}>
            <h2>Course Modules</h2>
            <div className={styles.moduleGrid}>
              <div className={styles.moduleCard}>
                <h3>Module 1: Fundamentals</h3>
                <ul>
                  <li>Introduction to Rust</li>
                  <li>Variables and data types</li>
                  <li>Control flow and functions</li>
                  <li>Ownership and borrowing</li>
                </ul>
                <a href="/rust/module1" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 2: Advanced Concepts</h3>
                <ul>
                  <li>Structs and enums</li>
                  <li>Traits and generics</li>
                  <li>Error handling</li>
                  <li>Testing and documentation</li>
                </ul>
                <a href="/rust/module2" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 3: Systems Programming</h3>
                <ul>
                  <li>Concurrency with threads</li>
                  <li>Async/await and futures</li>
                  <li>FFI and unsafe Rust</li>
                  <li>Performance optimization</li>
                </ul>
                <a href="/rust/module3" className={styles.button}>Start Module</a>
              </div>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Sample Code</h2>
            <div className={styles.codeBlock}>
              <pre>
                <code>
{`// A simple Rust function
fn factorial(n: u64) -> u64 {
    match n {
        0 => 1,
        _ => n * factorial(n - 1)
    }
}

// Using structs and traits
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

// Error handling with Result
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(a / b)
    }
}

// Concurrent programming
use std::thread;

fn main() {
    let handle = thread::spawn(|| {
        for i in 1..10 {
            println!("Thread: {}", i);
        }
    });
    
    handle.join().unwrap();
}`}
                </code>
              </pre>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Why Learn Rust?</h2>
            <div className={styles.benefitsGrid}>
              <div className={styles.benefitCard}>
                <h3>Memory Safety</h3>
                <p>Prevents common memory-related bugs at compile time</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Performance</h3>
                <p>Zero-cost abstractions and minimal runtime overhead</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Concurrency</h3>
                <p>Safe concurrent programming with ownership model</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Modern Tooling</h3>
                <p>Excellent package manager and build tools</p>
              </div>
            </div>
          </section>
        </div>

        <div className={styles.cta}>
          <h2>Ready to start learning Rust?</h2>
          <p>Begin your journey into systems programming today!</p>
          <div className={styles.buttonGroup}>
            <a href="/rust/module1" className={styles.primaryButton}>Start Learning</a>
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