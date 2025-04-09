import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function RustModule2() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Rust Module 2: Advanced Concepts | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced concepts in Rust programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/rust">← Back to Rust</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>🦀</div>
          <h1 className={styles.title}>Rust Module 2: Advanced Concepts</h1>
          <p className={styles.description}>
            Advanced Rust programming concepts and techniques
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '50%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Advanced Ownership and Lifetimes</h2>
            <p>Understanding lifetimes is crucial for writing safe Rust code that manages references correctly.</p>
            
            <h3>Lifetime Annotations</h3>
            <p>Lifetime annotations help the compiler understand how references relate to each other:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}`}
                </code>
              </pre>
            </div>

            <h3>Static Lifetimes</h3>
            <p>The 'static lifetime indicates that a reference lives for the entire program:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`let s: &'static str = "Hello, world!";`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Traits and Generics</h2>
            <p>Traits define shared behavior, while generics allow for flexible, reusable code.</p>
            
            <h3>Defining Traits</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`trait Summary {
    fn summarize(&self) -> String;
}`}
                </code>
              </pre>
            </div>

            <h3>Implementing Traits</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`struct NewsArticle {
    headline: String,
    content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}: {}", self.headline, self.content)
    }
}`}
                </code>
              </pre>
            </div>

            <h3>Generic Functions</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`fn print_value<T: std::fmt::Display>(value: T) {
    println!("Value: {}", value);
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Error Handling</h2>
            <p>Rust's Result and Option types provide robust error handling mechanisms.</p>
            
            <h3>The Result Type</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(a / b)
    }
}`}
                </code>
              </pre>
            </div>

            <h3>Error Propagation</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`fn process_data() -> Result<(), Box<dyn std::error::Error>> {
    let result = some_operation()?;
    // Process result
    Ok(())
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Concurrency</h2>
            <p>Rust provides safe concurrency through its ownership system.</p>
            
            <h3>Threads</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use std::thread;

let handle = thread::spawn(|| {
    // Thread code
    println!("Hello from a thread!");
});`}
                </code>
              </pre>
            </div>

            <h3>Message Passing</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use std::sync::mpsc;

let (tx, rx) = mpsc::channel();

thread::spawn(move || {
    tx.send("Hello from thread").unwrap();
});

let received = rx.recv().unwrap();`}
                </code>
              </pre>
            </div>

            <h3>Shared State</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use std::sync::{Arc, Mutex};

let counter = Arc::new(Mutex::new(0));
let mut handles = vec![];

for _ in 0..10 {
    let counter = Arc::clone(&counter);
    let handle = thread::spawn(move || {
        let mut num = counter.lock().unwrap();
        *num += 1;
    });
    handles.push(handle);
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://doc.rust-lang.org/book/ch10-00-generics.html" target="_blank" rel="noopener noreferrer">
                  Rust Book: Generics and Traits
                </a>
              </li>
              <li>
                <a href="https://doc.rust-lang.org/book/ch16-00-concurrency.html" target="_blank" rel="noopener noreferrer">
                  Rust Book: Concurrency
                </a>
              </li>
              <li>
                <a href="https://doc.rust-lang.org/rust-by-example/error.html" target="_blank" rel="noopener noreferrer">
                  Rust by Example: Error Handling
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/rust/module1" className={styles.secondaryButton}>Previous Module: Fundamentals</a>
            <a href="/rust/module3" className={styles.primaryButton}>Next Module: Advanced Applications</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 