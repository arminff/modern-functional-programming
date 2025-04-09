import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function ElixirModule1() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Elixir Module 1: Fundamentals | Modern Functional Programming</title>
        <meta name="description" content="Learn the fundamentals of Elixir programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/elixir">← Back to Elixir</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>⚡</div>
          <h1 className={styles.title}>Elixir Module 1: Fundamentals</h1>
          <p className={styles.description}>
            Introduction to Elixir programming
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '25%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Introduction to Elixir</h2>
            <p>Elixir is a dynamic, functional language designed for building scalable and maintainable applications. It leverages the Erlang VM, known for running low-latency, distributed, and fault-tolerant systems.</p>
            
            <h3>Key Features</h3>
            <ul>
              <li><strong>Functional Programming:</strong> Pure functions and immutable data</li>
              <li><strong>Concurrency:</strong> Built on Erlang's actor model with lightweight processes</li>
              <li><strong>Fault Tolerance:</strong> Built-in mechanisms for handling failures</li>
              <li><strong>Metaprogramming:</strong> Powerful macro system for code generation</li>
            </ul>
          </div>

          <div className={styles.moduleSection}>
            <h2>Basic Syntax and Data Types</h2>
            <p>Elixir has a clean, expressive syntax with a rich set of data types.</p>
            
            <h3>Basic Types</h3>
            <ul>
              <li><strong>Numbers:</strong> Integers and floats</li>
              <li><strong>Atoms:</strong> Constants whose name is their value</li>
              <li><strong>Strings:</strong> UTF-8 encoded</li>
              <li><strong>Booleans:</strong> true and false (atoms)</li>
            </ul>

            <h3>Variables and Pattern Matching</h3>
            <p>Variables in Elixir use pattern matching for assignment:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`x = 1
{a, b, c} = {1, 2, 3}
[head | tail] = [1, 2, 3, 4]`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Functions and Modules</h2>
            <p>Functions are the primary building blocks in Elixir, organized into modules.</p>
            
            <h3>Function Definition</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule Math do
  def add(a, b) do
    a + b
  end
  
  defp private_add(a, b) do
    a + b
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Anonymous Functions</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`sum = fn a, b -> a + b end
sum.(1, 2)  # => 3`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Collections and Enumerables</h2>
            <p>Elixir provides powerful collection types and operations.</p>
            
            <h3>Lists</h3>
            <p>Linked lists with head and tail operations:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`list = [1, 2, 3]
[head | tail] = list
# head = 1
# tail = [2, 3]`}
                </code>
              </pre>
            </div>

            <h3>Maps</h3>
            <p>Key-value pairs with pattern matching:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`map = %{name: "John", age: 30}
%{name: name} = map
# name = "John"`}
                </code>
              </pre>
            </div>

            <h3>Enumerables</h3>
            <p>Common operations on collections:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`list = [1, 2, 3, 4, 5]
Enum.map(list, &(&1 * 2))
Enum.filter(list, &(&1 > 2))
Enum.reduce(list, 0, &(&1 + &2))`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://elixir-lang.org/docs.html" target="_blank" rel="noopener noreferrer">
                  Official Elixir Documentation
                </a>
              </li>
              <li>
                <a href="https://elixir-lang.org/getting-started/introduction.html" target="_blank" rel="noopener noreferrer">
                  Elixir Getting Started Guide
                </a>
              </li>
              <li>
                <a href="https://hex.pm/" target="_blank" rel="noopener noreferrer">
                  Hex - Package Manager
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/elixir" className={styles.secondaryButton}>Back to Elixir</a>
            <a href="/elixir/module2" className={styles.primaryButton}>Next Module: Advanced Concepts</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 