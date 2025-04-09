import Head from 'next/head'
import Link from 'next/link'
import styles from '../styles/Home.module.css'

export default function Elixir() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Learn Elixir | Modern Functional Programming</title>
        <meta name="description" content="Learn Elixir - A dynamic, functional language designed for building scalable applications" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/">← Back to Home</Link>
        </div>

        <div className={styles.courseHeader}>
          <div className={styles.languageIcon}>⚡</div>
          <h1 className={styles.title}>Learn Elixir</h1>
          <p className={styles.description}>
            Master the art of building scalable applications with Elixir
          </p>
        </div>

        <div className={styles.courseContent}>
          <section className={styles.section}>
            <h2>What is Elixir?</h2>
            <p>
              Elixir is a dynamic, functional language designed for building scalable and maintainable applications. 
              It leverages the Erlang VM, known for running low-latency, distributed, and fault-tolerant systems.
            </p>
          </section>

          <section className={styles.section}>
            <h2>Course Modules</h2>
            <div className={styles.moduleGrid}>
              <div className={styles.moduleCard}>
                <h3>Module 1: Fundamentals</h3>
                <ul>
                  <li>Introduction to Elixir</li>
                  <li>Basic syntax and data types</li>
                  <li>Functions and pattern matching</li>
                  <li>Collections and enumerables</li>
                </ul>
                <a href="/elixir/module1" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 2: Advanced Concepts</h3>
                <ul>
                  <li>Processes and concurrency</li>
                  <li>OTP and supervision trees</li>
                  <li>Metaprogramming with macros</li>
                  <li>Testing and debugging</li>
                </ul>
                <a href="/elixir/module2" className={styles.button}>Start Module</a>
              </div>

              <div className={styles.moduleCard}>
                <h3>Module 3: Web Development</h3>
                <ul>
                  <li>Phoenix framework basics</li>
                  <li>Building RESTful APIs</li>
                  <li>Real-time applications</li>
                  <li>Deployment and scaling</li>
                </ul>
                <a href="/elixir/module3" className={styles.button}>Start Module</a>
              </div>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Sample Code</h2>
            <div className={styles.codeBlock}>
              <pre>
                <code>
{`# A simple Elixir function
defmodule Math do
  def factorial(0), do: 1
  def factorial(n) when n > 0, do: n * factorial(n - 1)
end

# Pattern matching with lists
defmodule ListOps do
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)
end

# Using the pipe operator
def process_data(data) do
  data
  |> String.downcase()
  |> String.split()
  |> Enum.map(&String.capitalize/1)
  |> Enum.join(" ")
end

# Defining a GenServer
defmodule Counter do
  use GenServer

  def start_link(initial_value) do
    GenServer.start_link(__MODULE__, initial_value)
  end

  def increment do
    GenServer.cast(__MODULE__, :increment)
  end

  def handle_cast(:increment, count) do
    {:noreply, count + 1}
  end
end`}
                </code>
              </pre>
            </div>
          </section>

          <section className={styles.section}>
            <h2>Why Learn Elixir?</h2>
            <div className={styles.benefitsGrid}>
              <div className={styles.benefitCard}>
                <h3>Concurrency</h3>
                <p>Built-in support for concurrent programming</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Fault Tolerance</h3>
                <p>Applications that self-heal from failures</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Scalability</h3>
                <p>Designed for handling millions of connections</p>
              </div>
              <div className={styles.benefitCard}>
                <h3>Productivity</h3>
                <p>Expressive syntax and powerful tooling</p>
              </div>
            </div>
          </section>
        </div>

        <div className={styles.cta}>
          <h2>Ready to start learning Elixir?</h2>
          <p>Begin your journey into scalable applications today!</p>
          <div className={styles.buttonGroup}>
            <a href="/elixir/module1" className={styles.primaryButton}>Start Learning</a>
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