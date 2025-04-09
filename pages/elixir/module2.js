import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function ElixirModule2() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Elixir Module 2: Advanced Concepts | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced concepts in Elixir programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/elixir">← Back to Elixir</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>⚡</div>
          <h1 className={styles.title}>Elixir Module 2: Advanced Concepts</h1>
          <p className={styles.description}>
            Advanced Elixir programming concepts and techniques
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '50%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Processes and Concurrency</h2>
            <p>Elixir's concurrency model is based on lightweight processes that communicate via message passing.</p>
            
            <h3>Spawning Processes</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule ProcessExample do
  def start do
    pid = spawn(fn -> loop() end)
    send(pid, {:hello, "world"})
  end

  def loop do
    receive do
      {:hello, msg} ->
        IO.puts("Received: #{msg}")
        loop()
      _ ->
        loop()
    end
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Process Links and Monitors</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule ProcessLinks do
  def start do
    parent = self()
    child = spawn_link(fn -> 
      Process.sleep(1000)
      exit(:boom)
    end)
    
    receive do
      {:EXIT, ^child, reason} ->
        IO.puts("Child process exited with reason: #{inspect(reason)}")
    end
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>OTP Behaviors</h2>
            <p>OTP (Open Telecom Platform) provides behaviors for building robust, distributed applications.</p>
            
            <h3>GenServer</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule Counter do
  use GenServer

  # Client API
  def start_link(initial_value \\ 0) do
    GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
  end

  def increment(amount \\ 1) do
    GenServer.cast(__MODULE__, {:increment, amount})
  end

  def value do
    GenServer.call(__MODULE__, :value)
  end

  # Server Callbacks
  def init(initial_value) do
    {:ok, initial_value}
  end

  def handle_cast({:increment, amount}, state) do
    {:noreply, state + amount}
  end

  def handle_call(:value, _from, state) do
    {:reply, state, state}
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Supervisor</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MySupervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      {Counter, 0}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Metaprogramming</h2>
            <p>Elixir's metaprogramming capabilities allow you to extend the language and create domain-specific languages.</p>
            
            <h3>Macros</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyMacros do
  defmacro unless(condition, do: block) do
    quote do
      if !unquote(condition), do: unquote(block)
    end
  end
end

defmodule Example do
  import MyMacros
  
  def test do
    unless 1 == 2 do
      IO.puts("1 is not equal to 2")
    end
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Protocols</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defprotocol Stringify do
  def to_string(value)
end

defimpl Stringify, for: Integer do
  def to_string(value), do: Integer.to_string(value)
end

defimpl Stringify, for: Float do
  def to_string(value), do: Float.to_string(value)
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Advanced Pattern Matching</h2>
            <p>Elixir's pattern matching is powerful and can be used in many advanced ways.</p>
            
            <h3>Pattern Matching in Function Heads</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule PatternMatching do
  def process({:ok, value}), do: "Success: #{value}"
  def process({:error, reason}), do: "Error: #{reason}"
  def process(_), do: "Unknown"
  
  def parse(%{"name" => name, "age" => age}) when is_binary(name) and is_integer(age) do
    {:ok, %{name: name, age: age}}
  end
  def parse(_), do: {:error, "Invalid format"}
end`}
                </code>
              </pre>
            </div>

            <h3>Pin Operator</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule PinExample do
  def test do
    x = 1
    ^x = 1  # This matches
    ^x = 2  # This raises an error
    
    # In pattern matching
    [^x, y, z] = [1, 2, 3]
    # x is still 1, y is 2, z is 3
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Advanced Data Structures</h2>
            <p>Elixir provides several advanced data structures for different use cases.</p>
            
            <h3>ETS (Erlang Term Storage)</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule Cache do
  def start do
    :ets.new(:cache, [:named_table, :set, :public])
  end
  
  def put(key, value) do
    :ets.insert(:cache, {key, value})
  end
  
  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> {:error, :not_found}
    end
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Maps and Structs</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule User do
  defstruct [:name, :email, :age]
  
  def create(name, email, age) do
    %__MODULE__{
      name: name,
      email: email,
      age: age
    }
  end
  
  def update_age(user, new_age) do
    %{user | age: new_age}
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html" target="_blank" rel="noopener noreferrer">
                  Elixir OTP Documentation
                </a>
              </li>
              <li>
                <a href="https://elixir-lang.org/getting-started/meta/quote-and-unquote.html" target="_blank" rel="noopener noreferrer">
                  Elixir Metaprogramming Guide
                </a>
              </li>
              <li>
                <a href="https://elixir-lang.org/getting-started/processes.html" target="_blank" rel="noopener noreferrer">
                  Elixir Processes Guide
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/elixir/module1" className={styles.secondaryButton}>Previous Module: Fundamentals</a>
            <a href="/elixir/module3" className={styles.primaryButton}>Next Module: Advanced Applications</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 