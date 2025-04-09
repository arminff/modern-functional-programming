import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function ElixirModule3() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Elixir Module 3: Advanced Applications | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced applications in Elixir programming" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/elixir">← Back to Elixir</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>⚡</div>
          <h1 className={styles.title}>Elixir Module 3: Advanced Applications</h1>
          <p className={styles.description}>
            Building real-world applications with Elixir
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '75%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Web Development with Phoenix</h2>
            <p>Phoenix is a powerful web framework for Elixir that enables building scalable web applications.</p>
            
            <h3>Basic Phoenix Application</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", MyAppWeb do
    pipe_through :api
    
    get "/users", UserController, :index
    post "/users", UserController, :create
    get "/users/:id", UserController, :show
  end
end`}
                </code>
              </pre>
            </div>

            <h3>LiveView Real-time Features</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyAppWeb.CounterLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("increment", _, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h1>Count: <%= @count %></h1>
      <button phx-click="increment">Increment</button>
    </div>
    """
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Distributed Systems</h2>
            <p>Elixir excels at building distributed systems using its built-in distribution capabilities.</p>
            
            <h3>Node Communication</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule Distributed do
  def start do
    Node.start(:"node1@127.0.0.1")
    Node.connect(:"node2@127.0.0.1")
    
    # Spawn a process on another node
    Node.spawn(:"node2@127.0.0.1", fn ->
      IO.puts("Running on node2")
    end)
  end
  
  def remote_call(node, module, function, args) do
    :rpc.call(node, module, function, args)
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Distributed Task</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule DistributedTask do
  def process_data(data) do
    nodes = Node.list()
    
    tasks = Enum.map(nodes, fn node ->
      Task.async(fn ->
        Node.spawn(node, fn ->
          # Process data on remote node
          process_chunk(data)
        end)
      end)
    end)
    
    # Collect results
    Enum.map(tasks, &Task.await/1)
  end
  
  defp process_chunk(data) do
    # Process data chunk
    data
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Database Integration</h2>
            <p>Elixir provides excellent database integration through Ecto, its database wrapper and query generator.</p>
            
            <h3>Ecto Schema and Changeset</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyApp.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string
    field :age, :integer
    
    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/)
    |> validate_number(:age, greater_than: 0)
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Ecto Queries</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyApp.UserQueries do
  import Ecto.Query
  
  def active_users do
    from u in User,
      where: u.age >= 18,
      order_by: [desc: u.name],
      preload: [:posts]
  end
  
  def users_by_age_range(min_age, max_age) do
    from u in User,
      where: u.age >= ^min_age and u.age <= ^max_age,
      select: {u.name, u.age}
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Testing and Quality Assurance</h2>
            <p>Elixir provides robust testing tools and practices for ensuring code quality.</p>
            
            <h3>ExUnit Tests</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyApp.CalculatorTest do
  use ExUnit.Case
  
  alias MyApp.Calculator
  
  describe "Calculator.add/2" do
    test "adds two numbers correctly" do
      assert Calculator.add(2, 3) == 5
      assert Calculator.add(-1, 1) == 0
      assert Calculator.add(0, 0) == 0
    end
  end
  
  describe "Calculator.divide/2" do
    test "divides two numbers correctly" do
      assert Calculator.divide(6, 2) == 3
      assert Calculator.divide(5, 2) == 2.5
    end
    
    test "raises error when dividing by zero" do
      assert_raise ArithmeticError, fn ->
        Calculator.divide(5, 0)
      end
    end
  end
end`}
                </code>
              </pre>
            </div>

            <h3>Property-based Testing with StreamData</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule MyApp.PropertyTest do
  use ExUnit.Case
  use StreamData
  
  property "addition is commutative" do
    check all a <- integer(),
              b <- integer() do
      assert MyApp.Calculator.add(a, b) == MyApp.Calculator.add(b, a)
    end
  end
  
  property "multiplication distributes over addition" do
    check all a <- integer(),
              b <- integer(),
              c <- integer() do
      assert MyApp.Calculator.multiply(a, MyApp.Calculator.add(b, c)) ==
             MyApp.Calculator.add(
               MyApp.Calculator.multiply(a, b),
               MyApp.Calculator.multiply(a, c)
             )
    end
  end
end`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Performance Optimization</h2>
            <p>Techniques for optimizing Elixir applications for better performance.</p>
            
            <h3>Process Pool</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule ProcessPool do
  use GenServer
  
  def start_link(size) do
    GenServer.start_link(__MODULE__, size, name: __MODULE__)
  end
  
  def init(size) do
    processes = for _ <- 1..size do
      {:ok, pid} = Task.start_link(fn -> loop() end)
      pid
    end
    
    {:ok, %{processes: processes, current: 0}}
  end
  
  def process(data) do
    GenServer.call(__MODULE__, {:process, data})
  end
  
  def handle_call({:process, data}, _from, state) do
    pid = Enum.at(state.processes, rem(state.current, length(state.processes)))
    result = send(pid, {:process, data, self()})
    
    receive do
      {:result, value} -> {:reply, value, %{state | current: state.current + 1}}
    end
  end
  
  defp loop do
    receive do
      {:process, data, from} ->
        result = heavy_computation(data)
        send(from, {:result, result})
        loop()
    end
  end
  
  defp heavy_computation(data) do
    # Simulate heavy computation
    Process.sleep(100)
    data
  end
end`}
                </code>
              </pre>
            </div>

            <h3>ETS Optimization</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`defmodule Cache do
  use GenServer
  
  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
  
  def init(_) do
    table = :ets.new(:cache, [:named_table, :set, :public])
    {:ok, %{table: table}}
  end
  
  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> {:error, :not_found}
    end
  end
  
  def put(key, value) do
    :ets.insert(:cache, {key, value})
    :ok
  end
  
  def delete(key) do
    :ets.delete(:cache, key)
    :ok
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
                <a href="https://hexdocs.pm/phoenix/overview.html" target="_blank" rel="noopener noreferrer">
                  Phoenix Framework Documentation
                </a>
              </li>
              <li>
                <a href="https://hexdocs.pm/ecto/Ecto.html" target="_blank" rel="noopener noreferrer">
                  Ecto Documentation
                </a>
              </li>
              <li>
                <a href="https://hexdocs.pm/stream_data/StreamData.html" target="_blank" rel="noopener noreferrer">
                  StreamData Documentation
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/elixir/module2" className={styles.secondaryButton}>Previous Module: Advanced Concepts</a>
            <a href="/elixir" className={styles.primaryButton}>Back to Elixir</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 