import Head from 'next/head'
import Link from 'next/link'
import styles from '../styles/Home.module.css'

export default function PracticeProjects() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Practice Projects | Modern Functional Programming</title>
        <meta name="description" content="Hands-on project ideas to help you apply your Haskell and Elixir knowledge" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/">← Back to Home</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>🛠️</div>
          <h1 className={styles.title}>Practice Projects</h1>
          <p className={styles.description}>
            Hands-on project ideas to help you apply your Haskell and Elixir knowledge
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.projectSection}>
            <h2>Todo List Application</h2>
            <p>Let's build a simple todo list application</p>
            
            <div className={styles.projectDetails}>
              <h3>We'll implement basic CRUD operations</h3>
              <ul>
                <li>Store todos in memory or a simple file</li>
                <li>Add, remove, and mark todos as complete</li>
              </ul>
              
              <h3>Features</h3>
              <ul>
                <li>Add new todos</li>
                <li>Mark todos as complete/incomplete</li>
                <li>Delete todos</li>
                <li>Persistent storage (extension)</li>
              </ul>
              
              <div className={styles.languageLinks}>
                <a href="#todo-list-haskell" className={styles.languageLink}>
                  <span className={styles.languageIcon}>λ</span> Haskell
                </a>
                <a href="#todo-list-elixir" className={styles.languageLink}>
                  <span className={styles.languageIcon}>⚡</span> Elixir
                </a>
              </div>
            </div>
            
            <div className={styles.implementationSection}>
              <h3>Implementation Details</h3>
              
              <div className={styles.languageImplementation}>
                <h4 id="todo-list-haskell">Haskell Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>Types.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Types where

data Todo = Todo
  { id :: Int
  , title :: String
  , completed :: Bool
  } deriving (Show, Eq)

type TodoList = [Todo]`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>Operations.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Operations where

import Types

addTodo :: String -> TodoList -> TodoList
addTodo title todos = 
  let newId = if null todos then 1 else maximum (map id todos) + 1
      newTodo = Todo newId title False
  in newTodo : todos

toggleTodo :: Int -> TodoList -> TodoList
toggleTodo todoId = map (\\todo -> 
  if id todo == todoId 
  then todo { completed = not (completed todo) }
  else todo)

deleteTodo :: Int -> TodoList -> TodoList
deleteTodo todoId = filter (\\todo -> id todo /= todoId)`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>Main.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Main where

import Types
import Operations

main :: IO ()
main = do
  let todos = addTodo "Learn Haskell" []
      todos' = addTodo "Learn Elixir" todos
      todos'' = toggleTodo 1 todos'
  print todos''`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
              
              <div className={styles.languageImplementation}>
                <h4 id="todo-list-elixir">Elixir Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>lib/todo.ex</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`defmodule Todo do
  defstruct [:id, :title, :completed]

  def new(title) do
    %Todo{id: generate_id(), title: title, completed: false}
  end

  defp generate_id do
    :erlang.unique_integer([:positive])
  end
end`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>lib/todo_list.ex</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`defmodule TodoList do
  def new, do: []

  def add(todos, title) do
    [Todo.new(title) | todos]
  end

  def toggle(todos, id) do
    Enum.map(todos, fn
      %Todo{id: ^id} = todo -> %{todo | completed: !todo.completed}
      todo -> todo
    end)
  end

  def delete(todos, id) do
    Enum.reject(todos, &(&1.id == id))
  end
end`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <div className={styles.projectSection}>
            <h2>URL Shortener</h2>
            <p>Web development and database operations</p>
            
            <div className={styles.projectDetails}>
              <h3>Features</h3>
              <ul>
                <li>Generate short codes for URLs</li>
                <li>Store and retrieve URLs</li>
                <li>Database persistence</li>
                <li>Web interface (extension)</li>
              </ul>
              
              <div className={styles.languageLinks}>
                <a href="#url-shortener-haskell" className={styles.languageLink}>
                  <span className={styles.languageIcon}>λ</span> Haskell
                </a>
                <a href="#url-shortener-elixir" className={styles.languageLink}>
                  <span className={styles.languageIcon}>⚡</span> Elixir
                </a>
              </div>
            </div>
            
            <div className={styles.implementationSection}>
              <h3>Implementation Details</h3>
              
              <div className={styles.languageImplementation}>
                <h4 id="url-shortener-haskell">Haskell Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>Types.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Types where

import Data.Text (Text)
import Data.Time (UTCTime)

data ShortUrl = ShortUrl
  { originalUrl :: Text
  , shortCode :: Text
  , createdAt :: UTCTime
  } deriving (Show, Eq)`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>Database.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Database where

import Types
import Database.SQLite.Simple
import Data.Time (getCurrentTime)

createTable :: Connection -> IO ()
createTable conn = execute_ conn 
  "CREATE TABLE IF NOT EXISTS urls (original_url TEXT, short_code TEXT, created_at TIMESTAMP)"

insertUrl :: Connection -> Text -> Text -> IO ()
insertUrl conn originalUrl shortCode = do
  now <- getCurrentTime
  execute conn 
    "INSERT INTO urls (original_url, short_code, created_at) VALUES (?, ?, ?)"
    (originalUrl, shortCode, now)

getUrl :: Connection -> Text -> IO (Maybe Text)
getUrl conn shortCode = do
  results <- query conn 
    "SELECT original_url FROM urls WHERE short_code = ?" 
    (Only shortCode)
  return $ case results of
    [Only url] -> Just url
    _ -> Nothing`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>Main.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Main where

import Database
import Database.SQLite.Simple
import Data.Text (pack)
import System.Random (randomRIO)
import Control.Monad (replicateM)

main :: IO ()
main = do
  conn <- open "urls.db"
  createTable conn
  
  let originalUrl = pack "https://example.com"
  shortCode <- generateShortCode
  insertUrl conn originalUrl shortCode
  
  maybeUrl <- getUrl conn shortCode
  print maybeUrl
  
  close conn

generateShortCode :: IO Text
generateShortCode = do
  chars <- replicateM 6 $ randomRIO ('a', 'z')
  return $ pack chars`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
              
              <div className={styles.languageImplementation}>
                <h4 id="url-shortener-elixir">Elixir Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>lib/url_shortener/url.ex</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`defmodule UrlShortener.Url do
  use Ecto.Schema
  import Ecto.Changeset

  schema "urls" do
    field :original_url, :string
    field :short_code, :string
    field :created_at, :utc_datetime

    timestamps()
  end

  def changeset(url, attrs) do
    url
    |> cast(attrs, [:original_url, :short_code])
    |> validate_required([:original_url, :short_code])
    |> unique_constraint(:short_code)
  end
end`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className={styles.codeFile}>
                  <h5>lib/url_shortener.ex</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`defmodule UrlShortener do
  import Ecto.Query
  alias UrlShortener.{Repo, Url}

  def create_url(original_url) do
    short_code = generate_short_code()
    
    %Url{}
    |> Url.changeset(%{original_url: original_url, short_code: short_code})
    |> Repo.insert()
  end

  def get_url(short_code) do
    Url
    |> where(short_code: ^short_code)
    |> Repo.one()
  end

  defp generate_short_code do
    :crypto.strong_rand_bytes(6)
    |> Base.url_encode64()
    |> binary_part(0, 6)
  end
end`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <div className={styles.projectSection}>
            <h2>Polarity Placement Puzzle</h2>
            <p>A puzzle solver for placing bar magnets on a 2D board</p>
            
            <div className={styles.projectDetails}>
              <h3>Project Description</h3>
              <p>The Polarity Placement Puzzle is a challenging problem that involves placing bar magnets on a 2D grid while satisfying various constraints. Each magnet has a positive (+) and negative (-) end, and they must be placed according to specific rules and constraints.</p>
              
              <h3>Problem Rules</h3>
              <ul>
                <li>The board is represented as a 2D grid where each cell can be empty, contain a magnet end, or be blocked.</li>
                <li>Each row and column has constraints specifying how many positive (+) and negative (-) ends must be placed.</li>
                <li>Same polarities cannot be adjacent horizontally or vertically (unless separated by a blocked cell).</li>
                <li>The board may contain pre-placed magnet ends (L/R for horizontal, T/B for vertical) that must be used.</li>
              </ul>
              
              <h3>Input Format</h3>
              <p>The input consists of:</p>
              <ul>
                <li>A list of strings representing the board, where:
                  <ul>
                    <li>'L' and 'R' indicate the left and right ends of a horizontal magnet</li>
                    <li>'T' and 'B' indicate the top and bottom ends of a vertical magnet</li>
                    <li>'.' indicates an empty cell</li>
                    <li>'X' indicates a blocked cell</li>
                  </ul>
                </li>
                <li>A tuple of four lists representing constraints:
                  <ul>
                    <li>First list: number of '+' required in each row (-1 means no constraint)</li>
                    <li>Second list: number of '-' required in each row (-1 means no constraint)</li>
                    <li>Third list: number of '+' required in each column (-1 means no constraint)</li>
                    <li>Fourth list: number of '-' required in each column (-1 means no constraint)</li>
                  </ul>
                </li>
              </ul>
              
              <h3>Example</h3>
              <p>Input board:</p>
              <div className={styles.codeBlock}>
                <pre>
                  <code>
                    {`L.R ... T.B
Constraints: ([1,0,1], [1,1,1], [1,0,1], [1,1,1])`}
                  </code>
                </pre>
              </div>
              
              <p>Expected output:</p>
              <div className={styles.codeBlock}>
                <pre>
                  <code>
                    {`+-R X.X T-+`}
                  </code>
                </pre>
              </div>
              
              <h3>Implementation Approach</h3>
              <p>The solution uses a backtracking algorithm to try different magnet placements. For each magnet:</p>
              <ul>
                <li>Identify all possible magnet placements (horizontal and vertical)</li>
                <li>For each placement, try both polarity orientations (+/- and -/+)</li>
                <li>Check if the current placement satisfies all constraints</li>
                <li>If valid, continue with the next magnet; if not, backtrack and try another placement</li>
                <li>Continue until a valid solution is found or all possibilities are exhausted</li>
              </ul>
              
              <h3>Features</h3>
              <ul>
                <li>Place bar magnets on a 2D board</li>
                <li>Satisfy row and column constraints</li>
                <li>Handle polarity constraints</li>
                <li>Backtracking algorithm implementation</li>
              </ul>
              
              <div className={styles.languageLinks}>
                <a href="#polarity-puzzle-haskell" className={styles.languageLink}>
                  <span className={styles.languageIcon}>λ</span> Haskell
                </a>
                <a href="#polarity-puzzle-elixir" className={styles.languageLink}>
                  <span className={styles.languageIcon}>⚡</span> Elixir
                </a>
              </div>
            </div>
            
            <div className={styles.implementationSection}>
              <h3>Implementation Details</h3>
              
              <div className={styles.languageImplementation}>
                <h4 id="polarity-puzzle-haskell">Haskell Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>Polarity.hs</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`module Polarity (polarity) where

import Data.List (transpose)

-- Define type aliases for clarity
type Board = [String]
type Solution = [String]
type Constraints = ([Int], [Int], [Int], [Int]) -- (left, right, top, bottom)
type Position = (Int, Int)
type Magnet = (Position, Position)

-- Main polarity function: takes a board and constraints, returns a solution
polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs =
    let rows = length board
        cols = length (head board)
        -- Initialize solution board with all 'X' (unfilled)
        emptyBoard = replicate rows (replicate cols 'X')
        
        -- Identify all horizontal magnet placements: L on left, R on right
        horizontalMagnets = [((i, j), (i, j + 1)) | i <- [0..rows-1], j <- [0..cols-2], 
                             board !! i !! j == 'L', board !! i !! (j + 1) == 'R']
        -- Identify all vertical magnet placements: T on top, B on bottom
        verticalMagnets = [((i, j), (i + 1, j)) | i <- [0..rows-2], j <- [0..cols-1], 
                           board !! i !! j == 'T', board !! (i + 1) !! j == 'B']
        -- Combine both horizontal and vertical possible placements
        possibleMagnets = horizontalMagnets ++ verticalMagnets
        
        -- Start the backtracking algorithm
        result = backtrack board specs emptyBoard possibleMagnets
    in result

-- Backtracking function to fill the board with magnets according to constraints
backtrack :: Board -> Constraints -> Solution -> [Magnet] -> Solution
backtrack board specs solution [] = 
    -- Base case: all magnets considered; check if final solution satisfies constraints
    if isValidSolution solution specs then solution
    else []  -- Return empty if not valid
backtrack board specs solution (magnet:rest) =
    let ((i1, j1), (i2, j2)) = magnet
        
        -- Skip if either magnet cell is already filled
        occupied = solution !! i1 !! j1 /= 'X' || solution !! i2 !! j2 /= 'X'
        
        -- Try placing the magnet with + at first cell and - at second
        solution2 = if occupied then [] else placeMagnet solution magnet '+' '-'
        trySolution2 = if null solution2 || not (isValid solution2 specs) then []
                       else backtrack board specs solution2 rest
        
        -- Try placing the magnet with - at first cell and + at second
        solution3 = if occupied then [] else placeMagnet solution magnet '-' '+'
        trySolution3 = if null solution3 || not (isValid solution3 specs) then []
                       else backtrack board specs solution3 rest
        
        -- Skip placing this magnet
        trySolution1 = backtrack board specs solution rest
    in
        -- Return the first valid solution found
        if not (null trySolution2) then trySolution2
        else if not (null trySolution3) then trySolution3
        else trySolution1

-- Places a magnet with specified polarities on the board
placeMagnet :: Solution -> Magnet -> Char -> Char -> Solution
placeMagnet solution ((i1, j1), (i2, j2)) c1 c2 =
    let solution1 = updateSolution solution i1 j1 c1
        solution2 = updateSolution solution1 i2 j2 c2
    in solution2

-- Helper to update a specific cell on the board
updateSolution :: Solution -> Int -> Int -> Char -> Solution
updateSolution solution i j c =
    take i solution ++ [take j (solution !! i) ++ [c] ++ drop (j + 1) (solution !! i)] ++ drop (i + 1) solution

-- Check if current solution is still valid under given constraints (not necessarily complete)
isValid :: Solution -> Constraints -> Bool
isValid solution (leftCons, rightCons, topCons, bottomCons) =
    let rows = length solution
        cols = length (head solution)
        
        -- Count + and - per row
        rowPos = [length [c | c <- solution !! i, c == '+'] | i <- [0..rows-1]]
        rowNeg = [length [c | c <- solution !! i, c == '-'] | i <- [0..rows-1]]
        
        -- Transpose to get columns and count + and - per column
        transposed = transpose solution
        colPos = [length [c | c <- transposed !! j, c == '+'] | j <- [0..cols-1]]
        colNeg = [length [c | c <- transposed !! j, c == '-'] | j <- [0..cols-1]]
        
        -- Ensure no adjacent '+' in any row
        noAdjPosRow = all (\\row -> not $ any (\\j -> j + 1 < length row && row !! j == '+' && row !! (j + 1) == '+') [0..length row - 2]) solution
        -- Ensure no adjacent '-' in any row
        noAdjNegRow = all (\\row -> not $ any (\\j -> j + 1 < length row && row !! j == '-' && row !! (j + 1) == '-') [0..length row - 2]) solution
        
        -- Ensure no adjacent '+' in any column
        noAdjPosCol = all (\\col -> not $ any (\\i -> i + 1 < length col && col !! i == '+' && col !! (i + 1) == '+') [0..length col - 2]) transposed
        -- Ensure no adjacent '-' in any column
        noAdjNegCol = all (\\col -> not $ any (\\i -> i + 1 < length col && col !! i == '-' && col !! (i + 1) == '-') [0..length col - 2]) transposed
    in
        -- Ensure no adjacency issues and partial constraint satisfaction (counts ≤ expected)
        noAdjPosRow && noAdjNegRow && noAdjPosCol && noAdjNegCol &&
        all (\\(actual, expected) -> expected == -1 || actual <= expected) (zip rowPos leftCons) &&
        all (\\(actual, expected) -> expected == -1 || actual <= expected) (zip rowNeg rightCons) &&
        all (\\(actual, expected) -> expected == -1 || actual <= expected) (zip colPos topCons) &&
        all (\\(actual, expected) -> expected == -1 || actual <= expected) (zip colNeg bottomCons)

-- Final check to see if the solution exactly matches the required constraints
isValidSolution :: Solution -> Constraints -> Bool
isValidSolution solution (leftCons, rightCons, topCons, bottomCons) =
    let rows = length solution
        cols = length (head solution)
        
        rowPos = [length [c | c <- solution !! i, c == '+'] | i <- [0..rows-1]]
        rowNeg = [length [c | c <- solution !! i, c == '-'] | i <- [0..rows-1]]
        
        transposed = transpose solution
        colPos = [length [c | c <- transposed !! j, c == '+'] | j <- [0..cols-1]]
        colNeg = [length [c | c <- transposed !! j, c == '-'] | j <- [0..cols-1]]
    in
        -- Ensure exact match with constraints
        all (\\(actual, expected) -> expected == -1 || actual == expected) (zip rowPos leftCons) &&
        all (\\(actual, expected) -> expected == -1 || actual == expected) (zip rowNeg rightCons) &&
        all (\\(actual, expected) -> expected == -1 || actual == expected) (zip colPos topCons) &&
        all (\\(actual, expected) -> expected == -1 || actual == expected) (zip colNeg bottomCons)`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
              
              <div className={styles.languageImplementation}>
                <h4 id="polarity-puzzle-elixir">Elixir Implementation</h4>
                
                <div className={styles.codeFile}>
                  <h5>lib/polarity.ex</h5>
                  <div className={styles.codeBlock}>
                    <pre>
                      <code>
                        {`defmodule Polarity do
  # Entry function that tries to solve the board based on the given specs.
  def polarity(board, specs) do
    board_grid = board_to_grid(board)             # Convert the tuple-based board to a 2D list of characters
    sol_grid = new_solution_grid(board_grid)      # Create an empty solution grid with same size, filled with nil
    dominoes = extract_dominoes(board_grid)       # Extract the positions of all dominoes (either vertical or horizontal)
    
    # Try to solve the puzzle using backtracking
    case solve(dominoes, 0, sol_grid, specs, board_grid) do
      {:ok, solved_sol_grid} -> grid_to_board(solved_sol_grid)  # Convert solved grid back to tuple form
      :error -> :no_solution                                     # Return :no_solution if no valid configuration found
    end
  end

  # Converts the board (tuple of strings) into a 2D list of codepoints (characters)
  defp board_to_grid(board) do
    board
    |> Tuple.to_list()
    |> Enum.map(&String.codepoints/1)
  end

  # Creates a new empty solution grid (same shape as the board), filled with \`nil\`
  defp new_solution_grid(board_grid) do
    Enum.map(board_grid, fn row -> Enum.map(row, fn _ -> nil end) end)
  end

  # Converts the grid back into a tuple of strings
  defp grid_to_board(grid) do
    grid
    |> Enum.map(&Enum.join/1)
    |> List.to_tuple()
  end

  # Finds all dominoes on the board by checking for 'T' (top) or 'L' (left) markers
  defp extract_dominoes(board_grid) do
    rows = length(board_grid)
    cols = if rows > 0, do: length(hd(board_grid)), else: 0

    for r <- 0..(rows - 1), c <- 0..(cols - 1),
        cell = Enum.at(Enum.at(board_grid, r), c),
        cell in ["T", "L"] do
      case cell do
        "T" -> {{r, c}, {r + 1, c}}  # vertical domino
        "L" -> {{r, c}, {r, c + 1}}  # horizontal domino
      end
    end
  end

  # Base case of backtracking: all dominoes have been placed
  defp solve(dominoes, index, sol_grid, specs, board_grid) when index == length(dominoes) do
    if valid_constraints?(sol_grid, specs) and valid_adjacency?(sol_grid) do
      {:ok, sol_grid}  # Return the solution if it's valid
    else
      :error           # Reject the configuration otherwise
    end
  end

  # Recursive backtracking: try to assign one of the value combinations to current domino
  defp solve(dominoes, index, sol_grid, specs, board_grid) do
    try_assign(dominoes, index, sol_grid, specs, board_grid, [
      {"X", "X"},      # Blocked cells
      {"+", "-"},      # Positive/Negative polarity
      {"-", "+"}       # Negative/Positive polarity
    ])
  end

  # If all options for this domino fail, return error
  defp try_assign(_dominoes, _index, _sol_grid, _specs, _board_grid, []), do: :error

  # Try assigning the current option to the domino and recurse
  defp try_assign(dominoes, index, sol_grid, specs, board_grid, [option | rest]) do
    {{r1, c1}, {r2, c2}} = Enum.at(dominoes, index)
    {val1, val2} = option
    new_sol_grid = update_grid(sol_grid, [{r1, c1, val1}, {r2, c2, val2}])

    # Proceed only if the local placement and partial constraints are valid
    if local_valid?(new_sol_grid, r1, c1) and
         local_valid?(new_sol_grid, r2, c2) and
         partial_constraints_ok?(new_sol_grid, specs) do
      case solve(dominoes, index + 1, new_sol_grid, specs, board_grid) do
        {:ok, sol} -> {:ok, sol}  # Found valid configuration
        :error -> try_assign(dominoes, index, sol_grid, specs, board_grid, rest)  # Try next option
      end
    else
      try_assign(dominoes, index, sol_grid, specs, board_grid, rest)  # Try next option
    end
  end

  # Helper to update multiple positions in a grid
  defp update_grid(sol_grid, updates) do
    sol_grid
    |> Enum.with_index()
    |> Enum.map(fn {row, r} ->
      updates_in_row = for {ru, c, val} <- updates, ru == r, do: {c, val}
      if updates_in_row == [] do
        row
      else
        row
        |> Enum.with_index()
        |> Enum.map(fn {cell, c} ->
          case Enum.find(updates_in_row, fn {col, _} -> col == c end) do
            nil -> cell
            {_, new_val} -> new_val
          end
        end)
      end
    end)
  end

  # Checks that a cell doesn't violate local adjacency rules
  defp local_valid?(sol_grid, r, c) do
    cell = Enum.at(Enum.at(sol_grid, r), c)
    if cell == nil or cell == "X" do
      true
    else
      neighbors =
        (if r > 0, do: [Enum.at(Enum.at(sol_grid, r - 1), c)], else: []) ++
          (if r < length(sol_grid) - 1, do: [Enum.at(Enum.at(sol_grid, r + 1), c)], else: []) ++
          (if c > 0, do: [Enum.at(Enum.at(sol_grid, r), c - 1)], else: []) ++
          (if c < length(hd(sol_grid)) - 1, do: [Enum.at(Enum.at(sol_grid, r), c + 1)], else: [])
      
      Enum.all?(neighbors, fn n ->
        n == nil or n == "X" or n != cell
      end)
    end
  end

  # Checks that current grid can still satisfy the constraints in future
  defp partial_constraints_ok?(sol_grid, specs) do
    left_cons = Tuple.to_list(specs["left"])
    right_cons = Tuple.to_list(specs["right"])
    top_cons = Tuple.to_list(specs["top"])
    bottom_cons = Tuple.to_list(specs["bottom"])

    # Check rows
    row_ok =
      Enum.with_index(sol_grid)
      |> Enum.all?(fn {row, r} ->
        plus_count = Enum.count(row, fn x -> x == "+" end)
        minus_count = Enum.count(row, fn x -> x == "-" end)
        total_assigned = Enum.count(row, fn x -> x != nil end)
        remaining = length(row) - total_assigned
        cond1 = (Enum.at(left_cons, r) == -1) or (plus_count <= Enum.at(left_cons, r) and plus_count + remaining >= Enum.at(left_cons, r))
        cond2 = (Enum.at(right_cons, r) == -1) or (minus_count <= Enum.at(right_cons, r) and minus_count + remaining >= Enum.at(right_cons, r))
        cond1 and cond2
      end)

    # Check columns
    cols =
      for c <- 0..(length(hd(sol_grid)) - 1) do
        Enum.map(sol_grid, fn row -> Enum.at(row, c) end)
      end

    col_ok =
      Enum.with_index(cols)
      |> Enum.all?(fn {col, c} ->
        plus_count = Enum.count(col, fn x -> x == "+" end)
        minus_count = Enum.count(col, fn x -> x == "-" end)
        remaining = Enum.count(col, fn x -> x == nil end)
        cond1 = (Enum.at(top_cons, c) == -1) or (plus_count <= Enum.at(top_cons, c) and plus_count + remaining >= Enum.at(top_cons, c))
        cond2 = (Enum.at(bottom_cons, c) == -1) or (minus_count <= Enum.at(bottom_cons, c) and minus_count + remaining >= Enum.at(bottom_cons, c))
        cond1 and cond2
      end)

    row_ok and col_ok
  end

  # Validates that the final grid fully satisfies all the constraints
  defp valid_constraints?(sol_grid, specs) do
    left_cons = Tuple.to_list(specs["left"])
    right_cons = Tuple.to_list(specs["right"])
    top_cons = Tuple.to_list(specs["top"])
    bottom_cons = Tuple.to_list(specs["bottom"])

    row_ok =
      Enum.with_index(sol_grid)
      |> Enum.all?(fn {row, r} ->
        plus_count = Enum.count(row, fn x -> x == "+" end)
        minus_count = Enum.count(row, fn x -> x == "-" end)
        (Enum.at(left_cons, r) == -1 or plus_count == Enum.at(left_cons, r)) and
          (Enum.at(right_cons, r) == -1 or minus_count == Enum.at(right_cons, r))
      end)

    cols =
      for c <- 0..(length(hd(sol_grid)) - 1) do
        Enum.map(sol_grid, fn row -> Enum.at(row, c) end)
      end

    col_ok =
      Enum.with_index(cols)
      |> Enum.all?(fn {col, c} ->
        plus_count = Enum.count(col, fn x -> x == "+" end)
        minus_count = Enum.count(col, fn x -> x == "-" end)
        (Enum.at(top_cons, c) == -1 or plus_count == Enum.at(top_cons, c)) and
          (Enum.at(bottom_cons, c) == -1 or minus_count == Enum.at(bottom_cons, c))
      end)

    row_ok and col_ok
  end

  # Ensures that no two same signs are adjacent unless they are both 'X'
  defp valid_adjacency?(sol_grid) do
    rows = length(sol_grid)
    cols = length(hd(sol_grid))

    row_adjacent_ok =
      for r <- 0..(rows - 1), c <- 0..(cols - 2) do
        a = Enum.at(Enum.at(sol_grid, r), c)
        b = Enum.at(Enum.at(sol_grid, r), c + 1)
        a == "X" or b == "X" or a != b
      end
      |> Enum.all?(& &1)

    col_adjacent_ok =
      for c <- 0..(cols - 1), r <- 0..(rows - 2) do
        a = Enum.at(Enum.at(sol_grid, r), c)
        b = Enum.at(Enum.at(sol_grid, r + 1))
        a == "X" or a == "X" or a != b
      end
      |> Enum.all?(& &1)

    row_adjacent_ok and col_adjacent_ok
  end
end`}
                      </code>
                    </pre>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/" className={styles.primaryButton}>Back to Home</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 