# This is the assignment from pg. 177 to refactor the Key/Value
# store to use GenServer
defmodule KeyValueStore do
  use GenServer

  # callback functions
  @impl GenServer
  def init(_) do
    {:ok, %{}}
  end

  @impl GenServer
  def handle_call({:get, key}, _, state) do
    {:reply, Map.get(state, key), state}
  end

  # This is the first time I've done an @impl for a multi-clause
  # function, this syntax appears to work, not sure if it's idiomatic.
  @impl GenServer
  def handle_call({:all}, _, state) do
    {:reply, state, state}
  end

  # Returning {:stop, ...} from handle_* is the idiomatic way
  # to stop a GenServer
  @impl GenServer
  def handle_call({:stop}, _, state) do
    {:stop, :normal, state, state}
  end

  @impl GenServer
  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}
  end

  # interface functions
  def start do
    GenServer.start(__MODULE__, nil, name: __MODULE__)
  end

  def put(key, value) do
    GenServer.cast(__MODULE__, {:put, key, value})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def entries() do
    GenServer.call(__MODULE__, {:all})
  end

  def stop() do
    GenServer.call(__MODULE__, {:stop})
  end
end
