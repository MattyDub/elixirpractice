defmodule KeyValueStore do
  # The 'use' macro injects a bunch of functions from the behaviour
  # into this module. Below, we override some of them.
  use GenServer

  @impl GenServer
  def init(_) do
    # This has to return the format {:ok, <initial state>}
    {:ok, %{}}
  end

  @impl GenServer
  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}
  end

  @impl GenServer
  def handle_call({:get, key}, _, state) do
    # The second argument here is a tuple that contains the request id
    # and the pid of the caller. We don't need it here, so it's ignored.

    # Remember, handle_call *has to* return the state, otherwise the server
    # behaviour will overwrite the state with nothing and we'd lose
    # all the state.
    {:reply, Map.get(state, key), state}
  end

  # interface functions
  def start do
    GenServer.start(KeyValueStore, nil)
  end

  def put(pid, key, value) do
    GenServer.cast(pid, {:put, key, value})
  end

  def get(pid, key) do
    GenServer.call(pid, {:get, key})
  end

end
