# This file is like todo_server_gen_server.ex, but I looked at the example code
# and Jurić factored this into two modules, so I'm going to do that too.

defmodule Todo.Server do
  use GenServer

  # interface functions
  def start do
    GenServer.start(Todo.Server, nil)
  end

  def add_entry(todo_server, new_entry) do
    GenServer.cast(todo_server, {:add_entry, new_entry})
  end

  def entries(todo_server, date) do
    GenServer.call(todo_server, {:entries, date})
  end

  def entries(todo_server) do
    GenServer.call(todo_server, {:entries})
  end

  @impl GenServer
  def init(_) do
    {:ok, Todo.List.new()}
  end

  @impl GenServer
  def handle_call({:entries, date}, _, state) do
    {:reply, Todo.List.entries(state, date), state}
  end

  @impl GenServer
  def handle_call({:entries}, _, state) do
    {:reply, Todo.List.entries(state), state}
  end

  @impl GenServer
  def handle_cast({:add_entry, entry}, state) do
    {:noreply, Todo.List.add_entry(state, entry)}
  end

end
