# This file is like todo_server_gen_server.ex, but I looked at the example code
# and Jurić factored this into two modules, so I'm going to do that too.

defmodule Todo.Server do
  use GenServer

  # interface functions
  def start(list_name) do
    IO.puts("Starting database server.")
    GenServer.start(Todo.Server, list_name)
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

  # On pp 193-4, Jurić points out that long-running inits are deprecated,
  # as they can block other processes. A workaround is to have init/1
  # send() a message to its process, and then do the long-running initialization
  # in handle_info/2. This has a potential problem that if the process was
  # registered by name, it's possible that the process might receive a
  # message prior to the initialization. You can prevent this by manually
  # registering the process in init/1.
  @impl GenServer
  def init(list_name) do
    {:ok, {list_name, Todo.Database.get(list_name) || Todo.List.new()}}
  end

  @impl GenServer
  def handle_call({:entries, date}, _, {list_name, todo_list}) do
    {:reply, Todo.List.entries(todo_list, date), {list_name, todo_list}}
  end

  @impl GenServer
  def handle_call({:entries}, _, {list_name, todo_list}) do
    {:reply, Todo.List.entries(todo_list), {list_name, todo_list}}
  end

  @impl GenServer
  def handle_cast({:add_entry, entry}, {list_name, todo_list}) do
    new_list = Todo.List.add_entry(todo_list, entry)
    Todo.Database.store(list_name, new_list)
    {:noreply, {list_name, new_list}}
  end

end
