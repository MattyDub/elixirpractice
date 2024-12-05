# This file is like todo_server_gen_server.ex, but I looked at the example code
# and Jurić factored this into two modules, so I'm going to do that too.

defmodule Todo.Server do
  use GenServer

  # interface functions
  def start(list_name) do
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

  @impl GenServer
  def init(list_name) do
    {:ok, {list_name, Todo.List.new()}}
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
