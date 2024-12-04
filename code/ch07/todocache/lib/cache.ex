# This module keeps a map of servers, and manages interactions with them.
defmodule Todo.Cache do
  use GenServer

  # behaviour functions
  @impl GenServer
  def init(_) do
    {:ok, %{}}
  end

  @impl GenServer
  def handle_call({:server_process, todo_list_name}, _, todo_servers) do
    # This is the meat of the cache: fetches the pid if it exists, or creates
    # it and tracks it if it doesn't (and then returns the pid)
    case Map.fetch(todo_servers, todo_list_name) do
      {:ok, todo_server} -> {:reply, todo_server, todo_servers}
      :error ->
        {:ok, new_server} = Todo.Server.start()
        {:reply, new_server, Map.put(todo_servers, todo_list_name, new_server)}
    end
  end

  # interface functions
  def start do
    GenServer.start(__MODULE__, nil)
  end

  def server_process(cache_pid, todo_list_name) do
    GenServer.call(cache_pid, {:server_process, todo_list_name})
  end
end
