# This file is the assignment from page 177 - make the Todo server
# with GenServer

defmodule TodoList do
  use GenServer
  defstruct auto_id: 1, entries: %{}

  @impl GenServer
  def init(_) do
    {:ok, %TodoList{}}
  end

  @impl GenServer
  def handle_call({:entries, date}, _, state) do
    entry = state.entries
      |> Stream.filter(fn {_, entry} -> entry.date == date end)
      |> Enum.map(fn {_, entry} -> entry end)
    {:reply, entry, state}
  end

  @impl GenServer
  def handle_call({:entries}, _, state) do
    entries = state.entries
      |> Enum.map(fn {_, entry} -> entry end)
    {:reply, entries, state}
  end

  @impl GenServer
  def handle_cast({:add_entry, entry}, state) do
    entry = Map.put(entry, :id, state.auto_id)
    new_entries = Map.put(
      state.entries,
      state.auto_id,
      entry
    )
    {:noreply, %TodoList{state |
      entries: new_entries,
      auto_id: state.auto_id + 1
    }}
  end

  # interface functions:
  def start do
    GenServer.start(__MODULE__, nil, name: __MODULE__)
  end

  def add_entry(entry) do
    GenServer.cast(__MODULE__, {:add_entry, entry})
  end

  def entries(date) do
    GenServer.call(__MODULE__, {:entries, date})
  end

  def entries do
    GenServer.call(__MODULE__, {:entries})
  end
end
