# We put both modules in here because we don't get to mix until Ch 07.

defmodule TodoServer do
  # example of usage:
  # iex(1)> ts = TodoServer.start()
  # #PID<0.114.0>
  # iex(2)> TodoServer.add_entry(ts, %{date: ~D[2024-10-05], title: "Task 1"})
  # {:add_entry, %{date: ~D[2024-10-05], title: "Task 1"}}
  # iex(3)> TodoServer.entries(ts)
  # [%{id: 1, date: ~D[2024-10-05], title: "Task 1"}]
  # (Note: see comment in the last process_message clause for why this is repeated)
  # iex(4)> TodoServer.entries(ts)
  # [%{id: 1, date: ~D[2024-10-05], title: "Task 1"}]
  # iex(5)> TodoServer.add_entry(ts, %{date: ~D[2024-10-06], title: "Watch Arsenal win"})
  # {:add_entry, %{date: ~D[2024-10-06], title: "Watch Arsenal win"}}
  # iex(6)> TodoServer.entries(ts)
  # [
  #   %{id: 1, date: ~D[2024-10-05], title: "Task 1"},
  #   %{id: 2, date: ~D[2024-10-06], title: "Watch Arsenal win"}
  # ]
  # iex(7)> TodoServer.entries(ts, ~D[2024-10-05])
  # [%{id: 1, date: ~D[2024-10-05], title: "Task 1"}]

  def start do
    # I was wondering why this book uses a lambda whenever it calls spawn/1. I think it's
    # because it's a little simpler to call spawn/1 and not spawn/3; see the top bit of
    # https://hexdocs.pm/elixir/1.12/Process.html for more context. spawn/1 takes a 0-arity
    # function, so if we want to pass args to it, we have to either jump through the hoop of
    # spawn/3 or use a lambda.
    spawn(fn -> loop(TodoList.new()) end)
  end

  def add_entry(todo_server, new_entry) do
    send(todo_server, {:add_entry, new_entry})
  end

  def entries(todo_server, date) do
    send(todo_server, {:entries, self(), date})
    receive do
      {:todo_entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  def entries(todo_server) do
    send(todo_server, {:entries, self()})
    receive do
      {:todo_entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  # non-interface code below here
  defp loop(todo_list) do
    # Note once again that we use the "loop" function to manage state for the process
    new_todo_list =
      receive do
        message -> process_message(todo_list, message)
      end
    loop(new_todo_list)
  end

  defp process_message(todo_list, {:add_entry, new_entry}) do
    # implicitly returning the return value of TodoList.add_entry (the updated todo list):
    TodoList.add_entry(todo_list, new_entry)
  end

  defp process_message(todo_list, {:entries, caller, date}) do
    # This clause and the next one "return" values to the caller, AND have to maintain
    # the state of the loop function:
    send(caller, {:todo_entries, TodoList.entries(todo_list, date)})
    todo_list
  end

  defp process_message(todo_list, {:entries, caller}) do
    send(caller, {:todo_entries, TodoList.entries(todo_list)})
    # The first time I tested this, I forgot to include this last line. This showed up as an
    # error on the *second* time I called `TodoServer.entries(pid)`. Because the first time
    # it was called, the state wasn't returned from this clause, it wiped the server state out.
    todo_list
  end

end

defmodule TodoList do
  defstruct auto_id: 1, entries: %{}

  #instantiates the TodoList struct. Note that the struct has the same
  #name as the module
  # def new(), do: %TodoList{}

  # This is a strict superset of functionality over the above
  def new(entries \\ []) do
    Enum.reduce(
      entries,
      %TodoList{},
      fn entry, todo_list_acc ->
        add_entry(todo_list_acc, entry)
        # could also do: &add_entry(&2, &1) but that feels less clear
      end
    )
  end
  def add_entry(todo_list, entry) do
    # remember, Map.put/3's arguments are (map, key, value)
    # First, add the next id in the sequence to the entry:
    entry = Map.put(entry, :id, todo_list.auto_id)
    # Then add the new entry into the state map, with the id as the key:
    new_entries = Map.put(
      todo_list.entries,
      todo_list.auto_id,
      entry
    )
    # Update the struct (new entries and new sequence value), then return the updated `todo_list` struct
    %TodoList{todo_list |
      entries: new_entries,
      auto_id: todo_list.auto_id + 1
    }
  end

  def entries(todo_list, date) do
    todo_list.entries
    |> Stream.filter(fn {_, entry} -> entry.date == date end)
    |> Enum.map(fn {_, entry} -> entry end)
  end
  def entries(todo_list) do
    todo_list.entries
    |> Enum.map(fn {_, entry} -> entry end)
  end
end
