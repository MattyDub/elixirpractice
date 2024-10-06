# This file is a simple refactor of todo_server.ex; the code is almost exactly the same
# but I changed it to use a registered process for the server (pg 152-153). I've removed
# some redundant comments; see that file for more notes to self.

defmodule TodoServer do
  # this registers a "module attribute" which we will use later
  @process_name :process_name

  def start do
    # register the server process with the name from the attribute. Registering
    # *requires* an atom.
    Process.register(spawn(fn -> loop(TodoList.new()) end), @process_name)
  end

  def add_entry(new_entry) do
    # Now we can send to the registered process without requiring the caller
    # to remember the pid
    send(@process_name, {:add_entry, new_entry})
  end

  def entries(date) do
    send(@process_name, {:entries, self(), date})
    receive do
      {:todo_entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  def entries() do
    send(@process_name, {:entries, self()})
    receive do
      {:todo_entries, entries} -> entries
    after
      5000 -> {:error, :timeout}
    end
  end

  # non-interface code below here
  defp loop(todo_list) do
    new_todo_list =
      receive do
        message -> process_message(todo_list, message)
      end
    loop(new_todo_list)
  end

  defp process_message(todo_list, {:add_entry, new_entry}) do
    TodoList.add_entry(todo_list, new_entry)
  end

  defp process_message(todo_list, {:entries, caller, date}) do
    send(caller, {:todo_entries, TodoList.entries(todo_list, date)})
    todo_list
  end

  defp process_message(todo_list, {:entries, caller}) do
    send(caller, {:todo_entries, TodoList.entries(todo_list)})
    todo_list
  end

end

defmodule TodoList do
  defstruct auto_id: 1, entries: %{}

  def new(entries \\ []) do
    Enum.reduce(
      entries,
      %TodoList{},
      fn entry, todo_list_acc ->
        add_entry(todo_list_acc, entry)
      end
    )
  end

  def add_entry(todo_list, entry) do
    entry = Map.put(entry, :id, todo_list.auto_id)
    new_entries = Map.put(
      todo_list.entries,
      todo_list.auto_id,
      entry
    )
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
