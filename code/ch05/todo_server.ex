# We put both modules in here because we don't get to mix until Ch 07.

defmodule TodoServer do
end

defmodule TodoList do
  defstruct auto_id: 1, entries: %{}

  #instantiates the TodoList struct. Note that the struct has the same
  #name as the module
  def new(), do: %TodoList{}

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
