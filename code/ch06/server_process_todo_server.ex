# This file is the assignment from page 166 - refactoring the Todo server from Ch 5 to use
# the server process pattern. I'm choosing to use the one with Process.register/2 as an extra
# exercise.
defmodule ServerProcess do
  # this registers a "module attribute" which we will use later
  # @process_name :process_name

  def start(callback_module) do
    # register the server process with the name from the attribute. Registering
    # *requires* an atom.
    spawn(fn ->
      initial_state = callback_module.init()
      loop(callback_module, initial_state)
    end)
  end

  def call(server_pid, request) do
    send(server_pid, {:call, request, self()})
    receive do
      {:response, response} -> response
    end
  end

  def cast(server_pid, request) do
    send(server_pid, {:cast, request})
  end

  # non-interface code below here
  defp loop(callback_module, state) do
    receive do
      {:call, request, caller} ->
        {response, new_state} = callback_module.handle_call(request, state)
        send(caller, {:response, response})
        loop(callback_module, new_state)
      {:cast, request} ->
        new_state = callback_module.handle_cast(request, state)
        loop(callback_module, new_state)
    end
  end

end

defmodule TodoList do
  defstruct auto_id: 1, entries: %{}

  # callback functions
  def init do
    %TodoList{}
  end

  def handle_call({:entries, date}, state) do
    entry = state.entries
      |> Stream.filter(fn {_, entry} -> entry.date == date end)
      |> Enum.map(fn {_, entry} -> entry end)
    {entry, state}
  end

  def handle_cast({:add_entry, entry}, state) do
    entry = Map.put(entry, :id, state.auto_id)
    new_entries = Map.put(
      state.entries,
      state.auto_id,
      entry
    )
    %TodoList{state |
      entries: new_entries,
      auto_id: state.auto_id + 1
    }
  end

  # interface functions:
  def start do
    ServerProcess.start(TodoList)
  end

  def add_entry(pid, entry) do
    ServerProcess.cast(pid, {:add_entry, entry})
  end

  def entries(pid, date) do
    ServerProcess.call(pid, {:entries, date})
  end
  # TODO: re-add this clause
  # def entries(todo_list) do
  #   todo_list.entries
  #   |> Enum.map(fn {_, entry} -> entry end)
  # end
end
