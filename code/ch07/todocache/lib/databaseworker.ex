# This is part of the exercise described on pg. 198-199. Persisting the data
# via a process like this allows for synchronizing access to a particular
# Todo list, as only this process will be able to access it. The Todo.Database
# module calls this, which does the actual synchronization.
defmodule Todo.DatabaseWorker do
  use GenServer

  # Note we don't use a :name key, as we need multiple of these workers.
  def start(folder_name) do
    GenServer.start(__MODULE__, folder_name)
  end

  # And also remember, future me, that if you don't register with a name,
  # you need to pass in the pid that was returned from init/1.
  def store(worker, key, data) do
    GenServer.cast(worker, {:store, key, data})
  end

  def get(worker, key) do
    GenServer.call(worker, {:get, key})
  end

  @impl GenServer
  def init(db_folder) do
    File.mkdir_p!(db_folder)
    {:ok, db_folder}
  end

  @impl GenServer
  def handle_cast({:store, key, data}, db_folder) do
    key
    |> file_name(db_folder)
    |> File.write!(:erlang.term_to_binary(data))
    # IO.puts("pid: #{inspect(self())} stores: {#{inspect(key)} => #{inspect(data)}}")

    {:noreply, db_folder}
  end

  @impl GenServer
  def handle_call({:get, key}, _, db_folder) do
    data = case File.read(file_name(key, db_folder)) do
      {:ok, contents} -> :erlang.binary_to_term(contents)
      _ -> nil
    end
    {:reply, data, db_folder}
  end

  defp file_name(key, db_folder) do
    Path.join(db_folder, to_string(key))
  end
end
