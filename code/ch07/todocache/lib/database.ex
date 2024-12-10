defmodule Todo.Database do
  use GenServer

  @db_folder "./persist"

  # This is the solution that Jurić came up with:
  defp choose_worker(key) do
    GenServer.call(__MODULE__, {:choose_worker, key})
  end

  @impl GenServer
  def handle_call({:choose_worker, key}, _, worker_map) do
    key = :erlang.phash2(key, 3)
    {:reply, Map.get(worker_map, key), worker_map}
  end

  # This was my (incorrect) solution to how to create choose_worker. This works,
  # but is not idiomatic - since the worker_map is the state for this process,
  # we should be interacting with the state via call(). I'm leaving this here
  # for pedagogical purposes as an example of what NOT to do.
  # defp choose_worker(worker_map, key) do
  #   key = :erlang.phash2(key, 3)
  #   worker_map[key]
  # end

  def start do
    GenServer.start(__MODULE__, nil, name: __MODULE__)
  end

  def store(key, data) do
    # So this threading approach is the correct way:
    key
    |> choose_worker()
    |> Todo.DatabaseWorker.store(key, data)

    # This was the incorrect way. It's not wrong, it's just not idiomatic.
    # By not interacting with the process state correctly for choose_worker,
    # I kinda painted myself into the corner of using call and cast in
    # Database. Again, leaving this for pedagogical purposes:
    # GenServer.cast(__MODULE__, {:store, key, data})
  end

  def get(key) do
    key
    |> choose_worker()
    |> Todo.DatabaseWorker.get(key)

    # See comments in store/2 above.
    # GenServer.call(__MODULE__, {:get, key})
  end

  @impl GenServer
  def init(_) do
    File.mkdir_p!(@db_folder)
    worker_map = for x <- 0..2, into: %{} do
      {:ok, worker} = Todo.DatabaseWorker.start(@db_folder)
      {x, worker}
    end
    {:ok, worker_map}
  end

  # See comments for choose_worker/1 and store/2 above for why these are here:
  # @impl GenServer
  # def handle_cast({:store, key, data}, worker_map) do
  #   worker = choose_worker(worker_map, key)
  #   Todo.DatabaseWorker.store(worker, key, data)
  #   {:noreply, worker_map}
  # end

  # @impl GenServer
  # def handle_call({:get, key}, _, worker_map) do
  #   worker = choose_worker(worker_map, key)
  #   data = Todo.DatabaseWorker.get(worker, key)
  #   {:reply, data, worker_map}
  # end

end
