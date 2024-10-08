# This module adapts the server_process module to use "casts" (asynchronous messages).
defmodule ServerProcess do
  def start(callback_module) do
    spawn(fn ->
      initial_state = callback_module.init()
      loop(callback_module, initial_state)
    end)
  end

  # In OTP, "call" is synchronous and "cast" is async
  def call(server_pid, request) do
    send(server_pid, {:call, request, self()})
    receive do
      {:response, response} -> response
    end
  end

  def cast(server_pid, request) do
    send(server_pid, {:cast, request}) # don't need to pass self() since this is fire-and-forget
  end

  defp loop(callback_module, current_state) do
    receive do
      {:call, request, caller} ->
        {response, new_state} = callback_module.handle_call(request, current_state)
        send(caller, {:response, response})
        loop(callback_module, new_state)
      # ":cast" is fire-and-forget, so we just call the handler in the callback module and recur
      {:cast, request} ->
        new_state = callback_module.handle_cast(request, current_state)
        loop(callback_module, new_state)
    end
  end
end

defmodule KeyValueStore do
  # interface functions
  def start do
    ServerProcess.start(KeyValueStore)
  end

  def put(pid, key, value) do
    # Putting things in the KV store can be asynchronous, so we use cast
    ServerProcess.cast(pid, {:put, key, value})
  end

  def get(pid, key) do
    # We want to get the response for get/2, so this is synchronous
    ServerProcess.call(pid, {:get, key})
  end

  # callback functions
  def init do
    %{}
  end

  def handle_call({:get, key}, state) do
    {Map.get(state, key), state}
  end

  # Don't need this anymore, since :put is asynchronous; commenting out to show
  # the delta from server_process.ex
  # def handle_call({:put, key, value}, state) do
  #   {:ok, Map.put(state, key, value)}
  # end

  def handle_cast({:put, key, value}, state) do
    Map.put(state, key, value)
  end
end
