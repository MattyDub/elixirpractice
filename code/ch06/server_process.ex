# This module shows how a GenServer might be implemented: it calls hooks
# provided by a callback module, and handles the "generic server" parts
# (e.g. request/response stuff) itself.
defmodule ServerProcess do
  def start(callback_module) do
    # Pedagogically, why didn't the author use registered processes here?
    spawn(fn ->
      initial_state = callback_module.init()
      loop(callback_module, initial_state)
    end)
  end

  # call/2 and loop/2 should look very familiar after ch 05, as these are
  # now well-established patterns.
  def call(server_pid, request) do
    send(server_pid, {request, self()})
    receive do
      {:response, response} -> response
    end
  end

  defp loop(callback_module, current_state) do
    receive do
      {request, caller} ->
        {response, new_state} = callback_module.handle_call(request, current_state)
        send(caller, {:response, response})
        loop(callback_module, new_state)
    end
  end
end

# This is the "callback" module. It implements the specific hooks that the generic
# server needs; in this case, that's init/0 and handle_call/2.
defmodule KeyValueStore do
  # interface functions
  def start do
    ServerProcess.start(KeyValueStore)
  end

  def put(pid, key, value) do
    ServerProcess.call(pid, {:put, key, value})
  end

  def get(pid, key) do
    ServerProcess.call(pid, {:get, key})
  end

  # callback functions
  def init do
    %{}
  end

  def handle_call({:get, key}, state) do
    {Map.get(state, key), state}
  end

  def handle_call({:put, key, value}, state) do
    {:ok, Map.put(state, key, value)}
  end
end
