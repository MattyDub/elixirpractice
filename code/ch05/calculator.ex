defmodule Calculator do
  # Initialize the server with the state (this follows the pattern introduced in
  # stateful_database_server).
  def start do
    spawn(fn -> loop(0) end)
  end

  # Note the state management pattern: generate new value, then pass it in the tail call
  defp loop(current_value) do
    new_value =
      receive do
        {:value, caller} ->
          send(caller, {:response, current_value})
          current_value

        {:add, value} -> current_value + value
        {:sub, value} -> current_value - value
        {:mul, value} -> current_value * value
        {:div, value} -> current_value / value

        invalid_request ->
          IO.puts("invalid request #{inspect(invalid_request)}")
          current_value
      end
    loop(new_value)
  end

  # The interface code is below here:
  # value/1 synchronously requests the value from the server
  def value(server_pid) do
    send(server_pid, {:value, self()})
    receive do
      {:response, value} -> value
    end
  end

  # These all asynchronously update the value on the server.
  # The order of operations is enforced by the process mailbox queueing.
  def add(server_pid, value), do: send(server_pid, {:add, value})
  def sub(server_pid, value), do: send(server_pid, {:sub, value})
  def mul(server_pid, value), do: send(server_pid, {:mul, value})
  def div(server_pid, value), do: send(server_pid, {:div, value})
end
