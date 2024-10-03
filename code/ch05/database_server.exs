defmodule DatabaseServer do
    # Start the listening process
    def start do
        spawn(&loop/0)
    end

    # Listen for the :run_query message, and if you get it, call run_query and send
    # the result back to the sender
    defp loop do
        receive do
            {:run_query, caller, query_def} ->
                send(caller, {:query_result, run_query(query_def)})
        end
        loop()
    end

    # Simulates a long-running query to demonstrate asynchronous/concurrent behavior
    # (Not behaviour...yet)
    defp run_query(query_def) do
        Process.sleep(2000)
        "#{query_def} result"
    end

    # Send the query start message to the listening process
    def run_async(server_pid, query_def) do
        send(server_pid, {:run_query, self(), query_def})
    end

    # nothing is listening for this, so it times out with the default timeout
    def run_async_2(server_pid, query_def) do
        send(server_pid, {:dont_care_message, self(), query_def})
    end

    # Listen for the :query_result message, and if you get it, return the result
    def get_result do
        receive do
            {:query_result, result} -> result
        after
            5000 -> {:error, :timeout}
        end
    end
end
