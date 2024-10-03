defmodule DatabaseServer do
    # This is a pretty simple example of a database storing state. The state doesn't change.
    # There's really not much to see here, see calculator.ex for something showing state
    # management. This is from pg 144-145.

    # Start the listening process; create a mocked "connection" (e.g. from an ODBC pool)
    # and use that for future queries
    def start do
        spawn(fn ->
            connection = :rand.uniform(1000)
            loop(connection)
        end)
    end

    # Listen for the :run_query message, and if you get it, call run_query and send
    # the result back to the sender. Note the use of the "connection" in the query.
    defp loop(connection) do
        receive do
            {:run_query, caller, query_def} ->
                send(caller, {:query_result, run_query(connection, query_def)})
        end
        loop(connection)
    end

    # Simulates a long-running query to demonstrate asynchronous/concurrent behavior
    # If we were using a real db connection, this is where that would happen.
    # Note that the state management originated in start/0.
    defp run_query(connection, query_def) do
        Process.sleep(2000)
        "Connection #{connection}: #{query_def} result"
    end

    # Note that the two "interface" functions don't interact with the state at all.
    # Send the query start message to the listening process
    def run_async(server_pid, query_def) do
        send(server_pid, {:run_query, self(), query_def})
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
