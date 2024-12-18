# This file contains some code snippets to show how to do some things.
# The intent is more to copy and paste these into iex than to run this file as a script.

# Supervisor start/kill/restart example:
Supervisor.start_link([Todo.Cache], strategy: :one_for_one) # :one_for_one restarts the process 1:1 when it is shut down
cache_pid = Process.whereis(Todo.Cache) # returns the PID for Todo.Cache.
Process.exit(cache_pid, :kill) # :kill exits even if the process is trapping exits
Process.whereis(Todo.Cache) # you can see this returns a different PID than was returned earlier.
# NB: When using Supervisors, we can find the relevant process even if the PID has changed IFF we
# register the process. Supervisors use names, which persist across process restarts, instead of PIDs, which don't.
# Names also allow for discovery via Process.whereis/1
