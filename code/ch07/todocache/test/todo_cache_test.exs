defmodule TodoCacheTest do
  # Hooks into ex_unit, the Elixir testing framework. Also injects some code into
  # the environment, including the `test` macro used below.
  use ExUnit.Case

  # Use the `test` macro:
  test "server_process" do
    {:ok, cache} = Todo.Cache.start()
    foo_pid = Todo.Cache.server_process(cache, "foo")

    assert foo_pid != Todo.Cache.server_process(cache, "bar")
    assert foo_pid == Todo.Cache.server_process(cache, "foo")
  end
end
