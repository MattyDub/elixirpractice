defmodule TodoCacheTest do
  # Hooks into ex_unit, the Elixir testing framework. Also injects some code into
  # the environment, including the `test` macro used below.
  use ExUnit.Case

  # Use the `test` macro:
  test "server_process" do
    {:ok, cache} = Todo.Cache.start()
    foo_pid = Todo.Cache.server_process(cache, "foo")

    # NB that `assert` is also a macro. See the definition here:
    # https://github.com/elixir-lang/elixir/blob/main-latest/lib/ex_unit/lib/ex_unit/assertions.ex#L131
    assert foo_pid != Todo.Cache.server_process(cache, "bar")
    assert foo_pid == Todo.Cache.server_process(cache, "foo")
  end

  test "to-do operations" do
    {:ok, cache} = Todo.Cache.start()
    alice = Todo.Cache.server_process(cache, "alice")
    Todo.Server.add_entry(alice, %{date: ~D[2024-12-04], title: "Do the thing"})
    entries = Todo.Server.entries(alice, ~D[2024-12-04])
    # Note that we are using pattern matching here; we are "unpacking" only the fields we care about:
    assert [%{date: ~D[2024-12-04], title: "Do the thing"}] = entries
  end
end
