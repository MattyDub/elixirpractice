defmodule DatabaseTest do
  use ExUnit.Case

  test "database" do
    {:ok, _} = Todo.Database.start()
    test_data = %{a: 1, b: 2, c: %{d: 4}}
    Todo.Database.store("basic_db_test", test_data)
    assert test_data = Todo.Database.get("basic_db_test")
  end
end
