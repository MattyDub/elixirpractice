defmodule TodosupervisorTest do
  use ExUnit.Case
  doctest Todosupervisor

  test "greets the world" do
    assert Todosupervisor.hello() == :world
  end
end
