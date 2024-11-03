#!/usr/bin/env bash
# USAGE: `./mixtest.sh [path to mix project]`
# e.g. `./mixtest.sh ch07/todo`
# NOTE that the `/code/code` is already prepended to the path
MSYS_NO_PATHCONV=1 docker exec -it -w /code/code/$1 elixirpractice-base-1 sh -c "mix test"
