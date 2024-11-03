#!/usr/bin/env bash
# USAGE: `./iexmix.sh [path to mix project]`
# e.g. `./iexmix.sh ch07/todo`
# NOTE that the `/code/code` is already prepended to the path
MSYS_NO_PATHCONV=1 docker exec -it -w /code/code/$1 elixirpractice-base-1 sh -c "iex -S mix"
