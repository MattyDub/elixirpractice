#!/usr/bin/env bash
# USAGE: `./mixnew.sh [path to mix project]`
# e.g. `./mixnew.sh ch08/todosupervisor`
# NOTE that the `/code/code` is already prepended to the path
MSYS_NO_PATHCONV=1 docker exec -it -w /code/code/ elixirpractice-base-1 sh -c "mix new $1"
