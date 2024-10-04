# elixirpractice

This repository contains my work as I'm learning [Elixir](https://elixir-lang.org) via the book [Elixir in Action](https://www.manning.com/books/elixir-in-action) by Saša Jurić.

## To use
You probably won't want to use anything here, but if you do, try this:

1. Clone the repo
1. `cd` into the directory where you cloned it
1. Run `docker compose up`
1. In another terminal (or in the same terminal if you used the [`-d` param](https://docs.docker.com/reference/cli/docker/compose/up/) for `docker compose up`), run `./iex.sh <path to the .exs file you want to run>`. This will start `iex` with your .exs file loaded. Then you can interact with it as you want.

## Notes
I'm currently developing on Windows, and I'm trying out "Git Bash" as a shell after reading [Invisible Ink's post about it](https://www.ii.com/git-bash-is-my-preferred-windows-shell/). One hiccup I ran into was that MSYS2 was prepending Windows-specific path information to references to `bash` in `shell.sh`. Thanks to [this gist](https://github.com/borekb/docker-path-workaround), I found out about the `MSYS_NO_PATHCONV` environment variable. That's why it's in the shell scripts.
