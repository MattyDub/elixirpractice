# elixirpractice

This repository contains my work as I'm learning [Elixir](https://elixir-lang.org) via the book [Elixir in Action](https://www.manning.com/books/elixir-in-action) by Saša Jurić. A lot of the code has comments that are my notes to myself as a mnemonic/pedagogical aid. If anybody ever sees this and finds it helpful, great, but it's really more for my own learning process.

## To use
You probably won't want to use anything here, but if you do, try this:

1. Clone the repo
1. `cd` into the directory where you cloned it
1. Run `docker compose up`
1. In another terminal (or in the same terminal if you used the [`-d` param](https://docs.docker.com/reference/cli/docker/compose/up/) for `docker compose up`), run `./iex.sh <path to the .exs file you want to run>`. This will start `iex` with your .exs file loaded. Then you can interact with it as you want.

## Notes
I'm currently developing on Windows, and I'm trying out "Git Bash" as a shell after reading [Invisible Ink's post about it](https://www.ii.com/git-bash-is-my-preferred-windows-shell/). One hiccup I ran into was that MSYS2 was prepending Windows-specific path information to references to `bash` in `shell.sh`. Thanks to [this gist](https://github.com/borekb/docker-path-workaround), I found out about the `MSYS_NO_PATHCONV` environment variable. That's why it's in the shell scripts.

## Notes to self:

1. In `iex`, you can run `:application.get_key(:<ModuleName>, :modules)` to see the list of loaded modules. This was helpful when I accidentally launched `iex` in the wrong directory, and got a "module <Foo> is not available" error message.
1. In `iex`, `:erlang.system_info(:process_count)` returns how many processes are running. This is calling [Erlang's `system_info`](https://www.erlang.org/doc/apps/erts/erlang.html#system_info/1) function. Presumably `:erlang.foo()` calls the corresponding Erlang function, but I can't find documentation that confirms this (searching for e.g. "elixir erlang module" returns a lot, none of it specifically relevant).