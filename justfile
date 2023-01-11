set positional-arguments := true

# shows all recipes (is also executed if recipe not specified, i.e. `just`)
default:
    @just --list

# sets up the watch screen; `just watch X` filters tests related to X
watch what="":
    cargo watch --exec "test {{ what }}" --ignore "**/snapshots/**" --clear

# reviews golden tests
review:
    cargo insta review

# formats all the code plus scripts
fmt:
    cargo fmt
    just --fmt --unstable
