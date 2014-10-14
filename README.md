EarlTheIRCbot
=============

My name is Earl! Sombody please take mercy and fork me into somthing better.

# Prerequisites

You will need [rebar](https://github.com/rebar/rebar) and [relx](https://github.com/erlware/relx).

# Configuration
Currently sevrers are compiled into Earl, this will change at some point, but for now the basic
configuration can be found `earl.hrl`, and admins are defined in `earl.erl`.

# Compiling
To compile do `cd earl; rebar compile`

# Testing
To run all eunit tests in the project, do `rebar eunit`

# Build
To build, do `make`. This makes an executable in `_rel` which can be ran...

# Run
To run do `./_rel/earl/bin/earl console`

# Documentation

To make the docs you will need a LaTeX installation. On Ubuntu do:
    sudo apt-get install texlive

Then you can do

```shell
cd doc
make all
```

To make the docs as a pdf.
