#!/bin/bash

# Make sure to uncomment the inclusion of `precompiled.jl` from `src/SPIRV.jl`,
# otherwise we'll be missing a lot of precompile directives.

pkg=$1
if [[ "$pkg" = "" ]]; then
  echo "Package name expected as first argument"
  exit 1
fi

tmp=/tmp/__${pkg}_compiled.jl
dst="src/precompilation_traces.jl"

echo "Running tests with --trace-compile=$tmp"
julia --color=yes --project --startup-file=no --trace-compile=$tmp test/runtests.jl

echo "Writing precompile statements at $dst"
julia --color=yes --startup-file=no -e"str = read(\"$tmp\", String); open(\"$dst\", \"w+\") do io; for line in split(str, '\n'); contains(line, \"$pkg\") && !contains(line, \"Main\") && println(io, line); end; end"
