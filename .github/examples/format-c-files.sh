#!/bin/bash

FILES=( $(find . -name '*.c' -type f) )
clang-format --Werror --style=Microsoft --sort-includes=0 -i ${FILES[@]}
