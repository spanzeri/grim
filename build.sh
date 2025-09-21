#!/bin/bash

OUT_DIR=out
TARGET=grimc
WARNINGS=(
  -Werror -Wall \
  -Wsign-compare \
  -Wshadow \
  -Wpointer-arith \
  -Wstrict-prototypes \
  -Wmissing-prototypes \
  -Wmissing-declarations \
  -Wredundant-decls \
  -Wwrite-strings \
  -Wformat=2 \
  -Winit-self \
  -Wundef \
  -Wbad-function-cast \
  -Wcast-qual \
  -Wconversion \
  -Wfloat-equal \
  -Wuninitialized \
)

mkdir -p ${OUT_DIR}
clang -o ${OUT_DIR}/${TARGET} grimc/grimc.c -O0 -g ${WARNINGS[@]} -lm

