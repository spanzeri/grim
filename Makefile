CC := clang
CFLAGS := \
	-std=c23 \
	-Werror -Wall -Wextra -Wmost -pedantic \
	-Wno-unused-parameter

LDFLAGS := -lm
TARGET := bin/grimc
SRCS := $(wildcard grimc/*.c) $(wildcard grimc/*.h) Makefile
OBJS := $(SRCS:.c=.o)

.PHONY: all clean debug release

all: debug

debug: CFLAGS += -g -O0 -DDEBUG
debug: $(TARGET)
	@echo "Debug build complete: $(TARGET)"

release: CFLAGS += -O3 -DNDEBUG -g
release: $(TARGET)
	@strip --strip-unneeded $(TARGET)
	@echo "Release build complete: $(TARGET)"

$(TARGET): bin $(SRCS)
	@echo "Compiling $(SRC)..."
	$(CC) $(CFLAGS) grimc/grimc.c -o $@ $(LDFLAGS)

bin:
	@mkdir -p bin

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf bin
