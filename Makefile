CC := clang
CFLAGS := \
	-std=c11 \
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
	-Wuninitialized

LDFLAGS := -lm
TARGET := bin/grimc
SRCS := $(wildcard grimc/*.c) $(wildcard grimc/*.h) Makefile

.PHONY: all clean debug release

all: debug

debug: CFLAGS += -g -O0 -DDEBUG
debug: $(TARGET)
	@echo "Debug build complete: $(TARGET)"

release: CFLAGS += -O3 -DNDEBUG -g
release: $(TARGET)
	@strip --strip-unneeded $(TARGET)
	@echo "Release build complete: $(TARGET)"

# Build the target
$(TARGET): bin $(SRCS)
	@echo "Compiling $(SRC)..."
	$(CC) $(CFLAGS) grimc/grimc.c -o $@ $(LDFLAGS)

# Create bin directory if it doesn't exist
bin:
	@mkdir -p bin

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf bin
