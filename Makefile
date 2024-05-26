CXX = clang++

TARGET ?= my-lang

SRCS ?= my-lang.cc

# Define the necessary LLVM configuration flags
LLVM_CONFIG = llvm-config
LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --system-libs --libs core)

# extra flag needed for the linker and homebrew
EXTRA_LDFLAGS = -L/opt/homebrew/Cellar/zstd/1.5.6/lib

# Default target
.PHONY: all
all:
	$(CXX) -g -O3 $(SRCS) $(LLVM_CXXFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) $(EXTRA_LDFLAGS) -o $(TARGET)

# Clean the build directory
.PHONY: clean
clean:
	rm -f $(TARGET)
