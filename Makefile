CXX = clang++

TARGET ?= my-lang

SRCS ?= my-lang.cpp lexer/tokenizer/tokenizer.cpp lexer/arguments/argument_tokenizer.cpp lexer/input/cli_reader.cpp lexer/input/file_reader.cpp


# Define the necessary LLVM configuration flags
LLVM_CONFIG = llvm-config
LLVM_CXXFLAGS = -I/usr/local/include -std=c++17 -funwind-tables -fno-rtti -DBUILD_EXAMPLES -D_DEBUG -D_GLIBCXX_ASSERTIONS -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --system-libs --libs core orcjit native)

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