cmake_minimum_required(VERSION 3.14)

include(cmake/prelude.cmake)

project(
    boa-programming-language
    VERSION 0.1.0
    DESCRIPTION "The boa programming language"
    HOMEPAGE_URL "https://github.com/nebler/boa"
    LANGUAGES CXX
)

include(cmake/project-is-top-level.cmake)
include(cmake/variables.cmake)

# ---- Find and Configure LLVM Package ----
find_package(LLVM REQUIRED CONFIG)

# Set the LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})

# Set LLVM flags
add_definitions(${LLVM_DEFINITIONS})

# Add additional flags
add_compile_options(-funwind-tables -fno-rtti -DBUILD_EXAMPLES -D_DEBUG -D_GLIBCXX_ASSERTIONS -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS)

# ---- Declare library ----
add_library(
    boa-programming-language_lib OBJECT
    source/lib.cpp
    source/lexer/arguments/argument_tokenizer.cpp
    source/lexer/input/cli_reader.cpp
    source/lexer/input/file_reader.cpp
)

target_include_directories(
    boa-programming-language_lib ${warning_guard}
    PUBLIC
    "\$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/source>"
)

target_compile_features(boa-programming-language_lib PUBLIC cxx_std_17)

# ---- Declare executable ----
add_executable(boa-programming-language_exe source/main.cpp)
add_executable(boa-programming-language::exe ALIAS boa-programming-language_exe)

set_property(TARGET boa-programming-language_exe PROPERTY OUTPUT_NAME boa-programming-language)

target_compile_features(boa-programming-language_exe PRIVATE cxx_std_17)

# Link the executable with the necessary LLVM libraries
llvm_map_components_to_libnames(LLVM_LIBS core orcjit native)
target_link_libraries(boa-programming-language_exe PRIVATE boa-programming-language_lib ${LLVM_LIBS} -L/opt/homebrew/Cellar/zstd/1.5.6/lib)

# ---- Install rules ----
if(NOT CMAKE_SKIP_INSTALL_RULES)
  include(cmake/install-rules.cmake)
endif()

# ---- Developer mode ----
if(NOT boa-programming-language_DEVELOPER_MODE)
  return()
elseif(NOT PROJECT_IS_TOP_LEVEL)
  message(
      AUTHOR_WARNING
      "Developer mode is intended for developers of boa-programming-language"
  )
endif()

include(cmake/dev-mode.cmake)
