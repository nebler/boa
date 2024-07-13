cmake_minimum_required(VERSION 3.14)

project(
    boa-programming-language
    VERSION 0.1.0
    DESCRIPTION "The boa programming language"
    HOMEPAGE_URL "https://github.com/nebler/boa"
    LANGUAGES CXX C
)

# Set C++ standard to C++17
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ---- Find and Configure LLVM Package ----
find_package(LLVM REQUIRED CONFIG)

# Set the LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})

# Set LLVM flags
add_definitions(${LLVM_DEFINITIONS})

# Add additional compiler flags
add_compile_options(
    -funwind-tables
    -fno-rtti
    -DBUILD_EXAMPLES
    -D_DEBUG
    -D_GLIBCXX_ASSERTIONS
    -D__STDC_CONSTANT_MACROS
    -D__STDC_FORMAT_MACROS
    -D__STDC_LIMIT_MACROS
)

# ---- Declare library ----
add_library(
    boa-programming-language_lib OBJECT
    source/lib.cpp
    source/lexer/arguments/argument_tokenizer.cpp
    source/lexer/input/cli_reader.cpp
    source/lexer/input/file_reader.cpp
)

# Set include directories for the library
target_include_directories(
    boa-programming-language_lib
    PUBLIC
    ${PROJECT_SOURCE_DIR}/source
)

# Specify C++ features used by the library
target_compile_features(boa-programming-language_lib PUBLIC cxx_std_17)

# ---- Declare executable ----
add_executable(boa-programming-language_exe source/main.cpp)

# Set properties for the executable
set_property(TARGET boa-programming-language_exe PROPERTY OUTPUT_NAME boa-programming-language)

# Specify C++ features used by the executable
target_compile_features(boa-programming-language_exe PRIVATE cxx_std_17)

# Link the executable with the necessary LLVM libraries
llvm_map_components_to_libnames(LLVM_LIBS core orcjit native)
target_link_libraries(boa-programming-language_exe PRIVATE boa-programming-language_lib ${LLVM_LIBS})

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
