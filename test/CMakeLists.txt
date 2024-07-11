# Parent project does not export its library target, so this CML implicitly
# depends on being added from it, i.e. the testing is done only from the build
# tree and is not feasible from an install location

project(boa-programming-languageTests LANGUAGES CXX)

# ---- Tests ----

add_executable(boa-programming-language_test source/boa-programming-language_test.cpp)
target_link_libraries(boa-programming-language_test PRIVATE boa-programming-language_lib)
target_compile_features(boa-programming-language_test PRIVATE cxx_std_17)

add_test(NAME boa-programming-language_test COMMAND boa-programming-language_test)

# ---- End-of-file commands ----

add_folders(Test)
