install(
    TARGETS boa-programming-language_exe
    RUNTIME COMPONENT boa-programming-language_Runtime
)

if(PROJECT_IS_TOP_LEVEL)
  include(CPack)
endif()
