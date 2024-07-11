#include "lib.hpp"

auto main() -> int
{
  auto const lib = library {};

  return lib.name == "boa-programming-language" ? 0 : 1;
}
