The primary difference between the getchar() and getc() is that the getc() is capable of reading from any input scheme, while the getchar() is capable of reading from the standard input. Hence, getchar() becomes equivalent to the getc(stdin).

So just use a different input when a flag is set

std::cout << "Enter characters (press Ctrl+D or Ctrl+Z to end):" << std::endl;

int ch;
while ((ch = getc(stdin)) != EOF) {
std::cout << static_cast<char>(ch); // Print the character
}

std::cout << "End of input." << std::endl;
return 0;
