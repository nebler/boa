This is a project original based on the Kaleidscope tutorial.
Additionally I have done the following:

Refactorings:

- Moved the lexer/parser to it's own folder

Featues:

- Added the option to read flags/input when starting the code
- Files can now be used as input
- CLI mode is still available just not default anymore (-i flag when starting)

Ideas/Next step:

Refactorings:

- Move the ast into it's own files
  - the original tutorial https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html#code-generation-setup suggested to maybe use the visitor pattern. I am not too fond of this idea but i still wanna put them in their own files and encapsulate them a little better.
- Encapsulate some of the initalization code
- Encapsulate some of the parsing code

Features:

- Different datatypes
- Classes
- Lists/Vectors/Arrays
- Eror Handling
- Parallel Computing
- Async Programming
- Debugging Options

Infrastructure:

- Some form of actual CI/CD
  - Tests
  - Deployment
  - Playground for the site
