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

- Move the ast into it's own files and use the visitor pattern in order to clean up the code
- Encapsulate some of the initalization code

Features:

- Different datatypes
- Classes
- Lists/Vectors/Arrays
- Eror Handling
- Parallel Computing
- Async Programming

Infrastructure:

- Some form of actual CI/CD
  - Tests
  - Deployment
  - Playground for the site
