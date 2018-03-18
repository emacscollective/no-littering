Please try to follow the conventions.

     https://github.com/tarsius/no-littering#conventions

In the past the majority of contributors have ignored at least some of the conventions.  Others did not fully understand them or had a good reason to depart from the conventions but did not explain why that is so. Unfortunately it is hard for me as the maintainer to tell whether a contributor did not invest enough time to get things right or just forgot to be explicit about their thought process. The result is that a pull request is actually more work for me than a simple "please theme PACKAGE from URL".

Going forward contributors are expected to follow the conventions more closely from the get-go and to be explicit about their thought process. Adding such statements to commit messages, would be helpful for example:

   - This file is used to store an s-expression.
   - This file is used to store raw text.
   - This is the only configuration/data file of the package.
   - This package does/doesn't take care of creating the containing
     directory if necessary. (If the package does not do it, then you
     should also fix that and submit an upstream pull request.)

Also please link to the repository of the package that your pull request is theming.

Thanks!
