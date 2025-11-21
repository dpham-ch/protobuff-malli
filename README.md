# Plan

2. Rewrite the read me file with the following content (make it professional, open, but concise).

3. Constraints: the library needs to run in babashka (only clojure code, no Java or JavaScript or command line).

4. Goal is to create a parser of Google protobuff file using Clojure instaparse into metosin/malli hiccup format with the registry format [:schema {:registry …} MessageType]

5. Add instaparse as dependency, add a resources folder.

6. Create a resource folder with example of simple protobuff definition from the official documentation , one file per example (.proto format)

7. For each of the example write the equivalent malli schema (files end with .edn)

8. Write test files in test folders.
