Tachyon is a metacircular VM for JavaScript (ECMAScript 5) written in
JavaScript. Please note that the project is still in the early stages, and,
as such, may be lacking in features or contain bugs.

INSTALLATION
------------

Requirements:
- A PC running Linux/Unix/MacOS
- An up to date version of gcc
- Build tools such as make

To install Tachyon:

1. Clone the Tachyon repository you have found this README file in. You've
probably already done this. If not, the command is:

    git clone <tachyon_repo_url>

2. Run the install script. This will fetch the code from the common repository
which contains our unit tests, parser and standard libraries. It will also
fetch the V8 JavaScript VM, install the Tachyon extensions onto it, and compile
32 and 64 bit binaries for your system.

    ./install

3. Run make to build final dependencies for Tachyon

    make

4. You are done. You may test that Tachyon works properly by running its unit
tests using "make test", and "make test64" (for 64-bit compilation). This
should run all unit tests and display "All tests passed." when done.

    make test
    make test64     (64-bit machines only)

You may run the Tachyon read-eval print loop by running the tachyon script:

    ./tachyon


CONTACTING US
-------------

For assistance or other inquiries about Tachyon, you may contact
Maxime Chevalier-Boisvert, maintainer of the Tachyon VM at chevalma at iro
dot umontreal dot ca.

