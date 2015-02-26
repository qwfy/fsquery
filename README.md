What is FSQuery
---------------
FSQuery is a command line tool, which allows you to find files in file system using a SQL-like language.


Installation
------------
This program is not on hackage, you need to clone this repo to use it. (Don't install it globally, it's not well tested.)

    $ git clone https://github.com/qwfy/fsquery.git ./FSQuery
    $ cd FSQuery
    $ cabal sandbox init
    $ cabal install

Then, you will find the executable in dist/dist-sandbox-xxxxxxxx/build/FSQuery/.


Usage
-----

To show help message, including the format of SQL, use `-h`, or `--help`:

    fsquery {-h|--help}

To enter REPL mode, specify no arguments:

    fsquery

To read query from stdin, use `-` as argument:

    fsquery -

Query can also be supplied in the argument, like this:

    fsquery 'select path, size from ./foo;'


Issues
-------
* Currently, there is no automated test for this program. If it type-checks, it's probably correct, so they say...
* It has problems with Unicode.
