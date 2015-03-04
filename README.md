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
* It's not well tested.
* It has problems with Unicode on Windows.


TODO
----
* [ ] Unicode support, including regex
* [x] QuickCheck
* [ ] Automated test (HUnit? Compare result with GNU find?)
* [ ] Support time format 'YYYYmmddHHMMSS'
* [ ] Support time delta, '-21H27M' would mean 21 hours 27 minutes ago
* [ ] Support file content as a column (GNU grep)
* [ ] Benchmark and performance
