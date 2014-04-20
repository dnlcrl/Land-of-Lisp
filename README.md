# Land of Lisp

A journey into the [Land of Lisp](http://landoflisp.com)

# 0. Install CLISP

Linux:

	apt-get install clisp

Mac OS (using [Homerew](http://brew.sh/‎)):

	brew install clisp

# 1. Staritng Up Lisp

Type **clisp** from your command line

	$ clisp
	  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
	  I I I I I I I      8     8   8           8     8     o  8    8
	  I  \ `+' /  I      8         8           8     8        8    8
	   \  `-+-'  /       8         8           8      ooooo   8oooo
	    `-__|__-'        8         8           8           8  8
	        |            8     o   8           8     o     8  8
	  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

	Welcome to GNU CLISP 2.49 (2010-07-07) <http://clisp.cons.org/>

	Copyright (c) Bruno Haible, Michael Stoll 1992, 1993
	Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
	Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
	Copyright (c) Bruno Haible, Sam Steingold 1999-2000
	Copyright (c) Sam Steingold, Bruno Haible 2001-2010

	Type :h and hit Enter for context help.

	[1]>

Try Typing **(+ 3 (* 2 4))**

	[1]> (+ 3 (* 2 4)) 
	11

When you want to shut down CLISP, just type (quit).

# 2. The Guess-My-Number Game

The game is composed by three functions, *guess-my-number*, *smaller*, and *bigger*. We also need two global variables: \*small\* and \*big\*.

A variable that is defined globally in Lisp is called a top-level definition.

*defparameter* creates new top-level definitions:

	> (defparameter *small* 1) 
	*SMALL*
	> (defparameter *big* 100) 
	*BIG*
