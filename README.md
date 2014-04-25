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

In Common Lisp, functions are defined with *defun*, like this:

	(defun function_name (arguments) 
	...)

The first function we define is *guess-my-number*.

	> (defun guess-my-number ()
	 (ash (+ *small* *big*) -1))
	GUESS-MY-NUMBER

The built-in Lisp function ash looks at a number in binary form, and then shifts its binary bits to the left or right, dropping any bits lost in the process. For example, the number 11 written in binary is 1011. We can move the bits in this number to the left with ash by using 1 as the second argument. We can move the bits to the right (and lop off the bit on the end) by passing in −1 as the second argument.
By using the ash function in guess-my-number, we are continually halving our search space of possible numbers to quickly narrow down to the final correct number.
Now we’ll write our smaller and bigger functions.

	> (defun smaller ()
	  (setf *big* (1- (guess-my-number)))
	  (guess-my-number))
	SMALLER


	> (defun bigger ()
	  (setf *small* (1+ (guess-my-number))) 
	  (guess-my-number))
	BIGGER

we use the setf function to change the value of our global variable \*big\*.
Now we add the function start-over to reset our global variables:

	> (defun start-over () 
	(defparameter *small* 1)
	(defparameter *big* 100)
	(guess-my-number))

# 3 Datatypes

There are lists which are composed by cons, every data is composed by cons, 
there are two magic built in functions to select an element in a data structure:
*car* and *cdr*, the first will return the first element of a list, the second one will return all the other elements. They can be merged til five times, in example:

	> (car '((peas carrots tomatoes) (pork beef chicken))) 
	(PEAS CARROTS TOMATOES)
	> (cdr '(peas carrots tomatoes)) 
	(CARROTS TOMATOES)
	> (cdr (car '((peas carrots tomatoes) (pork beef chicken)))) 
	(CARROTS TOMATOES)
	> (cdar '((peas carrots tomatoes) (pork beef chicken))) 
	(CARROTS TOMATOES)


# 4 Conditions

Let’s look at a common list-eating function, which calculates the length of a list.

	> (defun my-length (list) 
	(if list
	(1+ (my-length (cdr list))) 
	0))
	> (my-length '(list with four symbols)) 
	4

(), '(), nil and 'nil are equivalent to F.

	> (eq '() nil)
	T 
	> (eq '() ()) 
	T
	> (eq '() 'nil)
	T

The if command can be used to make different things happen when things are true (such as when 1 + 2 = 3) or false (such as when 1 + 2 = 4).

	> (if (= (+ 1 2) 3) 
	'yup
	'nope)
	YUP
	> (if (= (+ 1 2) 4) 
	'yup
	'nope)
	NOPE

The if command can also be used to check whether a list is empty: 
	
	> (if '(1)
	'the-list-has-stuff-in-it 
	'the-list-is-empty)

	THE-LIST-HAS-STUFF-IN-IT

	> (if '() 
	'the-list-has-stuff-in-it 
	'the-list-is-empty)
	THE-LIST-IS-EMPTY

