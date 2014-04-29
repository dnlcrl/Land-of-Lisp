# Land of Lisp

A journey into the [Land of Lisp](http://landoflisp.com)

## 0. Install CLISP

Linux:

	apt-get install clisp

Mac OS (using [Homerew](http://brew.sh/‎)):

	brew install clisp

## 1. Staritng Up Lisp

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

## 2. Definitions (The Guess-My-Number Game)

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

## 3. Datatypes

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


## 4. Conditions

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

important observations:
 - Only one of the expressions after the if is actually evaluated. We can only - do one thing in an if statement.

### Going Beyond if: The *when* and *unless* Alternatives

Since it’s kind of a pain to use progn every time you want to do multiple things inside an if, Lisp has several other commands that include an implicit progn. The most basic of these are when and unless:

With when, all the enclosed expressions are evaluated when the condition is true. With unless, all the enclosed expressions are
evaluated when the condition is false. The trade-off is that these commands can’t do anything when the condition evaluates in the opposite way; they just return nil and do nothing.

	> (defvar *number-is-odd* nil) > (when (oddp 5)
	(setf *number-is-odd* t) 'odd-number)
	ODD-NUMBER
	> *number-is-odd* T
	> (unless (oddp 4)
	(setf *number-is-odd* nil) 'even-number)
	EVEN-NUMBER
	> *number-is-odd*

### The *cond* form

The cond form is the classic way to do branching in Lisp. Through the liberal use of parentheses, it allows for an implicit progn, can handle more than one branch, and can even evaluate several conditions in succession.

	> (defvar *arch-enemy* nil)
	> (defun pudding-eater (person)
	(cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien) 
	'(curse you lisp alien - you ate my pudding))

	((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny) 
	'(i hope you choked on my pudding johnny))

	(t '(why you eat my pudding stranger ?))))

	> (pudding-eater 'johnny)
	(I HOPE YOU CHOKED ON MY PUDDING JOHNNY) 
	> *arch-enemy*
	JOHNNY
	> (pudding-eater 'george-clooney)
	(WHY YOU EAT MY PUDDING STRANGER ?)

### The *case* form

	> (defun pudding-eater (person)
	(case person
	((henry) (setf *arch-enemy* 'stupid-lisp-alien)
	'(curse you lisp alien - you ate my pudding))
	((johnny) (setf *arch-enemy* 'useless-old-johnny)
	'(i hope you choked on my pudding johnny))
	(otherwise '(why you eat my pudding stranger ?))))

Because the case command uses eq for comparisons, it is usually used only for branching on symbol values. It cannot be used to branch on string values, among other things.

### *and* and *or*

	> (defparameter *is-it-even* nil)
	*IS-IT-EVEN*
	> (or (oddp 4) (setf *is-it-even* t))
	T
	> *is-it-even*
	T

If we do the same thing using an odd number, the variable remains unchanged:
	
	> (defparameter *is-it-even* nil) 
	*IS-IT-EVEN*
	> (or (oddp 5) (setf *is-it-even* t)) 
	T
	> *is-it-even*
	NIL

Lisp uses shortcut Boolean evaluation. This means that once Lisp determines that an earlier statement in a list of or values is true, it simply returns true and doesn’t bother evaluating the remaining statements. 


### Functions That Return More than Just the Truth

*member* can be used to check for list membership for an item:

	> (if (member 1 '(3 4 1 5)) 
	'one-is-in-the-list 
	'one-is-not-in-the-list)

	'ONE-IS-IN-THE-LIST

Anyway it not returns t or f but the tail of the list starting from the wanted value:

	> (if (member nil '(3 4 nil 5)) 
	'nil-is-in-the-list 
	'nil-is-not-in-the-list)
	'nil-is-in-the-list

This works beacuse *member* returns the tail (not the only *nil*) and all things other than nil is true.

One function that really benefits from rich return values is *find-if*, as follows: 

	> (find-if #'oddp '(2 4 5 6))
	5

	> (if (find-if #'oddp '(2 4 5 6)) 
	'there-is-an-odd-number 
	'there-is-no-odd-number)

	'there-is-an-odd-number

### Comparing Staff

Conrad’s Rule of Thumb for Comparing Stuff

 1. use *eq* to compare symbols
 2. use *equal* for everything else

The eql command is similar to the eq command, but unlike eq, it also handles comparisons of numbers and characters:

	;;comparing symbols > (eql 'foo 'foo)
	T
	;;comparing numbers
	> (eql 3.4 3.4)
	T
	;;comparing characters > (eql #\a #\a)
	T

The equalp command is essentially the same as the equal command, except that it can handle some difficult comparison cases
with a bit of extra sophistication. For instance, it can compare strings with different capitalizations and can compare integers against floating-point numbers:

	;;comparing strings with different CAPS
	> (equalp "Bob Smith" "bob smith")
	T
	;;comparing integers against floating point numbers > (equalp 0 0.0)
	T

## 5. Building a Text Game Engine

The Wizard's Adventure Game
In this game, you are a wizard’s apprentice. You’ll explore the wizard’s house. When I complete the game (in Chapter 17), I’ll be able to solve puzzles and win a magical donut :grin:

The world inside our adventure game is very simple, containing only three locations. Let’s first create a top-level variable, *nodes*, to contain descriptions of the locations that exist in our game:
	
	(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
	(garden (you are in a beautiful garden.
	there is a well in front of you.))
	(attic (you are in the attic.
	there is a giant welding torch in the corner.))))


	
