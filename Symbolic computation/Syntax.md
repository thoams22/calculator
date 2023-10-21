# Types

## Elementary

- Number : i64.
- Variables : char.
- Constant : $e, \pi$

## Operators

- Addition : "a + b"
- Negation : "a - b" will be interpreted as "a + (-b)"
- Multiplication : "a * b"
- Exponentiation: "a ^ b" if "b" is compose of multiple [[Syntax#Elementary|elementary]] they must be in parenthesis.
- Fraction : "a/b" if "a" and/or "b" is compose of multiple [[Syntax#Elementary|elementary]] they must be in parenthesis.
- Equality : "a = b"
## Functions

- Predefined : 
	- there is a base of common functions defined Ex : "sin(x)", "log(x, y)" , etc.
- User defined : 
	- Nothing yet just defining a function.

```functions that can be defined in the CLI and be reused after they can take arguments of the types Variable. The functions names are variable followed by a parenthesis, the number of arguments must be constant or an Error will be raised. Ex: a(x) is different than a(x, y) and only one can be defined.
```

## Expression

An expression is a common type that can contains : [[Syntax#Elementary|elementary]] [[Syntax#Operators|operators]] [[Syntax#Functions|functions]] or other expression.

## Statement

A statement precise what to do with an expression.
The different statements are : 
- Simplify : simplify the expression. "4 + 8"
- Solve : "x + 4 = 2"
	- If there is one variable solve for this variable.
	- If there is multiple variable try solve for x.
	- else solve for a random variable in the expression.
- SolveFor : solve for the specified variable "x + 4 - c = 6, c"
- Replace : replace some element in the expression with another expression. "x^4 + x^2, x^2 = y"
 
# Steps of the program

the three step will continue until there is a program error, a panic or "exit" is entered in the CLI. 

## Lexer 
The lexer will convert the string in tokens and raise Error when a char is not supported.

The current char supported are : [a-z] [A-Z] [0-9] , ( ) + - = * ^ end of line wihtespace 
## Parser 

If the lexer does not raise any error.
The parser will convert the tokens returned by the lexer into Statement.

## Solver
If the parser does not raise any error.
Depending on the statement it will try to solve analytically the statement. 


# RoadMap 

Not in order of implementation.
- More equations solvable.
- Derivatives
- Integrals
- Complex
- Factories
- Saves functions in 
- Factorial/Gamma function
- Make variable string and not char to have "a_1". 
- Systems of equations
- Matrix/Vectors
- Infinity
- Polynomials divisions
- Numeric Evaluation
- ODE solving
- Laplace, Fourier, Z transform
- Limits
- Plotting
- i64 to big num.
- Base conversion "0x" "0b"
- Scripting ?