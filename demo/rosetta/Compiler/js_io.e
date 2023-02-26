--
-- demo\rosetta\Compiler\js_io.e
-- =============================
--
--  Fake file i/o for running under pwa/p2js in a browser
--  Does not cover the human readable reload parts of extra.e
--
with javascript_semantics
sequence {known_files,kfc} = columnize({
{"test3.c",split("""
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal **/  '\\'
/* character literal */  ' '
""","\n")},
{"test4.c",split("""
/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");
""","\n")},
{"primes.c",split("""
/*
 Simple prime number generator
 */
count = 1;
n = 1;
limit = 100;
while (n < limit) {
    k=3;
    p=1;
    n=n+2;
    while ((k*k<=n) && (p)) {
        p=n/k*k!=n;
        k=k+2;
    }
    if (p) {
        print(n, " is prime\n");
        count = count + 1;
    }
}
print("Total primes found: ", count, "\n");
""","\n")},
{"gcd.c",split("""
/* Compute the gcd of 1071, 1029:  21 */
 
a = 1071;
b = 1029;
 
while (b != 0) {
    new_a = b;
    b     = a % b;
    a     = new_a;
}
print(a);
""","\n")},
{"Header.h",split("""
#define area(h, w) h * w
""","\n")},
{"Source.t",split("""
#include "Header.h"
#define width 5
#define height 6
area = #area(height, width)#;
""","\n")}})

sequence linenos = repeat(-1,length(known_files))

global function js_open(string filename)
    integer fn = find(filename,known_files)
    assert(fn!=0)
    linenos[fn] = 0
    return fn
end function

global function js_gets(integer fn)
    integer lineno = linenos[fn]+1
    if lineno<=length(kfc[fn]) then
        linenos[fn] = lineno
        return kfc[fn][lineno]
    end if
    return EOF
end function

