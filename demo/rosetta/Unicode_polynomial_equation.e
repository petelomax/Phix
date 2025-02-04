constant raw_text = """
-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
===> x^5 - 2x^4 + 42x^3 + 40x + 1
x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵
==> 0
1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x^5 - 2x**4 + 42x^3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰
==> 1 + 40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵
===> 1 + 40x^-1 + 42x^-3 - 2x^-4 + x^-5
x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2
==> x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
===> x^5 - 0.5x^4 + 101.25x^3 + 100.75x + 2.5
x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
==> x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
x<sup>5</sup> - 2x<sup>4</sup> + 42x<sup>3</sup> + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x^5 - 2x^4 + 42x^3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - 2x⁴ + 42x³ + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x5 - 2x4 + 42x3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
------------------------------------------
Task details:

Given a string containing an untidy Unicode polynomial, e.g.

-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹
Coerce (or convert) the string into the "polynomial" object, at the same time normalise the polynomial to a canonical form. The ideal normalised output (in this example) would be:

x⁵ - 2x⁴ + 42x³ + 40x + 1
Specific examples of Unicode and polynomial texts to be parsed as test cases.
Description	Input example test cases
"Zero" coefficients are removed	x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1
The "0" polynomial case	0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵
"One" coefficients are normalised	1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰
Signs are normalised	+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1
ASCII representations are parsed	x^5 - 2x**4 + 42x^3 + 40x + 1
Non-ASCII representations are parsed	x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1 (c.f. ↑ & ·)
Specifically permit non-polynomials where terms have negative exponents	x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰ (n.b. Unicode Fraction)
Spaces in numbers and between operators are ignored	x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1
Single commas are ignored in numbers	x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1
A coefficient may be duplicated, zero, or missing	0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1
Support Scientific notation and optionally
support Unicode Decimal Exponent Symbol U+23E8/⏨	1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1
Unicode characters that must be specifically supported are:	⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁻ ⁺ · × ⁄ ↑ ⏨.
Where · & × are multiplication, and ⁄ is Unicode Fraction.
Support fractions for both input and output.	x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2
On output round the decimal to appropriate fraction.
Optionally support Unicode Vulgar fractions for both input and output.
¼ ½ ¾ ⅐ ⅑ ⅒ ⅓ ⅔ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ↉	x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
On output round the decimal to appropriate fraction.
There are (at least) three possible ways of achieving this task.

Using an external parsing library.
Using a built-in parsing/formatting library.

http://rosettacode.org/wiki/Unicode_polynomial_equation

output when uni_frac = true (linux only):
 1:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
 4:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
 6:                                        0   ok
 8:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
10:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
12:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
14:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
16:           1 + 40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵   ok
19:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
21:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
23:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
25:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
27:           x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½   ok
30:           x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½   ok
32:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
34:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
36:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
38:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok


"""
global function get_lines()
    return split(raw_text,"\n")
end function
