#include <stdio.h>

int fib(n)
{
int i, a, b, d;
    a=1; b=1;
    for (i = 2; i <= n; i++) {
	d = a+b;
	a = b;
	b = d;
    }
    return a;
}

int main(int argc, char **argv) 
{
    int i,n,e,r;

//  n=26;   e=121393;
//  n=27;   e=196418;
//  n=29;   e=514229;
    n=44;   e=701408733;
    for (i = 1; i <= 200000; i++) {
	r=fib(n);
	if (r!=e) {
	    printf("fib(%d) = %d, not %d\n", n, r, e);
	    return 1;
	}
    }
    return 0;
}
