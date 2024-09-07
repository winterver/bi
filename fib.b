printn(n,b) {
    extrn putchar;
    auto a;

    if(a=n/b)           /* assignment, not test for equality */
        printn(a, b);   /* recursive */
    putchar(n%b + '0');
}

fib(n) {
    if (n<3) return 1;
    return fib(n-1)+fib(n-2);
}

main() {
    printn(fib(34), 10);
    putchar('*n');
}
