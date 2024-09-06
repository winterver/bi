printn(n,b) {
    extrn putchar;
    auto a;

    if(a=n/b)           /* assignment, not test for equality */
        printn(a, b);   /* recursive */
    putchar(n%b + '0');
}

main() {
    printn(233);
    putchar('*n');
}
