printn(n,b) {
    extrn putchar;
    auto a;

    if(a=n/b)           /* assignment, not test for equality */
        printn(a, b);   /* recursive */
    putchar(n%b + '0');
}

puts(s) {
    extrn putchar;
    auto c, n;

    n = 0;
    while(c=char(s,n++))
        putchar(c);
    putchar('*n');
}

/*
a [] 233;
b [] a;

main() {
    printn(b, 10); putchar('*n');
    printn(*b, 10); putchar('*n');
}
*/

/*
a "233";
b a;

main() {
    printn(char(*b,1), 10);
    putchar('*n');
    puts(*b);
}
*/

/*
a [] "233";
b [] a;

main() {
    printn(char(*b,1), 10);
    putchar('*n');
    puts(*b);
}
*/

/*
main() {
    auto b, a;
    a = "233" "345" "456";
    b = &a;

    printn(char(*b,1), 10);
    putchar('*n');
    puts(*b);
}
*/

main() {
    auto b, a 2;
    a[0] = 2;
    a[1] = 3;
    a[2] = 3;
    printn(a[0], 10);
    printn(a[1], 10);
    printn(b, 10);
    putchar('*n');
}
