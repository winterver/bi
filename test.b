printn(n,b) {
    extrn putchar;
    auto a;

    if(a=n/b)           /* assignment, not test for equality */
        printn(a, b);   /* recursive */
    putchar(n%b + '0');
}

puts(s) {
    extrn putchar;
    auto c, n 0;

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

main() {
    auto b, a "233" "345" "456";
    b = &a;

    printn(char(*b,1), 10);
    putchar('*n');
    puts(*b);
}
