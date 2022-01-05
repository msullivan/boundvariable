#include <stdio.h>
int main() {
    FILE *fp = fopen("input", "r");
    if (fp == NULL) return 0;
    else return fgetc(fp);
}
