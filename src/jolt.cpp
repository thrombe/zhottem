#include <stdio.h>

#include <Jolt/Jolt.h>

extern "C" {
    #include "jolt.h"

    void testfn() {
        printf("hello from cpp\n");
    }
}
