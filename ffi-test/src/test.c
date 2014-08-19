#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Test_stub.h"
#endif

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    
    test();
    test2();
    
    hs_exit();
    return 0;
}

