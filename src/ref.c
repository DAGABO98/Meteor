#include "ref.h"
#include <stdlib.h>
#include <stdio.h>

void initRef(struct Ref *ref, int type){
    ref->ptr = NULL;
    ref->type = type;
}

void assignRef(struct Ref *ref, struct Mut *mut){
    ref->ptr = mut->ptr;
}
float readRefFloat(struct Ref *ref){
    return *((float *)ref->ptr);
}

int readRefInt(struct Ref *ref){
    return *((int *)ref->ptr);
}
bool readRefBool(struct Ref *ref){
    return *((bool *)ref->ptr);
}
