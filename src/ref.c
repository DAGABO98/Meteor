#include "ref.h"
#include <stdlib.h>
#include <stdio.h>

void initRefInt(struct RefInt *ref){
    ref->ptr = NULL;
}

void initRefFloat(struct RefFloat *ref){
    ref->ptr = NULL;
}

void initRefBool(struct RefBool *ref){
    ref->ptr = NULL;
}

void assignRefIntMut(struct RefInt *ref, struct MutInt *mut){
    ref->ptr = mut->ptr;
}

void assignRefIntRef(struct RefInt *ref, struct RefInt *source){
    ref->ptr = source->ptr;
}

void assignRefFloatMut(struct RefFloat *ref, struct MutFloat *mut){
    ref->ptr = mut->ptr;
}

void assignRefFloatRef(struct RefFloat *ref, struct RefFloat *source){
    ref->ptr = source->ptr;
}

void assignRefBoolMut(struct RefBool *ref, struct MutBool *mut){
    ref->ptr = mut->ptr;
}
void assignRefBoolRef(struct RefBool *ref, struct RefBool *source){
    ref->ptr = source->ptr;
}  

float readRefFloat(struct RefFloat *ref){
    return *((float *)ref->ptr);
}

int readRefInt(struct RefInt *ref){
    return *((int *)ref->ptr);
}
bool readRefBool(struct RefBool *ref){
    return *((bool *)ref->ptr);
}
