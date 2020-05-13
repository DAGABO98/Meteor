#include "mut.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void initMut(struct Mut *mut, int type){
    if(type == 0){
        mut->ptr = malloc(sizeof(int));
    }
    else if(type == 1){
        mut->ptr = malloc(sizeof(float));
    }
    else {
        mut->ptr = malloc(sizeof(bool));
    }
    mut->type = type;
}

void assignMutInt(struct Mut *mut, int value){
    memcpy(mut->ptr, &value, sizeof(value));
}                           
void assignMutFloat(struct Mut *mut, float value){
    memcpy(mut->ptr, &value, sizeof(value));
}                               
void assignMutBool(struct Mut *mut, bool value){
    memcpy(mut->ptr, &value, sizeof(value));
} 

int readMutInt(struct Mut *mut){
    return *((int *) mut->ptr);
}

float readMutFloat(struct Mut *mut){
    return *((float *) mut->ptr);
}

bool readMutBool(struct Mut *mut){
    return *((bool *) mut->ptr);
}

void destroyMut(struct Mut *mut){
    free(mut->ptr);
    mut->type = -1;
}
