#include "mut.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void initMutInt(struct MutInt *mut){
    mut->ptr = malloc(sizeof(int));
}

void initMutFloat(struct MutInt *mut){
    mut->ptr = malloc(sizeof(float));
}

void initMutBool(struct MutBool *mut){
    mut->ptr = malloc(sizeof(bool));
}

void assignMutInt(struct MutInt *mut, int value){
    memcpy(mut->ptr, &value, sizeof(value));
}                           
void assignMutFloat(struct MutFloat *mut, float value){
    memcpy(mut->ptr, &value, sizeof(value));
}                               
void assignMutBool(struct MutBool *mut, bool value){
    memcpy(mut->ptr, &value, sizeof(value));
} 

int readMutInt(struct MutInt *mut){
    return *((int *) mut->ptr);
}

float readMutFloat(struct MutFloat *mut){
    return *((float *) mut->ptr);
}

bool readMutBool(struct MutBool *mut){
    return *((bool *) mut->ptr);
}

void destroyMutInt(struct MutInt *mut){
    free(mut->ptr);
}

void destroyMutFloat(struct MutFloat *mut){
    free(mut->ptr);
}

void destroyMutBool(struct MutBool *mut){
    free(mut->ptr);
}
