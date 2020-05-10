#include <stdio.h>
#include <stdlib.h>

struct Mut {
        void *ptr;
        int type;
};

void initMut(struct Mut *mut, int type){
    if(type == 1){
        mut->ptr = malloc(sizeof(int));
    }
    else if(type == 2){
        mut->ptr = malloc(sizeof(float));
    }
    else {
        mut->ptr = malloc(sizeof(char *));
    }
    mut->type = type;
}

float readMutFloat(struct Mut *mut){
    return *((float *) mut->ptr);
}

int readMutInt(struct Mut *mut){
    return *((int *) mut->ptr);
}

char *readMutStr(struct Mut *mut){
    return (char *)mut->ptr;
}

void writeMutFloat(struct Mut *mut, float value){
    memcopy(mut->ptr, &value, sizeof(value));
}

void writeMutInt(struct Mut *mut, int value){
    memcopy(mut->ptr, &value, sizeof(value));
}

void *writeMutStr(struct Mut *mut, char *value){
    memcopy(mut->ptr, value, sizeof(value));
}

void destrMut(struct Mut *mut){
    free(mut->ptr);
    mut->type = 0;
}
