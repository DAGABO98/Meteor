#include "mut.h"
#include <stdbool.h>

struct RefInt {
        int *ptr;
};

struct RefFloat {
        float *ptr;
};

struct RefBool {
        bool *ptr;
};

void initRefInt(struct RefInt *ref);
void initRefFloat(struct RefFloat *ref);
void initRefBool(struct RefBool *ref);
void assignRefIntMut(struct RefInt *ref, struct MutInt *mut);
void assignRefIntRef(struct RefInt *ref, struct RefInt *source);
void assignRefFloatMut(struct RefFloat *ref, struct MutFloat *mut);
void assignRefFloatRef(struct RefFloat *ref, struct RefFloat *source);
void assignRefBoolMut(struct RefBool *ref, struct MutBool *mut);
void assignRefBoolRef(struct RefBool *ref, struct RefBool *source);
int readRefInt(struct RefInt *ref);
float readRefFloat(struct RefFloat *ref);
bool readRefBool(struct RefBool *ref);

