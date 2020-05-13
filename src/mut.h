#include <stdbool.h>
struct MutInt {
        int *ptr;
};

struct MutFloat {
        float *ptr;
};

struct MutBool {
        bool *ptr;
};

void initMutInt(struct MutInt *mut);
void initMutFloat(struct MutInt *mut);
void initMutBool(struct MutBool *mut);
void assignMutInt(struct MutInt *mut, int value);
void assignMutFloat(struct MutFloat *mut, float value);
void assignMutBool(struct MutBool *mut, bool value);
int readMutInt(struct MutInt *mut);
float readMutFloat(struct MutFloat *mut);
bool readMutBool(struct MutBool *mut);
void destroyMutInt(struct MutInt *mut);
void destroyMutFloat(struct MutFloat *mut);
void destroyMutBool(struct MutBool *mut);

