#include <stdbool.h>
struct Mut {
        void *ptr;
        int type;
};

void initMut(struct Mut *mut, int type);
void assignMutInt(struct Mut *mut, int value);
void assignMutFloat(struct Mut *mut, float value);
void assignMutBool(struct Mut *mut, bool value);
int readMutInt(struct Mut *mut);
float readMutFloat(struct Mut *mut);
bool readMutBool(struct Mut *mut);
void destroyMut(struct Mut *mut);
