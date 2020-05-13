#include "mut.h"
#include <stdbool.h>

struct Ref {
        void *ptr;
        int type;
};


void initRef(struct Ref *ref, int type);
void assignRef(struct Ref *ref, struct Mut *mut);
float readRefFloat(struct Ref *ref);
int readRefInt(struct Ref *ref);
bool readRefBool(struct Ref *ref);

