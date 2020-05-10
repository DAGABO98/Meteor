struct Mut {
        void *ptr;
        int type;
};

void initMut(struct Mut *mut);
float readMutFloat(struct Mut *mut);
int readMutInt(struct Mut *mut);
char *readRefStr(struct Mut *mut);
void modMut(struct Mut *Mut);
void destrMut(struct Mut *mut);
