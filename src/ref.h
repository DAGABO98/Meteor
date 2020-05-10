struct Ref {
        void *ptr;
        int type;
};

void initRef(struct Ref *ref, void *ptr, int type);
float readRefFloat(struct Ref *ref);
int readRefInt(struct Ref *ref);
char *readRefStr(struct Ref *ref);
