struct Ref {
        void *ptr;
        int type;
};

void initRef(struct Ref *ref, void *ptr, int type){
    ref->ptr = ptr;
    ref->type = type;
}

float readRefFloat(struct Ref *ref){
    return *((float *)ref->ptr);
}

int readRefInt(struct Ref *ref){
    return *((int *)ref->ptr);
}
char *readRefStr(struct Ref *ref){
    return (char *)ref->ptr;
}
