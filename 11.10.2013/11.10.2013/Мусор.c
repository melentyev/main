
/*
char* append_char(char *s, char c, int len) {
    if (len % DEFAULT_CHUNK_SIZE == 0) {
        s = (char*)realloc(s, (len + DEFAULT_CHUNK_SIZE) * sizeof(int));
    }
    s[len] = c;
    return s;
}

char *my_gets(FILE *stream) {
    char *s = (char*)malloc(sizeof(char) * DEFAULT_CHUNK_SIZE);
    int c, len = 0, done = FALSE;
    do {
        c = getc(stream);
        if(c == '\n' || c == 0) {
            c = 0;
            done = TRUE;
        }
        s = append_char(s, c, len++);
    } while(!done);
    return s;
}

int *get_vals(char *s, int *return_len) {
    int handled, 
        val, 
        *vals = (int*)malloc(sizeof(int) * DEFAULT_CHUNK_SIZE), 
        cnt = 0;
    while( (handled = sscanf(s, "%d", &val) ) > 0) {
        vals = append_int(vals, val, cnt++);
        s += handled;
    }
    *return_len = cnt;
    return vals;
}
*/
