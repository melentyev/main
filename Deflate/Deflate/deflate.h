#include <iostream>
#include <cstdio>
#include <string>
#include <vector>

class GifStream {
    std::vector<int> _dump;
    bool _makeDump;
    virtual unsigned char _readByte() = 0;
    unsigned char readByte() {
        if(_makeDump) {
            _dump.push_back(_readByte() );
            return _dump.back();
        }
        else 
            return _readByte();
    }
    void readBytes(unsigned char *buf, int cnt) {
        while(cnt-- > 0) 
            *(buf++) = readByte();    
    }
    void beginReadingDump() {
        _dump.clear();
        _makeDump = true;
    }
    void finishReadingDump(int offset) {
        for (int i = 0; i < offset; i++) printf("   ");
        for (int i = offset; i < _dump.size() + offset; i++) {
            if(i > 0 && i % 16 == 0) {
                printf("\n");
            }
            printf("%02x ", _dump[i - offset]);
        }
    }
public:
    GifStream() {
        _makeDump = false;
        _dump.clear();
    }
    void processStream() {
    
    }
    void raiseError(std::string s) {
        throw s;
    }
};

class DeflateStreamCIO : public GifStream {
    FILE *_stream;
public:    
    GifStreamCIO(FILE *stream = NULL) : GifStream() {
        _stream = stream;
    }
    unsigned char _readByte() {
        int v = getc(_stream);
        if(v == EOF) { 
            raiseError("EOF");
        }
        return v;
    }
};