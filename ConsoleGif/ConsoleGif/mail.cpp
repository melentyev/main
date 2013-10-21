#include <cstdio>
#include <cassert>
#include <cstring>
#include <cstdlib>
#include <memory.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
using namespace std;

enum GifDataBlockType {
    BT_COMMENT = 0,
    BT_APPLICATION = 1,
    BT_IMAGEDATA = 2,
    BT_IMAGEADDITIONAL = 3,
    BT_END = 5
};

struct GifLogicalScreen {
    unsigned short width, height, globalTableSize, colorDef;
    bool bGlobalTable, bSort;
    int backgroundIndex, sidesCoef;
};

struct GifDataSubblock {
    int size;
    unsigned char* data;
};

class GifDataBlock {
public: 
    GifDataBlock() : subblockCnt(0), subblocks(NULL) {}
    GifDataBlockType type;
    int subblockCnt;
    GifDataSubblock *subblocks;
};

class GifDataBlockApplication : public GifDataBlock {
public:
    char id[8];
    unsigned char idCode[3];
    GifDataBlockApplication() { type = BT_APPLICATION; }
};

class GifDataBlockImage : public GifDataBlock {
public:
    GifDataBlockImage() :left(0), top(0), width(0), height(0), lzwMinCode(0), localTableSize(0) { type = BT_IMAGEDATA; }
    unsigned short left, top, width, height; 
    int localTableSize, lzwMinCode;
    bool localTableFlag, interlaceFlag;
    
};

class GifDataDlockImageAdditional : public GifDataBlock {
public:
    GifDataDlockImageAdditional() : transparentColorIndex(0), disposalMethod(0), delayTime(0) { type = BT_IMAGEADDITIONAL; }
    int disposalMethod, transparentColorIndex;
    unsigned short delayTime;
    bool userInput, transparentColorFlag;
};

struct GifColorTable {
    int *colors;
    int size;
    int transparentIndex;
};


class GifStream {
    GifLogicalScreen logicalScreen;
    GifColorTable globalTable;
    GifDataDlockImageAdditional *nextImageModifier;
    vector<int> _dump;
    bool _makeDump;
    map<int, vector<unsigned char> > codes;

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
    void readColorTable(GifColorTable *table, int tableSize) {
        table->size = 1 << (tableSize + 1);
        table->colors = new int[table->size];
        memset(table->colors, 0, sizeof(int) * table->size);
        for(int i = 0; i < table->size; i++) {
            readBytes((unsigned char*) (table->colors + i), 3); 
            int g = 0;
        }
    }

    void readSubblocks(GifDataBlock* block) {
        int size;
        do {
            size = readByte();
            if (size > 0) {
                block->subblockCnt++;
                block->subblocks = (GifDataSubblock*)realloc(block->subblocks, block->subblockCnt * sizeof(GifDataSubblock) );
                block->subblocks[block->subblockCnt - 1].size = size;
                block->subblocks[block->subblockCnt - 1].data = new unsigned char[size];
                readBytes(block->subblocks[block->subblockCnt - 1].data, size);
            }
        } while(size > 0);
    }

    void readDataBlock(GifDataBlock* &block) {
        int id = readByte();
        if (id == 0x3B) {
            block = new GifDataBlock();
            block->type = BT_END;
        }
        else if (id == 0x21) {
            int header = readByte();
            if (header == 0xFF) {
                auto app_block = new GifDataBlockApplication();
                app_block->type = BT_APPLICATION;
                int checkSize = readByte();
                if(checkSize != 11) raiseError("check size failed\n");
                readBytes( (unsigned char*)app_block->id, 8);
                readBytes( (unsigned char*)app_block->idCode, 3);
                readSubblocks(app_block);
                block = app_block;
            }
            else if(header == 0xF9) {
                auto add_img_block = new GifDataDlockImageAdditional();
                add_img_block->type = BT_IMAGEADDITIONAL;
                int checkSize = readByte();
                if(checkSize != 4) raiseError("check size failed\n");
                int bitFields = readByte();
                add_img_block->transparentColorFlag = (bitFields & 1 ? true : false);
                add_img_block->userInput = ( bitFields & 2 ? true : false);
                add_img_block->disposalMethod = (bitFields >> 2) & 7;
                readBytes((unsigned char*) &add_img_block->delayTime, 2);
                add_img_block->transparentColorIndex = readByte();
                readSubblocks(add_img_block);
                block = add_img_block;
            }
            else if(header == 0xFE) {
                
            }
        }
        else if (id == 0x2C) {
            auto img_block = new GifDataBlockImage();
            readBytes((unsigned char*) &img_block->left, 2);
            readBytes((unsigned char*) &img_block->top, 2);
            readBytes((unsigned char*) &img_block->width, 2);
            readBytes((unsigned char*) &img_block->height, 2);
            int bitFields = readByte();
            img_block->localTableSize = bitFields & 7;
            img_block->localTableFlag = (bitFields & (1 << 7) ? true : false);
            img_block->interlaceFlag = (bitFields & (1 << 16) ? true : false);
            img_block->lzwMinCode = readByte();
            readSubblocks(img_block);
            block = img_block;
        }    
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

    void resetCodesTable(int lzwMinCode) {
        codes.clear();
        for (int i = 0; i < (1 << lzwMinCode); i++) {
            codes[i] = vector<unsigned char>(1, i);
        }
        codes[(1 << lzwMinCode)] = vector<unsigned char>(1, (1 << lzwMinCode) );
        codes[(1 << lzwMinCode) + 1] = vector<unsigned char>(1, (1 << lzwMinCode) + 1);
    }

    void unpackImageData(GifDataBlockImage *block) {
        string bits;
        vector<unsigned char> result;
        int codeClear, codeEnd, codeLen;
        vector<unsigned char> prevCodes;
        resetCodesTable(block->lzwMinCode);
        codeClear = (1 << block->lzwMinCode);
        codeEnd = (1 << block->lzwMinCode) + 1;
        codeLen = block->lzwMinCode + 1;
        prevCodes = vector<unsigned char>(0);
        for (int num = 0; num < block->subblockCnt; num++) {
            unsigned char *end = block->subblocks[num].data + block->subblocks[num].size;
            for(unsigned char* ptr = block->subblocks[num].data; ptr != end; ptr++) {
                int curByte = *ptr;
                for(int counter = 0; counter < 8; counter++) {
                    bits += (char)'0' + (curByte & 1);
                    curByte >>= 1;
                }
            }
        }
        for(string::iterator it = bits.begin(); it != bits.end();) {
            int code = 0, pos = 0;
            for(int counter = 0; counter < codeLen; counter++, it++) {
                code |= (*it - '0') << pos;
                pos++;
            }
            if(code == codeClear) {
                resetCodesTable(block->lzwMinCode);
                codeClear = (1 << block->lzwMinCode);
                codeEnd = (1 << block->lzwMinCode) + 1;
                codeLen = block->lzwMinCode + 1;
                prevCodes = vector<unsigned char>(0);
            }
            else if(code == codeEnd) {
                break;
            }
            else {
                int nextCode = codes.size();
                if(codes.count(code) > 0) {
                    for(vector<unsigned char>::iterator codeIt = codes[code].begin(); codeIt != codes[code].end(); codeIt++) {
                        result.push_back(*codeIt);
                    }
                    if(prevCodes.size() > 0) {
                        prevCodes.push_back( *(codes[code].begin() ) );
                        codes[nextCode] = prevCodes;
                    }
                }
                else {
                    prevCodes.push_back(prevCodes[0]);
                    codes[nextCode] = prevCodes;
                    for(vector<unsigned char>::iterator codeIt = prevCodes.begin(); codeIt != prevCodes.end(); codeIt++) {
                        result.push_back(*codeIt);
                    }
                }
                prevCodes = codes[code];
                
            }
            if(codes.size() == (1 << codeLen) ) {
                codeLen++;
            }
        }
    }


public:
    char version[4];

    GifStream() {
        _makeDump = false;
        _dump.clear();
        nextImageModifier = NULL;
    }
    void processStream() {
        unsigned char header[7];
        readBytes(header, 6);
        if (header[0] != 'G' || header[1] != 'I' || header[2] != 'F') {
            raiseError("Header not match");
        }
        memcpy(version, header + 3, 3);
        version[3] = 0;
        readBytes( (unsigned char *)&logicalScreen.width, 2);
        readBytes( (unsigned char *)&logicalScreen.height, 2);
        int bitFields = readByte();
        logicalScreen.globalTableSize = bitFields & 0x07;
        logicalScreen.bSort = (bool)(bitFields & 0x08 ? true : false);
        logicalScreen.colorDef = (bitFields >> 4) & 0x07;
        logicalScreen.bGlobalTable = (bool)(bitFields & 0xA0 ? true : false);
        logicalScreen.backgroundIndex = readByte();
        logicalScreen.sidesCoef = readByte();
        if (logicalScreen.bGlobalTable) {
            beginReadingDump();
            readColorTable(&globalTable, logicalScreen.globalTableSize);
            finishReadingDump(13);
        }
        GifDataBlock *block = NULL;
        int type;
        do {
            readDataBlock(block);
            type = block->type;
            if(type == BT_IMAGEADDITIONAL) {
                nextImageModifier = (GifDataDlockImageAdditional*)block;
            }
            else if(type == BT_IMAGEDATA) {
                unpackImageData( (GifDataBlockImage*) block);
                if (nextImageModifier != NULL) {
                    delete nextImageModifier;
                    nextImageModifier = NULL;
                }
            }
            else {
                 delete block;
            }
        } while(type != BT_END);
        delete block;
    }
    void raiseError(char *s) {
        puts(s);
        system("pause");
        exit(0);
    }
};

class GifStreamCIO : public GifStream {
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

class GifStreamString : public GifStream {
    unsigned char *_stream;
public:    
    GifStreamString(unsigned char *stream = NULL) : GifStream()  {
        _stream = stream;
    }
    unsigned char _readByte() {
        return *(_stream++);
    }
};

class GifStreamIOStream : public GifStream {
    ifstream *_stream;
public:    
    GifStreamIOStream(ifstream *stream = NULL) : GifStream() {
        _stream = stream;
        
    }
    unsigned char _readByte() {
        if(_stream->eof() ) {
            printf("EOF\n"); 
            system("pause");
            exit(0);
        }
        
        return _stream->get();
    }
};

int main() {
    //ifstream *fs = new ifstream("C:\\MinGW\\file2.gif");
    FILE *fin = fopen("C:\\MinGW\\file.gif", "rb");
    unsigned char *cont = new unsigned char[6 * 1000 * 1000];
    int filesize = fread(cont, 1, 6 * 1000 * 1000, fin);
    /*fs->seekg (0, ios::end);
    int length = fs->tellg();
    fs->seekg (0, ios::beg);
    fs->read(cont, length);
    */
    
    GifStreamString gif = GifStreamString(cont);
    //GifStreamIOStream gif = GifStreamIOStream(fs);
    gif.processStream();
    return 0;
}