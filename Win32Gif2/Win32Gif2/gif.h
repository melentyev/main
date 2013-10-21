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

struct GifColorTable {
    int *colors;
    int size;
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
    GifColorTable localTable;
    
};

class GifDataDlockImageAdditional : public GifDataBlock {
public:
    GifDataDlockImageAdditional() : transparentColorIndex(0), disposalMethod(0), delayTime(0) { type = BT_IMAGEADDITIONAL; }
    int disposalMethod, transparentColorIndex;
    unsigned short delayTime;
    bool userInput, transparentColorFlag;
};

struct _byteSeq {
    _byteSeq(signed short _Prev = 0, signed short _byte = 0) : Prev(_Prev), byte(_byte) {}
    signed short Prev, byte;
};

class GifStream {
    GifLogicalScreen logicalScreen;
    vector<int> _dump;
    bool _makeDump;
    _byteSeq codes[10000];
    int codesCount;

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
            if(img_block->localTableFlag){
                readColorTable(&img_block->localTable, img_block->localTableSize);
            }
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
        for (int i = 0; i < (1 << lzwMinCode); i++) {
            codes[i] = _byteSeq(-1, i);
        }
        codes[(1 << lzwMinCode)] = _byteSeq(-1, (1 << lzwMinCode) );
        codes[(1 << lzwMinCode) + 1] = _byteSeq(-1, (1 << lzwMinCode) + 1);
        codesCount = (1 << lzwMinCode) + 2;
    }

    int unpackBitCounter;
    int unpackSubblockNum;
    int unpackCurByte;
    GifDataBlockImage *unpackBlock;
    unsigned char *unpackSubblockEnd;
    unsigned char *unpackSubblockPtr;

    inline int unpackNextBit() {
        if (unpackBitCounter == 0) {
            if (unpackSubblockPtr == unpackSubblockEnd) {
                unpackSubblockNum++;
                if (unpackSubblockNum >= unpackBlock->subblockCnt) {
                    return -1;
                }
                unpackSubblockPtr = unpackBlock->subblocks[unpackSubblockNum].data;
                unpackSubblockEnd = unpackSubblockPtr + unpackBlock->subblocks[unpackSubblockNum].size;
            }
            unpackCurByte = (int)(*unpackSubblockPtr) << 1;
            unpackSubblockPtr++;
        }        
        unpackBitCounter = (unpackBitCounter + 1) & 7;
        return (unpackCurByte >>= 1) & 1;
        'ab';
    }

    void unpackImageData(GifDataBlockImage *block, unsigned char *resultBuffer) {
        vector<unsigned char> result;
        int codeClear, codeEnd, codeLen, lenForReversing;
        int prevString;
        
        unsigned char arrForReversing[4096];
        resetCodesTable(block->lzwMinCode);
        codeClear = (1 << block->lzwMinCode);
        codeEnd = (1 << block->lzwMinCode) + 1;
        codeLen = block->lzwMinCode + 1;
        prevString = -1;

        unpackSubblockNum = 0;
        unpackBitCounter = 0;
        unpackBlock = block;
        unpackSubblockPtr = block->subblocks[0].data;
        unpackSubblockEnd = unpackSubblockPtr + block->subblocks[0].size;

        for(;;) {
            int code = 0, pos = 0;
            for(int counter = 0; counter < codeLen; counter++) {
                code |= (unpackNextBit() << pos);
                pos++;
            }
            if(code == codeClear) {
                resetCodesTable(block->lzwMinCode);
                codeClear = (1 << block->lzwMinCode);
                codeEnd = (1 << block->lzwMinCode) + 1;
                codeLen = block->lzwMinCode + 1;
                prevString = -1;
            }
            else if(code == codeEnd) {
                break;
            }
            else {
                if(code < codesCount) {
                    lenForReversing = 0;
                    int curCode = code;
                    do {
                        arrForReversing[lenForReversing++] = codes[curCode].byte;
                        curCode = codes[curCode].Prev;
                    } while (curCode != -1);
                    for(int i = lenForReversing - 1; i >=0; i--) {
                        *(resultBuffer++) = arrForReversing[i];
                    }
                    if(prevString >= 0) {
                        codes[codesCount] = _byteSeq(prevString, arrForReversing[lenForReversing - 1]);
                        prevString = codesCount++;
                    }
                }
                else {
                    lenForReversing = 0;
                    int curCode = prevString;
                    do {
                        arrForReversing[lenForReversing++] = codes[curCode].byte;
                        curCode = codes[curCode].Prev;
                    } while (curCode != -1);
                    codes[codesCount] = _byteSeq(prevString, arrForReversing[lenForReversing - 1]);
                    prevString = codesCount++;
                    for(int i = lenForReversing - 1; i >=0; i--) {
                        *(resultBuffer++) = arrForReversing[i];
                    }
                    *(resultBuffer++) = arrForReversing[lenForReversing - 1];
                }
                prevString = code;
            }
            if (codesCount == (1 << codeLen) ) {
                if(codeLen < 12) codeLen++;
            }
        }
    }


public:
    char version[4];
    GifColorTable globalTable;
    GifDataDlockImageAdditional *nextImageModifier;
    void(*onImageDecodedCallback)(GifDataBlockImage*, unsigned char*);
    GifStream() {
        _makeDump = false;
        _dump.clear();
        nextImageModifier = NULL;
    }
    void processStream(unsigned char*(*allocateMemory)(int w, int h) ) {
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
            //beginReadingDump();
            readColorTable(&globalTable, logicalScreen.globalTableSize);
            //finishReadingDump(13);
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
                unsigned char *res = allocateMemory(( (GifDataBlockImage*) block)->width, ( (GifDataBlockImage*) block)->height);
                unpackImageData( (GifDataBlockImage*) block, res);
                onImageDecodedCallback( (GifDataBlockImage*) block, res);
                if (nextImageModifier != NULL) {
                    delete nextImageModifier;
                    nextImageModifier = NULL;
                }
            }
            else {
                 delete block;
            }
        } while(type != BT_END);
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
