#include <iostream>
#include <algorithm>
#include <set>
#include <map>
using namespace std;

typedef struct __HNode {
    unsigned char alpha;
    int n;
    __HNode *parent;
    __HNode(unsigned char _alpha, int _n) : alpha(_alpha), n(_n) {}
    static __HNode* create(unsigned char _alpha, int _n) {
        return new __HNode(_alpha, _n);
    }
} *HNode;

class HNodeComparator {
    bool operator()(HNode &a, HNode &b) {
    
    }
};

set <HNode, HNodeComparator> processingList;
map <unsigned char, HNode> charToHNode;

void prepareData(FILE* stream) {
    int byte;
    byte = getc(stream);
    while(byte != EOF) {
        if(charToHNode.count(byte) == 0) {
            charToHNode[byte] = __HNode::create(byte, 0);
        }
        charToHNode[byte]->n++;
        byte = getc(stream);
    }

}

int main() {
    return 0;
} 