#include <cstdio>
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <ctime>
using namespace std;

typedef struct node {
    int key, h;
    node *l, *r;
    node(int _key = 0) : key(_key), l(nullptr), r(nullptr), h(1) {}
} * pnode;

struct AVL {
    pnode root;
    int _size;
    AVL() : root(nullptr), _size(0) {}
    int height(pnode p) {
        return p == nullptr ? 0 : p->h;
    }
    void upd(pnode p) {
        if(!p) return;
        p->h = max(height(p->l), height(p->r) ) + 1;
    }
    pnode left_rotate(pnode t) {
        pnode p = t->r;
        t->r = p->l;
        p->l = t;
        upd(p->l);
        upd(p);
        return p;
    }
    pnode right_rotate(pnode t) {
        pnode p = t->l;
        t->l = p->r;
        p->r = t;
        upd(p->r);
        upd(p);
        return p;
    }
    void balance(pnode &x) {
        upd(x);
        if (height(x->r) - height(x->l) > 1) {
            if ( !(height(x->r->l) <= height(x->r->r) ) ) {
                x->r = right_rotate(x->r);
            }
            x = left_rotate(x);
        }
        else if (height(x->l) - height(x->r) > 1) {
            if ( !(height(x->l->r) <= height(x->l->l) ) ) {
                x->l = left_rotate(x->l);
            }
            x = right_rotate(x);
        }
        upd(x);
    }
    pnode _insert(pnode t, pnode &x) {
        if (x == nullptr) {
            x = t;
            return x;
        }
        else {
            pnode res;
            if (t->key < x->key) {
                res = _insert(t, x->l);
            }
            else if (t->key == x->key) {
                return x;
            }
            else {
                res = _insert(t, x->r);
            }
            upd(x);
            balance(x);
            return res;
        }
    }

    pnode find(int x) {
        pnode t = root;
        while (t != nullptr && t->key != x) {
            if (x < t->key) {
                t = t->l;
            }
            else {
                t = t->r;
            }
        }
        return t;
    }

    pnode left_max(pnode t) {
        if (t == nullptr) return nullptr;
        pnode v = t->l;
        while (v->r != nullptr) {
            v = v->r;
        }
        return v;
    }   

    pnode right_min(pnode t) {
        if (t == nullptr) return nullptr;
        pnode v = t->r;
        while (v->l != nullptr) {
            v = v->l;
        }
        return v;
    }

    void _erase(pnode t, pnode &x) {
        if (x == t) {
            if (x->l == nullptr && x->r == nullptr) {
                _delete_node(x);
                x = nullptr;
                return;
            }
            else {
                if (height(x->l) > height(x->r) ) {
                    pnode v = left_max(x);
                    x->key = v->key;
                    _erase(v, x->l);
                }
                else {
                    pnode v = right_min(x);
                    x->key = v->key;
                    _erase(v, x->r);
                }
            }
        }
        else if (t->key < x->key) {
            _erase(t, x->l);
        }
        else {
            _erase(t, x->r);
        }
        balance(x);
    }

    void _delete_node(pnode t) {
        delete t;
    }

    pnode insert(pnode t) {
        return _insert(t, root);
    }
    template<class F>
    void traverse(F f, pnode t, int d = 0) {
        if (!t) return;
        traverse(f, t->l, d + 1);
        f(t, d);
        traverse(f, t->r, d + 1);
    }
};

int main() {
    AVL t;
    int n = 50000;
    int start;
    vector<int> a;
    set<int> sp;
    for (int i = 0; i < n; i++) {
        a.push_back(i + 1);
    }
    random_shuffle(a.begin(), a.end() );
    start = clock();
    for (int i = 0; i < n; i++) {
        t.insert(new node(a[i]) );
    }
    cout << ( (double)clock() - start) / CLOCKS_PER_SEC << endl;
    start = clock();
    for (int i = 0; i < n; i++) {
        sp.insert(a[i]);
    }
    cout << ( (double)clock() - start) / CLOCKS_PER_SEC << endl;
    random_shuffle(a.begin(), a.end() );
    for (int i = 0; i < n / 2 + n / 4 + n / 8 + n / 16; i++) {
        t._erase(t.find(a[i]), t.root);
        sp.erase(sp.find(a[i]) );
    }
    vector<int> resAVL, resSet;
    t.traverse([&resAVL](pnode p, int d) { resAVL.push_back(p->key); }, t.root);
    for (set<int>::iterator it = sp.begin(); it !=  sp.end(); it++) {
        resSet.push_back(*it);
    }
    for(int i = 0; i < (int)resAVL.size(); i++) {
        if(resAVL[i] != resSet[i]) cout << "ERROR!!!" << endl;
    }
    cout << t.height(t.root) << endl;
    system("pause");
    return 0;
}