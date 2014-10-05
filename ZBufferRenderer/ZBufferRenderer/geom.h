#pragma once
#ifndef GEOM_H
#define GEOM_H
#include <cmath>
#include <iostream>
#include <utility>
using namespace std;


const double INF = 1e12;
const double globalEPS = 1e-9;
const double globalPI = acosl(0.5L) * 3;

template <class T>
T sqr(T x) {
    return x * x;
}

template <class T>
T det(T a1, T b1, T a2, T b2) {
    return a1 * b2 - a2 * b1;
}

template <class T>
T Abs(T x) {
    return (x < (T)0) ? -x : x;
}



struct point {
	bool EQ(const double x, const double y, double EPS = globalEPS) const {
		return Abs(x - y) < EPS; 
	}
    double x, y;
    point() { x = y = 0.0; }
    point(double _x, double _y) : x(_x), y(_y) {}
    point(point p1, point p2) {
        x = p2.x - p1.x;
        y = p2.y - p1.y;
    }
    double len2() {
        return x * x + y * y;
    }
    double len() {
        return sqrt(len2());
    }
    point operator + (const point &p) const {
        return point(x + p.x, y + p.y);
    }
    point operator - (const point &p) const {
        return point(x - p.x, y - p.y);
    }
    point operator * (const double &k) const {
        return point(x * k, y * k);
    }
    double operator * (const point &p) const {
        return x * p.x + y * p.y;
    }
    double operator % (const point &p) const {
        return x * p.y - p.x * y;
    }
	point operator - () const {
		return point(-x, -y);
	}
    point operator += (const point &p) {
        x += p.x;
        y += p.y;
        return *this;
    }
    point operator -= (const point &p) {
        x -= p.x;
        y -= p.y;
        return *this;
    }
    point normalize() {
        double L = len();
        return point(x / L, y / L);
    }
    point rotate(const double &a) const {
        return point(x * cos(a) - y * sin(a), x * sin(a) + y *cos(a) );
    }
    point rotate_cos(const double &a, int sign = 1) const {
        double Sin = sqrt(1 - sqr(a)) * sign;
        return point(x * a - y * Sin, x * Sin + y *a );
    }
    point rotate_sin(const double &a, int sign = 1) const { 
        double Cos = sqrt(1 - sqr(a)) * sign;
        return point(x * Cos - y * a, x * a + y * Cos );
    }
    void out(int pr) {
        cout.precision(pr);
        cout << fixed << x << ' ' << y;
    }
    /*static bool cmpx(const point &p1, const point &p2) {
        return (p1.x + globalEPS < p2.x) || (EQ(p1.x, p2.x) && p1.y < p2.y);
    }*/
    point rot90() const {
        return point(-y, x);
    }
    bool eq(const point &p) const {
        return EQ(x, p.x) && EQ(y, p.y);
    }
};

/*double dist(point p1, point p2) {
    return point(p1, p2).len();
}*/

struct line;

struct segment {
	bool EQ(const double x, const double y, double EPS = globalEPS) const {
		return Abs(x - y) < EPS; 
	}
    point a, b;
    segment() {}
    segment(double _x, double _y, double __x, double __y) {
        a = point(_x, _y);
        b = point(__x, __y);
    }
    segment(const point &p1, const point &p2){ a = p1; b = p2; }
    point mid() { 
        return point((a.x + b.x) / 2.0, (a.y + b.y) / 2.0);
    } 
    bool intersect(segment s) {
        double v1, v2, v3, v4;
        point p1 = a, p2 = b, p3 = s.a, p4 = s.b;
        v1 = point(p1, p2) % point(p1, p3);
        v2 = point(p1, p2) % point(p1, p4);
        v3 = point(p3, p4) % point(p3, p1);
        v4 = point(p3, p4) % point(p3, p2);
        if(v1 * v2 > -globalEPS || v3 * v4 > -globalEPS) return false;
        return (bounding_cross(s));
    }
    bool bounding_cross(const segment &s) {
        double stx1 = min(a.x, b.x), enx1 = max(a.x, b.x), stx2 = min(s.a.x, s.b.x), enx2 = max(s.a.x, s.b.x);
        double sty1 = min(a.y, b.y), eny1 = max(a.y, b.y), sty2 = min(s.a.y, s.b.y), eny2 = max(s.a.y, s.b.y);
        return( max(stx1, stx2) <= min(enx1, enx2) + globalEPS
             && max(sty1, sty2) <= min(eny1, eny2) + globalEPS);
    }
	double crossYl(double Y) {
		if(fabs(a.y - b.y) < 1e-8) return min(a.x, b.x);
		double t = (Y - b.y) / (a.y - b.y);
		return b.x + (a.x - b.x) * t;
	}
};

struct line {
	bool EQ(const double x, const double y, double EPS = globalEPS) const {
		return Abs(x - y) < EPS; 
	}
    double a, b, c;
    static const int IntInf = 100500;
    line() { a = b = c = 0.0;}
    line(double _a, double _b, double _c) { a = _a; b = _b; c = _c; }
    line(const point &p1, const point &p2) {
        a = p2.y - p1.y;
        b = p1.x - p2.x;
        c = -det(p1.x, p1.y, p2.x, p2.y);
    }
    line(const segment &s) {
        line l(s.a, s.b);
        a = l.a;
        b = l.b;
        c = l.c;
    }
    point any() const {
        return (EQ(b, 0.0L) ? point(-c / a, 0.0) : point(0.0, -c / b));
    }
    line normalize() {
        double d = point(a, b).len();
        return line(a / d, b / d, c / d);
    }
    point normal() {
        return point(a, b);
    }
    double solve(point p) {
        return a * p.x + b * p.y + c;
    }
    line reverse() const {
        return line (-a, -b, -c);
    }
    int intersection(const line &l, point &P) const {
        double d = det(a, b, l.a, l.b), dx = det(c, b, l.c, l.b), dy = det(a, c, l.a, l.c);
        if(EQ(d, 0.0L)) {
            if(EQ(dx, 0.0L) && EQ(dy, 0.0L)) {
                P = any();
                return IntInf;
            }
            else
                return 0;
        }
        else {
            P = point(- dx / d, - dy / d);
            return 1;
        }
    }
};

class vertex {
public:
	double x, y, z;
	vertex() { x = y = z = 0; }
	vertex(double a, double b, double c) : x(a), y(b), z(c) {}
	vertex(vertex v1, vertex v2) : x(v2.x - v1.x), y(v2.y - v1.y), z(v2.z - v1.z) {}
	vertex operator+(const vertex &v) const { return vertex(x + v.x, y + v.y, z + v.z); }
	vertex operator+=(const vertex &v) { return vertex(x = x + v.x, y = y + v.y, z = z + v.z); }
	vertex operator-(const vertex &v) const { return vertex(x - v.x, y - v.y, z - v.z); }
	vertex operator-=(const vertex &v) { return vertex(x = x - v.x, y = y - v.y, z = z - v.z); }
	vertex operator*(double k) const { return vertex(x * k, y * k, z * k); }
	vertex operator*=(double k) { return vertex(x = x * k, y = y * k, z = z * k); }
	vertex operator-() const { return vertex(-x, -y, -z); }
	double len2() { return x * x + y * y + z * z; } 
	double len() { return sqrt(len2() ); }
};

class polygon { 
public:
	vertex a, b, c, norm;
	double D;
	DWORD color;
	polygon() {}
	polygon(const vertex &v1, const vertex &v2, const vertex &v3, DWORD _c) : a(v1), b(v2), c(v3), color(_c) {}
	void calcPlane() {
		vertex v(a, b), u(a, c);
		norm = vertex(v.y * u.z - v.z * u.y, v.z * u.x - v.x * u.z, v.x * u.y - v.y * u.x);
		D = -(norm.x * a.x + norm.y * a.y + norm.z * a.z);
	}
};

struct matrix4 {
	double data[4][4];
	matrix4() {}
	void clear() {
		int cnt = 0;
        memset(data, 0, sizeof(double) * 4 * 4);
	}
	void identity() { 
		clear();
		for(int i = 0; i<4; i++) {
			data[i][i] = 1.0; 
		}
	}
	void mult(const matrix4 &b, matrix4 &c) {
		for(int i = 0; i < 4; i++) {
			for(int j = 0; j < 4; j++) {
				c.data[i][j] = 0;
				for(int k = 0; k < 4; k++) {
					c.data[i][j] += data[i][k] * b.data[k][j];
				}
			}
		}
	}
	void trans(const vertex &v) {
		identity();
		data[3][0] = v.x;
		data[3][1] = v.y;
		data[3][2] = v.z;
	}
	void rotx(double a) {
		identity();
		data[1][1] = cos(a);
		data[1][2] = sin(a);
		data[2][1] = -sin(a);
		data[2][2] = cos(a);
	}
	void roty(double a) {
		identity();
		data[0][0] = cos(a);
		data[0][2] = -sin(a);
		data[2][0] = sin(a);
		data[2][2] = cos(a);
	}
	void rotz(double a) {
		identity();
		data[0][0] = cos(a);
		data[0][1] = sin(a);
		data[1][0] = -sin(a);
		data[1][1] = cos(a);
	}
};

#endif