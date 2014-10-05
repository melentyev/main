#include "canvas.h"
#include "geom.h"
#include <cmath>
#include <vector>

class pixel {
public:
	int x, y;
	pixel() { x = y = 0; }
	pixel(int _x, int _y) : x(_x), y(_y) {}
	pixel(double _x, double _y) : x((int)floor(_x)), y((int)floor(_y)){} 
};

class Renderer;

class camera {
public:
	friend class Renderer;
	matrix4 conv;
	void apply(vertex &v) {
		double xt, yt, zt;
		xt = v.x * conv.data[0][0] + v.y * conv.data[1][0] + v.z * conv.data[2][0] + 1 * conv.data[3][0];
		yt = v.x * conv.data[0][1] + v.y * conv.data[1][1] + v.z * conv.data[2][1] + 1 * conv.data[3][1];
		zt = v.x * conv.data[0][2] + v.y * conv.data[1][2] + v.z * conv.data[2][2] + 1 * conv.data[3][2];
		v.x = xt;
		v.y = yt;
		v.z = zt;
	}
	void apply(polygon &p) {
		apply(p.a);
		apply(p.b);
		apply(p.c);
	}
public:
	vertex dir, up, pos;
	camera() : dir(0.0, 0.0, 1.0) { conv.identity(); }
	void moveBy(const vertex &v) {
		matrix4 T, res;
		T.trans(-v);
		conv.mult(T, res);
		memcpy(conv.data, res.data, 16*sizeof(double) ); 
	}
	void rotateX(double a) {
		matrix4 T, res;
		T.rotx(-a);
		conv.mult(T, res);
		memcpy(conv.data, res.data, 16*sizeof(double) ); 
	}
	void rotateY(double a) {
		matrix4 T, res;
		T.roty(-a);
		conv.mult(T, res);
		memcpy(conv.data, res.data, 16*sizeof(double) ); 
	}
	void rotateZ(double a) {
		matrix4 T, res;
		T.rotz(-a);
		conv.mult(T, res);
		memcpy(conv.data, res.data, 16*sizeof(double) ); 
	}
};

const int Z_BUFFER = 0;
const int R_TRACE = 1;

class Renderer {
public:
	Canvas c;
	camera cam;
	DWORD *color_data;
	double *depth_data;
	double maxdepth, z0, left, bottom, right, top, frontW;
	point screencorner;
	int memsize;
	int type;
    bool drawFrames;
	std::vector<polygon*> data;
	void(*Debug)(Canvas &canv);
	Renderer() : Debug(NULL) { drawFrames = false, color_data = NULL; depth_data = NULL; }
	Renderer(int W, int H, int type) : c(W, H), Debug(NULL) { 
		memsize = W * H;
		color_data = new DWORD[memsize]; 
		depth_data = new double[memsize]; 
		maxdepth = 100.0;
		z0 = 1.0;
		left = -1.0;
		bottom = -1.0;
		right = 1.0;
		top = 1.0;
		drawFrames = false;
		frontW = right - left;
		clear();
	}
	void clear(DWORD background = Canvas::basecolor) {
		for(int i = 0; i < memsize; i++) {
			color_data[i] = background; 
			depth_data[i] = maxdepth;
		}
	}
	void DebugOutput(void(*fn)(Canvas &canv)) {
		Debug = fn;
	}
	void RayTracing() {
		int row, col;
		double Min;
		for(int i = 0; i < memsize; i++) {
			row = i % c.W;
			col = i / c.W;
			Min = maxdepth;
			point Pix = PixelToPoint(col + 0.5, row + 0.5);
			for(vector<polygon*>::iterator it = data.begin(); it != data.end(); it++) {
				
			}
	
		}
	}
	void renderScene(HDC hdest) {
		if(type == R_TRACE) {
			RayTracing();
		}
		for(int i = 0; i < memsize; i++)
			c.bits[i] = color_data[i];
		if(Debug != NULL) {
			Debug(c);
			Debug = NULL;
		}
		c.render(hdest);
		clear();
	}
	point rasterize(const vertex &v) {
		double t = z0 / v.z;
		return point(v.x * t, v.y * t);
	}
	pixel PointToPixel(const point &p) {
		return pixel((p.x - left) / frontW * c.W, ((-p.y) + top) / frontW * c.W);
	}
	point PixelToPoint(double x, double y) {
		return point((double)x / c.W * frontW + left, (double)-y / c.W * frontW + top);
	}
	/* p(t) == (x0*t; y0*t; z0*t);
	  n * p(t) + D = 0; 
	  A * x0*t + B * y0*t + C * z0*t + D == 0;
	  t*(A*x0 + B*y0 + C*z0) == -D;
	  t == - D / (A*x0 + B*y0 + C*z0) */
	vertex RayTrace(const point &Pix, polygon &p) {
		double t = -p.D / (p.norm.x * Pix.x + p.norm.y * Pix.y + p.norm.z * z0);
		return vertex(Pix.x * t, Pix.y * t, z0 * t);
	}
	
	void submitPolygon(polygon &p) {
		cam.apply(p);
		p.calcPlane();
		if(type == R_TRACE) {
			data.push_back(&p);
			return;
		}
		point p1 = rasterize(p.a), p2 = rasterize(p.b), p3 = rasterize(p.c);
		pixel s1 = PointToPixel(p1), s2 = PointToPixel(p2), s3 = PointToPixel(p3);
		double L = min(s1.x, min(s2.x, s3.x) ), B = min(s1.y, min(s2.y, s3.y) ),
			T = max(s1.y, max(s2.y, s3.y) ), R = max(s1.x, max(s2.x, s3.x) );
		double y0, xl, xr, y12l = min(p1.y, p2.y), y12r = max(p1.y, p2.y),
							y23l = min(p2.y, p3.y), y23r = max(p2.y, p3.y),
							y31l = min(p3.y, p1.y), y31r = max(p3.y, p1.y), x1, x2, x3, zl, zr, dz, z1;
		bool b1, b2, b3;
		double INF = 1e9;
		for (int row = 0; row < c.H; row++) {
            DWORD *colorPtr;
            double *depthPtr;
			y0 = PixelToPoint(0, row + 0.5).y;
			b1 = b2 = b3 = false;
			if(y0 >= y12l && y0 <= y12r) x1 = segment(p1, p2).crossYl(y0), b1 = true;
			if(y0 >= y23l && y0 <= y23r) x2 = segment(p2, p3).crossYl(y0), b2 = true;
			if(y0 >= y31l && y0 <= y31r) x3 = segment(p3, p1).crossYl(y0), b3 = true;
			if(!b1 && !b2 && !b3) continue;
			xl = min((b1 ? x1 : INF), min((b2 ? x2 : INF), (b3 ? x3 : INF)) );
			xr = max((b1 ? x1 : -INF), max((b2 ? x2 : -INF), (b3 ? x3 : -INF)) );
			zl = RayTrace(point(xl, y0), p).z;
			zr = RayTrace(point(xr, y0), p).z;
			int ixl = PointToPixel(point(xl, 0)).x, ixr = PointToPixel(point(xr, 0) ).x;
			//ixl = max(ixl, 0);
			//ixr = min(ixr, c.W - 1);
			dz = (zr - zl) / (ixr - ixl);
			z1 = zl;
            depthPtr = depth_data + (row * c.W + ixl);
            colorPtr = color_data + (row * c.W + ixl);

			for(int col = ixl; col <= ixr; col++) {
				if(z1 > z0) {
					if(col >= 0 && col < c.W && z1 < *depthPtr) {
						*depthPtr = z1;
						*colorPtr = p.color;
					}
				}
				z1 += dz;
                depthPtr++;
                colorPtr++;
			}
		}	
        if(drawFrames) {
            
        }
	}	
};

class AntiAliaser : public Renderer {
public:
	void(*AADebug)(Canvas &canv);
	Canvas aacanv, aliased;
	bool AAproc;
	AntiAliaser(int _W, int _H, bool flag, int type) : AAproc(flag), 
				Renderer(flag ? _W * 2 : _W, flag ? _H * 2 : _H, type), 
				aacanv(flag ? _W * 2 : _W, flag ? _H * 2 : _H), aliased(_W, _H) {} 
	DWORD midcolor(DWORD a, DWORD b, DWORD c, DWORD d) {
		DWORD R = a % 256 + b % 256 + c % 256 + d % 256;
		DWORD G = (a >> 8) % 256 + (b >> 8) % 256 + (c >> 8) % 256 + (d >> 8) % 256;
		DWORD B = (a >> 16) % 256 + (b >> 16) % 256 + (c >> 16) % 256 + (d >> 16) % 256;
		R /= 4;
		G /= 4;
		B /= 4;
		return RGB(R, G, B);
	}
	void AArenderScene(HDC hdest) {
		if(!AAproc) {
			renderScene(hdest);
			return;
		}
		AADebug = Debug; Debug = NULL;
		renderScene(aacanv.GetDC());
		for(int row = 0; row < aliased.H; row++) {
			for(int col = 0; col < aliased.W; col++) {
				//aliased.bits[row * aliased.W + col] = aacanv.pixel(row * 2, col * 2); 
				aliased.bits[row * aliased.W + col] = midcolor(aacanv.pixel(row * 2, col * 2), 
															   aacanv.pixel(row * 2 + 1, col * 2),
															   aacanv.pixel(row * 2, col * 2 + 1),
															   aacanv.pixel(row * 2 + 1, col * 2 + 1));
			}
		}
		if(AADebug != NULL) {
			AADebug(aliased);
			AADebug = NULL;
		}
		aliased.render(hdest);
	}
};

/*for(int row = max(0, B); row <= min(T, c.H - 1); row++) {
			for(int col = max(0, L); col <= min(R, c.W - 1); col++) {
				point Pix = PixelToPoint(col + 0.5, row + 0.5);
				cp1 = point(p1, p2) % point(p1, Pix);
				cp2 = point(p2, p3) % point(p2, Pix);
				cp3 = point(p3, p1) % point(p3, Pix);
				if(cp1 >= 0 && cp2 >= 0 && cp3 >= 0 || cp1 <= 0 && cp2 <= 0 && cp3 <= 0) {
					double z1 = RayTrace(Pix, p).z;
					if(z1 <= z0) continue;
					if(z1 < depth_data[row * c.W + col]) {
						depth_data[row * c.W + col] = z1;
						color_data[row * c.W + col] = p.color;
					}
				}
			}
		}*/
/*
void submitPolygonOld(polygon &p) {
		cam.apply(p);
		p.calcPlane();
		if(type == R_TRACE) {
			data.push_back(&p);
			return;
		}
		point p1 = rasterize(p.a), p2 = rasterize(p.b), p3 = rasterize(p.c);
		double cp1, cp2, cp3;
		pixel s1 = PointToPixel(p1), s2 = PointToPixel(p2), s3 = PointToPixel(p3);
		int L = min(s1.x, min(s2.x, s3.x) ), B = min(s1.y, min(s2.y, s3.y) ),
			T = max(s1.y, max(s2.y, s3.y) ), R = max(s1.x, max(s2.x, s3.x) );
		for(int row = max(0, B); row <= min(T, c.H - 1); row++) {
			for(int col = max(0, L); col <= min(R, c.W - 1); col++) {
				point Pix = PixelToPoint(col + 0.5, row + 0.5);
				cp1 = point(p1, p2) % point(p1, Pix);
				cp2 = point(p2, p3) % point(p2, Pix);
				cp3 = point(p3, p1) % point(p3, Pix);
				if(cp1 >= 0 && cp2 >= 0 && cp3 >= 0 || cp1 <= 0 && cp2 <= 0 && cp3 <= 0) {
					double z1 = RayTrace(Pix, p).z;
					if(z1 <= z0) continue;
					if(z1 < depth_data[row * c.W + col]) {
						depth_data[row * c.W + col] = z1;
						color_data[row * c.W + col] = p.color;
					}
				}
			}
		}
	}	*/