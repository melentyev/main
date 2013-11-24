#include "stdafx.h"

class Canvas {
	HDC dc;
	BITMAPINFO bmi;
	HBITMAP bitmap;
public:
	int W, H;
	DWORD *bits;
	static const int basecolor = RGB(200, 200, 200);
	Canvas() { dc = 0; W = H = 0; bits = NULL; }
	Canvas(int w, int h) {
		W = w;
		H = h;
		dc = CreateCompatibleDC(0);
	
		ZeroMemory(&bmi, sizeof(BITMAPINFOHEADER));
		bmi.bmiHeader.biSize        = sizeof(BITMAPINFOHEADER);
		bmi.bmiHeader.biWidth       = W;
		bmi.bmiHeader.biHeight      = -H;
		bmi.bmiHeader.biPlanes      = 1;
		bmi.bmiHeader.biCompression = BI_RGB;
		bmi.bmiHeader.biBitCount    = 32;

		bitmap = CreateDIBSection(dc, &bmi, DIB_RGB_COLORS, (void**)&bits, 0, 0);
		SelectObject(dc, bitmap);
	}
	DWORD* operator[](int r) const {
		return bits + (W * r); 
	}
	DWORD& pixel(int r, int c) const { 
		return bits[r * W + c];
	}
	void setpixel(int r, int c, DWORD v) const {
		bits[r * W + c] = v;
	}
	HDC GetDC() const {
		return dc;
	}
	void render(HDC hdest) {
		BitBlt(hdest, 0, 0, W, H, dc, 0, 0, SRCCOPY);
	}
	void release() {
		if(dc) 
			DeleteDC(dc);
	}
};