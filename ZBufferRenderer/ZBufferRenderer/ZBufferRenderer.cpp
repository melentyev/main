#include "stdafx.h"
#include "ZBufferRenderer.h"
#include "Mmsystem.h"
#include <iostream>
#include "EasyBMP.h"
#pragma comment(lib, "winmm.lib")
#define MAX_LOADSTRING 100
using namespace std;

HINSTANCE hInst;		
TCHAR szTitle[MAX_LOADSTRING];	
TCHAR szWindowClass[MAX_LOADSTRING];		
bool keys[1 << 16];
int temp;
HWND hWnd;

ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);
	BMP Image;
	Image.ReadFromFile("example.bmp");

 	// TODO: Place code here.
	MSG msg;
	HACCEL hAccelTable;

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_ZBUFFERRENDERER, szWindowClass, MAX_LOADSTRING);
	MyRegisterClass(hInstance);

	// Perform application initialization:
	if (!InitInstance (hInstance, nCmdShow))
	{
		return FALSE;
	}

	hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_ZBUFFERRENDERER));

	// Main message loop:
	/*while (GetMessage(&msg, NULL, 0, 0))
	{
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	*/
	
	while(true) {
		if(PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE)) {
			if(!GetMessage(&msg, NULL, 0, 0) ) 
				break;
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		else {
			InvalidateRect(hWnd, NULL, false);
		}
	}
	
	return (int) msg.wParam;

}

ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;
	wcex.cbSize = sizeof(WNDCLASSEX);
	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ZBUFFERRENDERER));
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= MAKEINTRESOURCE(IDC_ZBUFFERRENDERER);
	wcex.lpszClassName	= szWindowClass;
	wcex.hIconSm		= LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

	return RegisterClassEx(&wcex);
}
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow) {
	hInst = hInstance; // Store instance handle in our global variable

	hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
		0, 0, 720, 720, NULL, NULL, hInstance, NULL);

	if (!hWnd)
	{
		return FALSE;
	}

	ShowWindow(hWnd, nCmdShow);
	UpdateWindow(hWnd);

	return TRUE;
}

AntiAliaser *renderer = NULL;

int dx = 0, dy = 0, mdx = 0;
double delta;

void MainLoop() {
	static int prev = timeGetTime();
	int cur = timeGetTime();
	mdx = max(mdx, abs(dx));
	//if(cur - prev < 20) return;
	delta = (cur - prev) / 1000.0;
	double mk = 1.3, mr = 0.37;
	if (keys[VK_UP]) renderer->cam.moveBy(vertex(0, 0, 1) * delta* mk);
	if (keys[VK_DOWN]) renderer->cam.moveBy(vertex(0, 0, -1) * delta * mk);
	renderer->cam.rotateY(mr * dx * delta);
	renderer->cam.rotateX(mr * dy * delta);
	//if(keys[VK_RIGHT]) renderer.cam.rotateY(mr * dx);
	if (keys[VK_LEFT]) renderer->cam.moveBy(vertex(-1, 0, 0) * delta * mk);
	if (keys[VK_RIGHT]) renderer->cam.moveBy(vertex(1, 0, 0) * delta * mk);
	if (keys[VK_PRIOR]) renderer->cam.moveBy(vertex(0, 1, 0) * delta * mk);
	if (keys[VK_NEXT]) renderer->cam.moveBy(vertex(0, -1, 0) * delta * mk);
    if (keys['S']) {
       temp++; temp--;
    }
	prev = cur;
	dx = 0;
	dy = 0;
}

void PrepareScene() {
	polygon cube[12];
	cube[0] = polygon(vertex(0.0, 0.0, 0.0), vertex(1.0, 0.0, 0.0), vertex(1.0, 0.0, 1.0), RGB(0, 0, 255) );
	cube[1] = polygon(vertex(0.0, 0.0, 0.0), vertex(1.0, 0.0, 1.0), vertex(0.0, 0.0, 1.0), RGB(0, 0, 255) );
	cube[2] = polygon(vertex(0.0, 0.0, 0.0), vertex(1.0, 0.0, 0.0), vertex(1.0, 1.0, 0.0), RGB(0, 255, 255) );
	cube[3] = polygon(vertex(0.0, 0.0, 0.0), vertex(1.0, 1.0, 0.0), vertex(0.0, 1.0, 0.0), RGB(0, 255, 255) );
	cube[4] = polygon(vertex(0.0, 0.0, 0.0), vertex(0.0, 0.0, 1.0), vertex(0.0, 1.0, 1.0), RGB(0, 255, 0) );
	cube[5] = polygon(vertex(0.0, 0.0, 0.0), vertex(0.0, 1.0, 1.0), vertex(0.0, 1.0, 0.0), RGB(0, 255, 0) );
	for(int i = 0; i<6; i++) {
		cube[i + 6] = cube[i];
		cube[i + 6].a.x = 1.0 - cube[i + 6].a.x; cube[i + 6].a.y = 1.0 - cube[i + 6].a.y; cube[i + 6].a.z = 1.0 - cube[i + 6].a.z;
		cube[i + 6].b.x = 1.0 - cube[i + 6].b.x; cube[i + 6].b.y = 1.0 - cube[i + 6].b.y; cube[i + 6].b.z = 1.0 - cube[i + 6].b.z;
		cube[i + 6].c.x = 1.0 - cube[i + 6].c.x; cube[i + 6].c.y = 1.0 - cube[i + 6].c.y; cube[i + 6].c.z = 1.0 - cube[i + 6].c.z;
	}
	cube[6].color = RGB(255, 0, 0); cube[7].color = RGB(255, 0, 0);
	cube[8].color = RGB(255, 0, 255); cube[9].color = RGB(255, 0, 255);
	cube[10].color = RGB(255, 255, 0); cube[11].color = RGB(255, 255, 0);
	double scale = 1.0, dx = 0, dy = 0, dz = 0;
	for(int i = 0; i<12; i++) cube[i].a *= scale, cube[i].b *= scale, cube[i].c *= scale;
	
	for(int i = 0; i<12; i++) cube[i].a.x += dx, cube[i].b.x += dx, cube[i].c.x += dx;
	for(int i = 0; i<12; i++) cube[i].a.y += dy, cube[i].b.y += dy, cube[i].c.y += dy;
	for(int i = 0; i<12; i++) cube[i].a.z += dz, cube[i].b.z += dz, cube[i].c.z += dz;
	
	/*polygon P(vertex(0.0, 0.0, 4.0), vertex(1.0, 1.0, 4.0), vertex(1.0, 0.0, 4.0), RGB(0, 0, 255) );
	polygon P2(vertex(-1.0, -1.0, 5.0), vertex(2.0, 2.0, 3.0), vertex(2.0, -1.0, 5.0), RGB(0, 255, 0) );
	MainRenderer.submitPolygon(P);
	MainRenderer.submitPolygon(P2);*/
	//MainRenderer.cam = camera();
	
	//MainRenderer.cam.rotateY(0.3);
	for(int i = 0; i<12; i++) 
		renderer->submitPolygon(cube[i]);
}

inline void DebugMsg(Canvas &canv) {
	static int count = 0, time = GetTickCount(), FPS = 0;
	static RECT txtrect;
	count++;
    int lines = 0;
	static char str[][256] = {"", "", "", "", "", "", "", "", "", "", ""};
    sprintf_s(str[lines++], "FPS:      %d", FPS);
	sprintf_s(str[lines++], "delta:    %.5lf", delta);
	sprintf_s(str[lines++], "dx:       %d", mdx);
	//sprintf_s(str[1], "x: %.3lf, y: %.3lf", pos.x, pos.y);
	//sprintf_s(str[2], "na: %.3lf, nb: %.3lf", nearcut.a, nearcut.b);

	txtrect.left = 0; txtrect.right = 1024; txtrect.top = 0;  txtrect.bottom = 1024;  

    for(int i = 0; i < lines; i++) {

	    txtrect.top += DrawTextA(canv.GetDC(), str[i], -1, &txtrect, DT_SINGLELINE | DT_NOCLIP  ) ;
    }
    if(GetTickCount() - time >= 500) { 		
		FPS = count * 2;
		count = 0;
		time = GetTickCount();
	}
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int wmId, wmEvent;
	PAINTSTRUCT ps;
	HDC hdc;
	static POINT mold, mnew;
	static bool bCamMove = false;
	switch (message) {
	case WM_CREATE:
		renderer = new AntiAliaser(640, 640, true, Z_BUFFER);
		mnew.x = renderer->c.W / 2;
		mnew.y = renderer->c.H / 2;
		mold = mnew;
		ClientToScreen(hWnd, &mnew);
		SetCursorPos(mnew.x, mnew.y);
		//SetCapture(hWnd);
		//ShowCursor(false);
		renderer->AAproc = true;
		renderer->drawFrames = true;
		renderer->cam.moveBy(vertex(0.0, 0.0, -4.0));

		break;
	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);
		// Parse the menu selections:
		switch (wmId)
		{
		case IDM_EXIT:
			DestroyWindow(hWnd);
			break;
		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;
	case WM_PAINT:
		hdc = BeginPaint(hWnd, &ps);
		MainLoop();
		PrepareScene();
		renderer->DebugOutput(DebugMsg);
		renderer->AArenderScene(hdc);
		EndPaint(hWnd, &ps);
		break;
	case WM_KEYDOWN: 
		if(wParam == VK_RETURN) 
			PostQuitMessage(0);
		keys[wParam] = true;
		break;
	case WM_KEYUP:
		keys[wParam] = false;
		break;
	case WM_LBUTTONDOWN:
        SetCapture(hWnd);
		mold.x = LOWORD(lParam);
		mold.y = HIWORD(lParam);
		bCamMove = true;
		break;
	case WM_LBUTTONUP:
        ReleaseCapture();
		bCamMove = false;
		break;
	case WM_MOUSEMOVE:
		if (bCamMove) {
			mnew.x = LOWORD(lParam);
			mnew.y = HIWORD(lParam);
			//ScreenToClient(hWnd, &mnew);
		
			if(mnew.x == mold.x && mnew.y == mold.y) break;
			dx = mnew.x - mold.x;
			dy = mnew.y - mold.y;
			mold = mnew;
		}
		//ClientToScreen(hWnd, &mnew);
		//SetCursorPos(mnew.x, mnew.y);
		//mold = mnew;
		
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}
