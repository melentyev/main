// Win32Gif2.cpp : Defines the entry point for the application.
//
#pragma comment(linker, "/STACK:16177216")
#include "stdafx.h"
#include "Mmsystem.h"
#include "Win32Gif2.h"
#pragma comment(lib, "winmm.lib")

#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR szWindowClass[MAX_LOADSTRING];			// the main window class name

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY _tWinMain(_In_ HINSTANCE hInstance,
                     _In_opt_ HINSTANCE hPrevInstance,
                     _In_ LPTSTR    lpCmdLine,
                     _In_ int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

 	// TODO: Place code here.
	MSG msg;
	HACCEL hAccelTable;

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_WIN32GIF2, szWindowClass, MAX_LOADSTRING);
	MyRegisterClass(hInstance);

	// Perform application initialization:
	if (!InitInstance (hInstance, nCmdShow))
	{
		return FALSE;
	}

	hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_WIN32GIF2));

	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0))
	{
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return (int) msg.wParam;
}



//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style			= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(IDI_WIN32GIF2));
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= MAKEINTRESOURCE(IDC_WIN32GIF2);
	wcex.lpszClassName	= szWindowClass;
	wcex.hIconSm		= LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

	return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
   HWND hWnd;

   hInst = hInstance; // Store instance handle in our global variable

   hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

   if (!hWnd)
   {
      return FALSE;
   }

   ShowWindow(hWnd, nCmdShow);
   UpdateWindow(hWnd);

   return TRUE;
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//




GifStreamString gif;

class MyCallbacks : public GifCallbackBase {
public:
    HDC hdc, buf;
    HBITMAP hbmMem;
    HANDLE hOld;
    MyCallbacks() : hbmMem(NULL) {}
    unsigned char* allocateMemory(unsigned int size_in_bytes) {
        return new unsigned char[size_in_bytes];
    }

    void onImageDecoded(GifDataBlockImage* img) {
        static DWORD nextImage = 0;
        if(hbmMem == NULL) {
            hbmMem = CreateCompatibleBitmap(hdc, gif.logicalScreen.width, gif.logicalScreen.height);
        }
        hOld = SelectObject(buf, hbmMem);
        unsigned char* data = img->decodedData;
        for(int i = 0; i < img->height; i++) {
            for(int j = 0; j < img->width; j++) {
                if(!img->hasAdditionalParams || !(img->additionalParams.transparentColorFlag) 
                    || img->additionalParams.transparentColorIndex != *data) {
                        SetPixel(buf, img->left + j, img->top + i, img->localTableFlag ? img->localTable.colors[*data] : gif.globalTable.colors[*data] );
                }
                data++;
            }
        }
        while(timeGetTime() < nextImage) {
            Sleep(1);
        }
        BitBlt(hdc, img->left, img->top, img->width, img->height, buf, img->left, img->top, SRCCOPY);
        SelectObject(buf, hOld);
        if (img->hasAdditionalParams) {
            nextImage = timeGetTime() + img->additionalParams.delayTime * 10;
        }
    }
} myCallbacks;

unsigned char* allocateMemoryForImage(int w, int h) {
    return new unsigned char [w * h]; 
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int wmId, wmEvent;
    static FILE *fin = NULL;
    PAINTSTRUCT ps;
    static char strings[][256] = {
        "C:\\MinGW\\file11.gif",
        "C:\\Users\\Admin\\Desktop\\������� �����\\giftest\\Ceric1.gif",
        "C:\\Users\\Admin\\Desktop\\������� �����\\giftest\\Dvdp3.gif",
        "C:\\Users\\Admin\\Desktop\\������� �����\\giftest\\Light_dispersion_conceptual.gif",
        "C:\\Users\\Admin\\Desktop\\������� �����\\giftest\\moem_shlyukhoy_pol.gif",
        "C:\\Users\\Admin\\Desktop\\������� �����\\giftest\\tumblr_mhpbw4VzEx1rldc56o1_500.gif",
    };
    static unsigned char *cont;
    static int filesize;
        
	switch (message)
	{
    case WM_CREATE:
        fopen_s(&fin, strings[1], "rb");
        cont = new unsigned char[6 * 1000 * 1000];
        filesize = fread(cont, 1, 6 * 1000 * 1000, fin);
        gif = GifStreamString(cont);
        break;
	case WM_COMMAND:
		wmId    = LOWORD(wParam);
		wmEvent = HIWORD(wParam);
		// Parse the menu selections:
		switch (wmId)
		{
		case IDM_ABOUT:
			DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
			break;
        case ID_FILE_AGAIN:
            UpdateWindow(hWnd);
            break;
		case IDM_EXIT:
			DestroyWindow(hWnd);
			break;
		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
		break;
	case WM_PAINT:
		myCallbacks.hdc = BeginPaint(hWnd, &ps);
        
        myCallbacks.buf = CreateCompatibleDC(myCallbacks.hdc);
        
        gif.processStream(&myCallbacks);
	
        
        DeleteObject(myCallbacks.hbmMem);
        DeleteDC    (myCallbacks.buf);

        EndPaint(hWnd, &ps);
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	UNREFERENCED_PARAMETER(lParam);
	switch (message)
	{
	case WM_INITDIALOG:
		return (INT_PTR)TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)FALSE;
}
