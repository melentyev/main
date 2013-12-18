TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += main.c \
    interpreter.c \
    parser.c \
    linker.c

HEADERS += \
    declarations.h

