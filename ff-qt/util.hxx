#ifndef ff_qt_util_hxx
#define ff_qt_util_hxx


#include <QtCore>
#include "FFI/Cxx.hxx"


inline QDate qDate(Date d) {
    return QDate(d.year, d.month, d.day);
}


#endif // ff_qt_util_hxx
