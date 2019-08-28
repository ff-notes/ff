#ifndef ff_qt_util_hxx
#define ff_qt_util_hxx


#include <QtCore/QDate>

#include "FFI/Cxx.hxx"


inline QDate toQDate(Date const & d) {
    return QDate(d.year, d.month, d.day);
}


inline Date fromQDate(QDate const & d) {
    return Date{d.year(), d.month(), d.day()};
}


#endif // ff_qt_util_hxx
