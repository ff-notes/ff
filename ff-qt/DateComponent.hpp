#ifndef ff_qt_DateComponent_hpp
#define ff_qt_DateComponent_hpp


#include <QtWidgets>


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;
public:
    DateComponent(QString label, QDate date);
};


#endif // ff_qt_DateComponent_hpp
