#ifndef ff_qt_DateComponent_hxx
#define ff_qt_DateComponent_hxx


#include <QtWidgets>


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;
public:
    DateComponent(QString label, QDate date);
};


#endif // ff_qt_DateComponent_hxx
