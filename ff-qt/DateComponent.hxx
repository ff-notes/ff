#ifndef ff_qt_DateComponent_hxx
#define ff_qt_DateComponent_hxx


#include <QtWidgets>


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;
    bool isEditable;
    QString title;
    QDateEdit * dateEdit;
    QLabel * label;
public:
    DateComponent(QString label, QDate date, bool isEditable);
    void setDate(QDate date);
};


#endif // ff_qt_DateComponent_hxx
