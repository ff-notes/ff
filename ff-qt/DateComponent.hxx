#ifndef ff_qt_DateComponent_hxx
#define ff_qt_DateComponent_hxx


#include <QtWidgets/QDateEdit>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>


class DateComponent: public QHBoxLayout {
private:
    using super = QHBoxLayout;

    bool const isEditable;
    QString const title;

    QDateEdit * dateEdit;
    QLabel * const label;

public:
    DateComponent(
        QString const & label,
        QDate const & date,
        bool isEditable,
        std::function<void(QDate const &)> onDateChanged
    );

    void setDate(QDate date);
};


#endif // ff_qt_DateComponent_hxx
