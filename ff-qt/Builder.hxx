#ifndef ff_qt_Builder_hxx
#define ff_qt_Builder_hxx


#include <QtWidgets>


template <typename Wrapped>
struct NewBase {
    Wrapped * p;
    operator Wrapped * () { return p; }
};


template <typename Wrapped>
struct New;


template <>
struct New<QDateEdit>: NewBase<QDateEdit> {
    New(QDate d): NewBase{new QDateEdit(d)} {}
    New & setCalendarPopup(bool a) {p->setCalendarPopup(a); return *this;}
};


template <>
struct New<QHBoxLayout>: NewBase<QHBoxLayout> {
    New & addLayout(QLayout * a) { p->addLayout(a); return *this; }
    New & addWidget(QWidget * a) { p->addWidget(a); return *this; }
    New & addStretch() { p->addStretch(); return *this; }
};


template <>
struct New<QMenu>: NewBase<QMenu> {
    template <typename Func1>
    New & addAction(const QString & a, Func1 b) {
        p->addAction(a, b); return *this;
    }
};


template <>
struct New<QTabWidget>: NewBase<QTabWidget> {
    New & addTab(QWidget * a, const QString & b) {
        p->addTab(a, b); return *this;
    }
};


template <typename Wrapped>
struct Make;


template <>
struct Make<QFont> {
private:
    QFont p;

public:
    operator QFont & () { return p; }

    Make & setBold(bool a) { p.setBold(a); return *this; }
    Make & setUnderline(bool a) { p.setUnderline(a); return *this; }
};


template <>
struct Make<QPalette> {
private:
    QPalette p;

public:
    Make(const QPalette & p): p(p) {}
    operator QPalette & () { return p; }

    Make & setColor(QPalette::ColorRole a, QColor b) {
        p.setColor(a, b); return *this;
    }
};


#endif // ff_qt_Builder_hxx
