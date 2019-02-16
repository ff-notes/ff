#ifndef ff_qt_Builder_hxx
#define ff_qt_Builder_hxx


#include <QtWidgets>


template <typename Wrapped>
class New;


template <>
class New<QDateEdit> {
private:
    QDateEdit * p;

public:
    New(QDate d): p(new QDateEdit(d)) {}
    operator QDateEdit * () { return p; }

    New & setCalendarPopup(bool a) {p->setCalendarPopup(a); return *this;}
};


template <>
class New<QMenu> {
private:
    QMenu * p;

public:
    operator QMenu * () { return p; }

    template <typename Func1>
    New & addAction(const QString & a, Func1 b) {
        p->addAction(a, b, c); return *this;
    }
};


template <>
class New<QTabWidget> {
private:
    QTabWidget * p;

public:
    operator QTabWidget * () { return p; }

    New & addTab(QWidget * a, const QString & b) {
        p->addTab(a, b); return *this;
    }
};


template <typename Wrapped>
class Make;


template <>
class Make<QFont> {
private:
    QFont p;

public:
    operator QFont & () { return p; }

    Make & setBold(bool a) { p.setBold(a); return *this; }
    Make & setUnderline(bool a) { p.setUnderline(a); return *this; }
};


template <>
class Make<QPalette> {
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
